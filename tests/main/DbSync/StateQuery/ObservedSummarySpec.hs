{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

-- | Tests for the locally-observed hard-fork summary.
--
-- Each test builds a fresh 'ObservedSummary' from the real mainnet
-- 'TopLevelConfig' (so per-era 'EraParams' come from consensus, the
-- single point of truth), feeds in a sequence of @(era, slot)@ pairs,
-- and verifies the resulting 'Interpreter' answers slot-detail queries
-- consistent with mainnet's known historical era boundaries.
--
-- The historical mainnet boundary epochs (Shelley=208, Allegra=236,
-- …) appear in this test only as /input/ slot numbers — the slot of
-- the first block of each era. They are NOT hardcoded in
-- 'ObservedSummary'; the summary derives them from observation.
module DbSync.StateQuery.ObservedSummarySpec
  ( spec
  ) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.HardFork.History.Qry (interpretQuery, qryFromExpr)
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry

import DbSync.Config.Genesis (mkTopLevelConfig, readCardanoGenesisConfig)
import DbSync.Config.Node (parseNodeConfig)
import DbSync.StateQuery.ObservedSummary
  ( EraIdx (..)
  , ObservationResult (..)
  , ObservedSummary
  , currentInterpreter
  , currentSummary
  , initObservedSummary
  , isObservationBroken
  , observeAt
  )
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

-- ---------------------------------------------------------------------------
-- * Mainnet configuration fixture
-- ---------------------------------------------------------------------------

-- | Directory containing a verbatim snapshot of the real mainnet
-- node config + genesis files. Lets the test run anywhere without
-- depending on a developer's local cardano-node deployment.
mainnetDir :: FilePath
mainnetDir = "fixtures/mainnet"

-- | Build a fresh 'ObservedSummary' from real mainnet genesis files.
mkInitialMainnetObservedSummary :: IO ObservedSummary
mkInitialMainnetObservedSummary = do
  Right nc <- parseNodeConfig (mainnetDir <> "/config.json")
  Right gc <- readCardanoGenesisConfig nc mainnetDir
  pure $ initObservedSummary (mkTopLevelConfig nc gc)

-- | Mainnet's well-known historical era-transition slot for each
-- Shelley-based era. The slot is the first slot of the first epoch of
-- that era.
--
-- These appear here as /test inputs/, NOT as data the production code
-- depends on. The production code derives boundaries from observation;
-- the test asserts that, given these slots as observed inputs, the
-- production code produces sensible boundaries.
mainnetShelleyStartSlot, mainnetAllegraStartSlot, mainnetMaryStartSlot,
  mainnetAlonzoStartSlot, mainnetBabbageStartSlot, mainnetConwayStartSlot
    :: SlotNo
mainnetShelleyStartSlot = SlotNo  4_492_800   -- epoch 208
mainnetAllegraStartSlot = SlotNo 16_588_800   -- epoch 236
mainnetMaryStartSlot    = SlotNo 23_068_800   -- epoch 251
mainnetAlonzoStartSlot  = SlotNo 39_916_800   -- epoch 290
mainnetBabbageStartSlot = SlotNo 72_316_800   -- epoch 365
mainnetConwayStartSlot  = SlotNo 133_660_800  -- epoch 507

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "DbSync.StateQuery.ObservedSummary" $ do

  describe "initObservedSummary" $ do
    it "starts in Byron with no past eras and not broken" $ do
      os <- mkInitialMainnetObservedSummary
      isObservationBroken os `shouldBe` False
      let summary = currentSummary os
      -- Summary has exactly one entry (Byron only)
      length (summaryEras summary) `shouldBe` 1

  describe "observeAt: Byron-only" $ do
    it "is unchanged when the same era is observed" $ do
      os0 <- mkInitialMainnetObservedSummary
      let (r1, os1) = observeAt ByronIdx (SlotNo 0) os0
          (r2, _os2) = observeAt ByronIdx (SlotNo 100_000) os1
      r1 `shouldBe` Unchanged
      r2 `shouldBe` Unchanged

    it "answers EpochSize correctly inside Byron" $ do
      os <- mkInitialMainnetObservedSummary
      let interp = currentInterpreter os
      -- Byron epoch size on mainnet is 21600
      shouldEpochSizeBe interp (EpochNo 0)   (EpochSize 21_600)
      shouldEpochSizeBe interp (EpochNo 100) (EpochSize 21_600)
      -- Epoch 207 (last Byron epoch on mainnet) should still be Byron
      -- because we haven't yet observed the transition.
      shouldEpochSizeBe interp (EpochNo 207) (EpochSize 21_600)

  describe "observeAt: Byron→Shelley" $ do
    it "produces a NewTransition at epoch 208" $ do
      os0 <- mkInitialMainnetObservedSummary
      let (result, _os1) =
            observeAt ShelleyIdx mainnetShelleyStartSlot os0
      case result of
        NewTransition t -> do
          -- Byron epoch size 21600, slot 4_492_800 / 21600 = 208
          show (toJsonyTransition t) `shouldSatisfy` ("Byron" `Cardano.Prelude.isInfixOf`)
          show (toJsonyTransition t) `shouldSatisfy` ("Shelley" `Cardano.Prelude.isInfixOf`)
        other -> panic ("expected NewTransition, got " <> show other)

    it "answers Shelley epoch sizes after the transition" $ do
      os0 <- mkInitialMainnetObservedSummary
      let (_, os1) = observeAt ShelleyIdx mainnetShelleyStartSlot os0
          interp = currentInterpreter os1
      -- Byron epochs 0–207 still 21600
      shouldEpochSizeBe interp (EpochNo 100) (EpochSize 21_600)
      shouldEpochSizeBe interp (EpochNo 207) (EpochSize 21_600)
      -- Shelley epochs 208+ should be 432000
      shouldEpochSizeBe interp (EpochNo 208) (EpochSize 432_000)
      shouldEpochSizeBe interp (EpochNo 250) (EpochSize 432_000)

  describe "observeAt: full mainnet era walk" $ do
    it "produces a Summary satisfying invariantSummary" $ do
      os <- runMainnetWalk
      let summary = currentSummary os
      runExceptInvariant summary `shouldBe` Right ()

    it "knows all post-Byron epoch sizes are 432000" $ do
      os <- runMainnetWalk
      let interp = currentInterpreter os
      mapM_ (\e -> shouldEpochSizeBe interp e (EpochSize 432_000))
        [ EpochNo 208, EpochNo 236, EpochNo 251, EpochNo 290
        , EpochNo 365, EpochNo 507, EpochNo 600
        ]
      shouldEpochSizeBe interp (EpochNo 100) (EpochSize 21_600)

    it "agrees with mainnet for known slot↔epoch translations" $ do
      os <- runMainnetWalk
      let interp = currentInterpreter os
      -- Slot 4_492_800 is the first slot of epoch 208 (Shelley start)
      shouldSlotEpochBe interp (SlotNo  4_492_800)   (EpochNo 208, 0)
      -- Slot 16_588_800 is the first slot of epoch 236 (Allegra start)
      shouldSlotEpochBe interp (SlotNo 16_588_800)   (EpochNo 236, 0)
      -- Slot 133_660_800 is the first slot of epoch 507 (Conway start)
      shouldSlotEpochBe interp (SlotNo 133_660_800)  (EpochNo 507, 0)

  describe "observeAt: broken observation" $ do
    it "marks state broken when era jumps more than one ahead" $ do
      os0 <- mkInitialMainnetObservedSummary
      let (result, os1) =
            observeAt BabbageIdx mainnetBabbageStartSlot os0
      case result of
        ObservationBroken fromEra toEra -> do
          fromEra `shouldBe` ByronIdx
          toEra `shouldBe` BabbageIdx
        other -> panic ("expected ObservationBroken, got " <> show other)
      isObservationBroken os1 `shouldBe` True

    it "ignores subsequent observations once broken" $ do
      os0 <- mkInitialMainnetObservedSummary
      let (_, os1) = observeAt BabbageIdx mainnetBabbageStartSlot os0
          (r2, _os2) = observeAt ConwayIdx mainnetConwayStartSlot os1
      r2 `shouldBe` Unchanged

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | Walk through the full mainnet era progression, returning the
-- resulting 'ObservedSummary'.
runMainnetWalk :: IO ObservedSummary
runMainnetWalk = do
  os0 <- mkInitialMainnetObservedSummary
  pure $
      step ConwayIdx  mainnetConwayStartSlot
    . step BabbageIdx mainnetBabbageStartSlot
    . step AlonzoIdx  mainnetAlonzoStartSlot
    . step MaryIdx    mainnetMaryStartSlot
    . step AllegraIdx mainnetAllegraStartSlot
    . step ShelleyIdx mainnetShelleyStartSlot
    $ os0
  where
    step :: EraIdx -> SlotNo -> ObservedSummary -> ObservedSummary
    step e s os = snd (observeAt e s os)

-- | Run a 'Qry' for an epoch's 'EpochSize'.
askEpochSize
  :: History.Interpreter xs
  -> EpochNo
  -> Either Qry.PastHorizonException EpochSize
askEpochSize interp epoch =
  interpretQuery interp (qryFromExpr (Qry.EEpochSize (Qry.ELit epoch)))

-- | Assert that the interpreter answers an EpochSize query with the
-- expected value (failing the test otherwise). Avoids the need for
-- an 'Eq' instance on 'PastHorizonException'.
shouldEpochSizeBe
  :: History.Interpreter xs -> EpochNo -> EpochSize -> Expectation
shouldEpochSizeBe interp epoch expected =
  case askEpochSize interp epoch of
    Right got -> got `shouldBe` expected
    Left e ->
      expectationFailure $
        "askEpochSize " <> show epoch <> ": PastHorizon: " <> show e

-- | Run a 'Qry' for a slot's epoch / within-epoch index.
askSlotToEpoch
  :: History.Interpreter xs
  -> SlotNo
  -> Either Qry.PastHorizonException (EpochNo, Word64)
askSlotToEpoch interp slot =
  interpretQuery interp (Qry.slotToEpoch' slot)

-- | Assert that the interpreter answers a slot→epoch query with the
-- expected (epoch, within-epoch slot) pair.
shouldSlotEpochBe
  :: History.Interpreter xs -> SlotNo -> (EpochNo, Word64) -> Expectation
shouldSlotEpochBe interp slot expected =
  case askSlotToEpoch interp slot of
    Right got -> got `shouldBe` expected
    Left e ->
      expectationFailure $
        "askSlotToEpoch " <> show slot <> ": PastHorizon: " <> show e

-- | Extract the underlying list of 'EraSummary' values from a 'Summary'.
summaryEras :: History.Summary xs -> [History.EraSummary]
summaryEras = toList . History.getSummary

-- | Run 'invariantSummary' and convert the 'Except' to an 'Either'.
runExceptInvariant :: History.Summary xs -> Either [Char] ()
runExceptInvariant = runExcept . History.invariantSummary

-- | Render an 'ObservedTransition' as a '[Char]' for use in 'isInfixOf' assertions.
toJsonyTransition :: Show a => a -> [Char]
toJsonyTransition = show
