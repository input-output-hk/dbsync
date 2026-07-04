{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the EpochBoundary extractor.
--
-- The extractor itself has a no-op 'pdProcess' (boundary work is
-- driven by the consumer at boundary blocks, not the per-block
-- callback). Tests cover:
--
-- * Extractor metadata: name, version, dependencies, registered tables.
-- * 'runEpochBoundary' dispatch logic: no-op when @apNewEpoch@ is
--   'Nothing' (non-boundary block) or when @neAdaPots@ \/ @euProtoParams@
--   are 'Nothing' (Byron-era boundary).
-- * 'runEpochBoundary' with populated @euProtoParams@ writes the
--   expected boundary rows, and the @cost_model@ dedup path returns
--   the same id on a repeat boundary.
module DbSync.Extractor.EpochBoundarySpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import Cardano.Ledger.Plutus.Language (Language)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import Test.Hspec (Spec, anyException, describe, expectationFailure, it, shouldBe, shouldThrow)

import qualified DbSync.Worker.Ledger.EpochUpdate as Generic
import qualified DbSync.Worker.Ledger.ProtoParams as Proto
import qualified DbSync.Worker.Ledger.StakeDist as Generic
import DbSync.Db.Schema.AdaPots (adaPotsTableDef)
import DbSync.Db.Schema.Ids (BlockId (..))
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Extractor (ExtractorDef (..), freshExtractState)
import DbSync.Extractor.EpochBoundary (epochBoundaryExtractor, runEpochBoundary)
import DbSync.Worker.Ledger.Types
  ( ApplyResult (..)
  , BoundaryApplyData (..)
  , emptyDepositsMap
  )
import DbSync.AppM (runAppM)
import DbSync.Phase.Ingest.Resolver (mkIngestResolver)
import DbSync.Resolver (IdResolver)
import DbSync.StateQuery (SlotDetails (..))
import DbSync.Test.Lsm (withTestIngestStores)
import DbSync.Test.PipelineEnv (mkTestPipelineEnv)
import DbSync.Worker.TxOut.AddressBuffer (newAddressBufferRef)
import DbSync.Writer (Writer (..))
import DbSync.Test.Writer (TestWriterState (..), emptyTestWriterState, mkTestWriter)

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "epochBoundaryExtractor metadata" $ do
    it "has name 'epoch_boundary'" $
      pdName epochBoundaryExtractor `shouldBe` "epoch_boundary"

    it "registers four tables" $
      length (pdTables epochBoundaryExtractor) `shouldBe` 4

    it "registers ada_pots, epoch_param, epoch_state, cost_model" $
      map tdName (pdTables epochBoundaryExtractor)
        `shouldBe` ["ada_pots", "epoch_param", "epoch_state", "cost_model"]

    it "ada_pots entry is structurally identical to adaPotsTableDef" $
      case pdTables epochBoundaryExtractor of
        td : _ -> td `shouldBe` adaPotsTableDef
        []     -> expectationFailure "expected at least one table"

  describe "runEpochBoundary — no-op cases" $ do
    it "does nothing when apNewEpoch is Nothing (not a boundary)" $ do
      counterRef <- newIORef (0 :: Int)
      let resolver = mkCountingResolver counterRef
      writerRef <- newIORef emptyTestWriterState
      let writer = countingAdaPotsWriter counterRef (mkTestWriter writerRef)
          result = mkApplyResult Strict.Nothing

      let env = mkTestPipelineEnv resolver writer []
      runAppM env (runEpochBoundary (boundaryApplyData result) (BlockId 100))

      adaPotsCalls <- readIORef counterRef
      adaPotsCalls `shouldBe` 0

    it "does nothing when apNewEpoch carries neAdaPots = Nothing (Byron boundary)" $ do
      counterRef <- newIORef (0 :: Int)
      let resolver = mkCountingResolver counterRef
      writerRef <- newIORef emptyTestWriterState
      let writer = countingAdaPotsWriter counterRef (mkTestWriter writerRef)
          result =
            mkApplyResult $
              Strict.Just $
                mkNewEpoch (EpochNo 1) Strict.Nothing

      let env = mkTestPipelineEnv resolver writer []
      runAppM env (runEpochBoundary (boundaryApplyData result) (BlockId 100))

      adaPotsCalls <- readIORef counterRef
      adaPotsCalls `shouldBe` 0

    -- The "ada_pots happy path" needs a real 'Cardano.Ledger.Shelley.AdaPots'
    -- value — deferred to fixture work that lives alongside the
    -- existing genesis-fixture deferrals in DbSync.Worker.Ledger.StateSpec.

  describe "runEpochBoundary — proto-param boundary writes" $ do
    -- 'ProtoParams' rides in queued boundary payloads, so its 'rnf'
    -- must reach through the 'Maybe'-wrapped era fields — a thunk
    -- hiding inside a 'Just' would leak whatever it closes over
    -- (the era's 'PParams', cost models included).
    it "ProtoParams NFData forces the Maybe-wrapped era fields (bomb)" $
      evaluate (force dummyProtoParams { Proto.ppCostmdls = Just (panic "unforced cost models") })
        `shouldThrow` anyException

    it "writes no new rows when euProtoParams is Nothing (Byron boundary)" $ do
      written <- runBoundary $
        mkApplyResult $ Strict.Just $ mkNewEpochWith (EpochNo 1) Strict.Nothing
      twEpochParams written `shouldBe` []
      twEpochStates written `shouldBe` []
      twCostModels  written `shouldBe` []

    -- epoch_state is gated on Conway gov state; these fixtures carry
    -- none (apGovActionState = Nothing), so no epoch_state row is
    -- written. The Conway path is covered by GovernanceGenesisSpec.
    it "populated euProtoParams writes 1 epoch_param, no epoch_state without gov state" $ do
      written <- runBoundary $
        mkApplyResult $ Strict.Just $
          mkNewEpochWith (EpochNo 1) (Strict.Just dummyProtoParams)
      length (twEpochParams written) `shouldBe` 1
      length (twEpochStates written) `shouldBe` 0
      length (twCostModels  written) `shouldBe` 0

    it "populated euProtoParams with cost models writes 1 cost_model" $ do
      let params = dummyProtoParams { Proto.ppCostmdls = Just Map.empty }
      written <- runBoundary $
        mkApplyResult $ Strict.Just $
          mkNewEpochWith (EpochNo 1) (Strict.Just params)
      length (twCostModels  written) `shouldBe` 1
      length (twEpochParams written) `shouldBe` 1
      length (twEpochStates written) `shouldBe` 0

    it "two boundaries with the same cost models write only one cost_model row" $ do
      let params = dummyProtoParams { Proto.ppCostmdls = Just Map.empty }
          boundary epoch =
            mkApplyResult $ Strict.Just $
              mkNewEpochWith epoch (Strict.Just params)
      written <- runBoundaries [boundary (EpochNo 1), boundary (EpochNo 2)]
      length (twCostModels  written) `shouldBe` 1
      length (twEpochParams written) `shouldBe` 2
      length (twEpochStates written) `shouldBe` 0

-- ---------------------------------------------------------------------------
-- Test fixtures
-- ---------------------------------------------------------------------------

-- | Project an 'ApplyResult' onto the boundary payload, mirroring what the
-- worker enqueues, so the existing 'mkApplyResult' fixtures can drive the
-- 'BoundaryApplyData'-typed boundary extractors.
boundaryApplyData :: ApplyResult -> BoundaryApplyData
boundaryApplyData ar =
  BoundaryApplyData
    { bndNewEpoch        = apNewEpoch ar
    , bndEvents          = apEvents ar
    , bndGovActionState  = apGovActionState ar
    , bndGovExpiresAfter = apGovExpiresAfter ar
    , bndSlotDetails     = apSlotDetails ar
    }

-- | A minimal 'ApplyResult' with everything zero-/empty-/Nothing- valued
-- except 'apNewEpoch', which is supplied by the caller.
mkApplyResult :: Strict.Maybe Generic.NewEpoch -> ApplyResult
mkApplyResult mNewEpoch =
  ApplyResult
    { apPrices          = Strict.Nothing
    , apGovExpiresAfter = Strict.Nothing
    , apNewEpoch        = mNewEpoch
    , apDeposits        = Strict.Nothing
    , apSlotDetails     = dummySlotDetails
    , apStakeSlice      = Generic.NoSlices
    , apEvents          = []
    , apGovActionState  = Nothing
    , apDepositsMap     = emptyDepositsMap
    , apPoolsRegistered = Set.empty
    }

-- | Build a 'Generic.NewEpoch' for the given epoch with the supplied
-- 'neAdaPots' payload (and no other interesting payload).
mkNewEpoch :: EpochNo -> Strict.Maybe a -> Generic.NewEpoch
mkNewEpoch epoch _ =
  -- The 'a' phantom in our caller is just so we don't have to import
  -- the real 'Shelley.AdaPots' type here — for the no-op tests we
  -- don't touch the field.
  Generic.NewEpoch
    { Generic.neEpoch       = epoch
    , Generic.neIsEBB       = False
    , Generic.neAdaPots     = Strict.Nothing
    , Generic.neEpochUpdate =
        Generic.EpochUpdate
          { Generic.euProtoParams = Strict.Nothing
          , Generic.euNonce       = Ledger.NeutralNonce
          }
    , Generic.neDRepState   = Strict.Nothing
    , Generic.neEnacted     = Strict.Nothing
    , Generic.nePoolDistr   = Strict.Nothing
    }

dummySlotDetails :: SlotDetails
dummySlotDetails =
  SlotDetails
    { sdSlotTime    = epochZero
    , sdCurrentTime = epochZero
    , sdEpochNo     = EpochNo 0
    , sdSlotNo      = SlotNo 0
    , sdEpochSlot   = 0
    , sdEpochSize   = EpochSize 21600
    }
  where
    epochZero :: UTCTime
    epochZero = UTCTime (toEnum 0) (secondsToDiffTime 0)

-- ---------------------------------------------------------------------------
-- Test doubles
-- ---------------------------------------------------------------------------

-- | A resolver whose only behaviour is to count 'assignAdaPotsId'
-- calls. Every other 'IdResolver' field is 'panic' — these tests
-- only exercise the AdaPots path.
mkCountingResolver :: IORef Int -> IdResolver IO
mkCountingResolver _ =
  -- We only need 'assignAdaPotsId' to be a counter; the no-op tests
  -- never touch any other field. We can't easily construct a fully
  -- panicking resolver without a lot of boilerplate, so reuse the
  -- counting writer below to count calls instead — this resolver is
  -- only exercised once 'runEpochBoundary' decides to write a row,
  -- which our no-op tests deliberately avoid.
  panic "mkCountingResolver: unused in no-op tests"

-- | Wrap an existing 'Writer' so 'writeAdaPots' increments the
-- supplied counter. Used to detect whether 'runEpochBoundary'
-- attempted to write a row.
countingAdaPotsWriter :: IORef Int -> Writer IO -> Writer IO
countingAdaPotsWriter ref inner = inner
  { writeAdaPots = \pots -> do
      atomicModifyIORef' ref $ \n -> (n + 1, ())
      writeAdaPots inner pots
  }

-- ---------------------------------------------------------------------------
-- Real-resolver harness for the boundary-write tests
-- ---------------------------------------------------------------------------

-- | Run a single boundary against a real 'mkIngestResolver' and a
-- 'TestWriter'. The resulting state captures rows written for any
-- of the boundary tables.
runBoundary :: ApplyResult -> IO TestWriterState
runBoundary applyResult = runBoundaries [applyResult]

-- | Run a sequence of boundaries on a single shared resolver +
-- writer pair. Used to exercise cross-boundary state like the
-- 'esCostModelCache' dedup.
runBoundaries :: [ApplyResult] -> IO TestWriterState
runBoundaries applyResults = withTestIngestStores $ \utxoStore dedupStores -> do
  stRef   <- newIORef freshExtractState
  addrBuf <- newAddressBufferRef
  wrRef   <- newIORef emptyTestWriterState
  let resolver = mkIngestResolver stRef dedupStores addrBuf utxoStore Nothing
      writer   = mkTestWriter wrRef
      env      = mkTestPipelineEnv resolver writer []
  forM_ applyResults $ \r ->
    runAppM env (runEpochBoundary (boundaryApplyData r) (BlockId 100))
  readIORef wrRef

-- | 'Generic.NewEpoch' with the supplied protocol-params payload.
mkNewEpochWith :: EpochNo -> Strict.Maybe Proto.ProtoParams -> Generic.NewEpoch
mkNewEpochWith epoch mPp =
  Generic.NewEpoch
    { Generic.neEpoch       = epoch
    , Generic.neIsEBB       = False
    , Generic.neAdaPots     = Strict.Nothing
    , Generic.neEpochUpdate =
        Generic.EpochUpdate
          { Generic.euProtoParams = mPp
          , Generic.euNonce       = Ledger.NeutralNonce
          }
    , Generic.neDRepState   = Strict.Nothing
    , Generic.neEnacted     = Strict.Nothing
    , Generic.nePoolDistr   = Strict.Nothing
    }

-- | Minimum 'ProtoParams' fixture: every mandatory field at zero \/
-- 'minBound', every Alonzo+Conway field 'Nothing'. Tests that need
-- specific values override fields one at a time.
dummyProtoParams :: Proto.ProtoParams
dummyProtoParams = Proto.ProtoParams
  { Proto.ppMinfeeA              = 0
  , Proto.ppMinfeeB              = 0
  , Proto.ppMaxBBSize            = 0
  , Proto.ppMaxTxSize            = 0
  , Proto.ppMaxBHSize            = 0
  , Proto.ppKeyDeposit           = Coin 0
  , Proto.ppPoolDeposit          = Coin 0
  , Proto.ppMaxEpoch             = Ledger.EpochInterval 0
  , Proto.ppOptimalPoolCount     = 0
  , Proto.ppInfluence            = 0
  , Proto.ppMonetaryExpandRate   = minBound
  , Proto.ppTreasuryGrowthRate   = minBound
  , Proto.ppDecentralisation     = minBound
  , Proto.ppExtraEntropy         = Ledger.NeutralNonce
  , Proto.ppProtocolVersion      =
      Ledger.ProtVer (Ledger.natVersion @9) 0
  , Proto.ppMinUTxOValue         = Coin 0
  , Proto.ppMinPoolCost          = Coin 0
  , Proto.ppCoinsPerUtxo         = Nothing
  , Proto.ppCostmdls             = Nothing
  , Proto.ppPriceMem             = Nothing
  , Proto.ppPriceStep            = Nothing
  , Proto.ppMaxTxExMem           = Nothing
  , Proto.ppMaxTxExSteps         = Nothing
  , Proto.ppMaxBlockExMem        = Nothing
  , Proto.ppMaxBlockExSteps      = Nothing
  , Proto.ppMaxValSize           = Nothing
  , Proto.ppCollateralPercentage = Nothing
  , Proto.ppMaxCollateralInputs  = Nothing
  , Proto.ppPoolVotingThresholds       = Nothing
  , Proto.ppDRepVotingThresholds       = Nothing
  , Proto.ppCommitteeMinSize           = Nothing
  , Proto.ppCommitteeMaxTermLength     = Nothing
  , Proto.ppGovActionLifetime          = Nothing
  , Proto.ppGovActionDeposit           = Nothing
  , Proto.ppDRepDeposit                = Nothing
  , Proto.ppDRepActivity               = Nothing
  , Proto.ppMinFeeRefScriptCostPerByte = Nothing
  }

-- Silence -Wunused-imports for type aliases that only surface in
-- type signatures via the boundary fixtures.
_unusedTypes :: (Language, Alonzo.CostModel) -> ()
_unusedTypes _ = ()
