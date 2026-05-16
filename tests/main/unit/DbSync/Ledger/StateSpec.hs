{-# LANGUAGE NumericUnderscores #-}

-- | Unit tests for the pure helpers in 'DbSync.Ledger.State'.
--
-- The integration-level paths ('applyBlock', 'tickThenReapplyCheckHash',
-- 'mkHasLedgerEnv') need real ProtocolInfo + LSM session fixtures and
-- are deferred to Phase 6 alongside the boot flow's test setup. Here
-- we only cover the small pure decision functions that can be
-- exercised standalone.
module DbSync.Ledger.StateSpec
  ( spec
  ) where

import Cardano.Prelude

import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict

import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import qualified DbSync.Era.Shelley.Generic.EpochUpdate as Generic
import qualified DbSync.Era.Shelley.Generic.StakeDist as Generic
import DbSync.Ledger.State
  ( applyToEpochBlockNo
  , ledgerDbCheckpointBufferSize
  , pruneStrictSeq
  , shouldSnapshotAtEpoch
  )
import DbSync.Ledger.Types
  ( ApplyResult (..)
  , EpochBlockNo (..)
  , emptyDepositsMap
  )
import DbSync.StateQuery (SlotDetails (..))

import Test.Hspec (Spec, describe, it, shouldBe)

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "applyToEpochBlockNo" $ do
    it "byron-era state stays at ByronEpochBlockNo regardless of inputs" $ do
      applyToEpochBlockNo True False (EpochBlockNo 5)  `shouldBe` ByronEpochBlockNo
      applyToEpochBlockNo True True  (EpochBlockNo 5)  `shouldBe` ByronEpochBlockNo
      applyToEpochBlockNo True True  ByronEpochBlockNo `shouldBe` ByronEpochBlockNo

    it "new epoch resets the counter to 0 (Shelley+)" $
      applyToEpochBlockNo False True (EpochBlockNo 99) `shouldBe` EpochBlockNo 0

    it "non-epoch-boundary increments the counter" $
      applyToEpochBlockNo False False (EpochBlockNo 7) `shouldBe` EpochBlockNo 8

    it "first block after Byron seeds the counter at 0" $
      applyToEpochBlockNo False False ByronEpochBlockNo `shouldBe` EpochBlockNo 0

  describe "shouldSnapshotAtEpoch" $ do
    let mkResult mEpoch =
          ApplyResult
            { apPrices          = Strict.Nothing
            , apGovExpiresAfter = Strict.Nothing
            , apPoolsRegistered = Set.empty
            , apNewEpoch        = mEpoch
            , apDeposits        = Strict.Nothing
            , apSlotDetails     = dummySlotDetails
            , apStakeSlice      = Generic.NoSlices
            , apEvents          = []
            , apGovActionState  = Nothing
            , apDepositsMap     = emptyDepositsMap
            }
        emptyEpochUpdate =
          Generic.EpochUpdate
            { Generic.euProtoParams = Strict.Nothing
            , Generic.euNonce       = Ledger.NeutralNonce
            }
        newEpoch n =
          Generic.NewEpoch
            { Generic.neEpoch       = EpochNo n
            , Generic.neIsEBB       = False
            , Generic.neAdaPots     = Strict.Nothing
            , Generic.neEpochUpdate = emptyEpochUpdate
            , Generic.neDRepState   = Strict.Nothing
            , Generic.neEnacted     = Strict.Nothing
            , Generic.nePoolDistr   = Strict.Nothing
            }

    it "returns False when not on an epoch boundary" $
      shouldSnapshotAtEpoch (mkResult Strict.Nothing) True True 580 `shouldBe` False

    it "returns False at epoch 0 (we never snapshot the boot epoch)" $
      shouldSnapshotAtEpoch
        (mkResult (Strict.Just (newEpoch 0))) True True 580
        `shouldBe` False

    it "snapshots every epoch when consistent + near tip" $
      shouldSnapshotAtEpoch
        (mkResult (Strict.Just (newEpoch 7))) True True 580
        `shouldBe` True

    it "snapshots every 10 epochs when lagging (not near tip)" $ do
      shouldSnapshotAtEpoch
        (mkResult (Strict.Just (newEpoch 7))) True False 580
        `shouldBe` False
      shouldSnapshotAtEpoch
        (mkResult (Strict.Just (newEpoch 10))) True False 580
        `shouldBe` True
      shouldSnapshotAtEpoch
        (mkResult (Strict.Just (newEpoch 100))) True False 580
        `shouldBe` True

    it "snapshots every epoch past the near-tip-epoch threshold even when lagging" $ do
      shouldSnapshotAtEpoch
        (mkResult (Strict.Just (newEpoch 581))) True False 580
        `shouldBe` True
      shouldSnapshotAtEpoch
        (mkResult (Strict.Just (newEpoch 580))) True False 580
        `shouldBe` True
      shouldSnapshotAtEpoch
        (mkResult (Strict.Just (newEpoch 579))) True False 580
        `shouldBe` False

    it "ignores the near-tip flag if we're not consistent with the chain tip" $
      shouldSnapshotAtEpoch
        (mkResult (Strict.Just (newEpoch 7))) False True 580
        `shouldBe` False

  -- Exercise the underlying polymorphic spine logic on plain Ints.
  -- 'pruneLedgerDb' is a one-line wrapper around 'pruneStrictSeq',
  -- and constructing real 'DbSyncStateRef' values just to test
  -- sequence slicing would require a full LSM session (Phase 6
  -- fixture territory).
  describe "pruneStrictSeq" $ do
    let seqOf n = Seq.fromList [1 .. n :: Int]
        keptLen n = Seq.length (fst (pruneStrictSeq 100 (seqOf n)))
        dropped n = snd (pruneStrictSeq 100 (seqOf n))

    it "keeps every entry when the buffer is at or below the cap" $ do
      keptLen 0   `shouldBe` 0
      keptLen 50  `shouldBe` 50
      keptLen 100 `shouldBe` 100

    it "reports zero dropped entries when no pruning was needed" $ do
      dropped 50  `shouldBe` []
      dropped 100 `shouldBe` []

    it "keeps the @k@ newest entries when the buffer exceeds the cap" $ do
      keptLen 150 `shouldBe` 100
      -- Sanity: the kept prefix is the head of the input.
      Seq.fromList [1 .. 100 :: Int]
        `shouldBe` fst (pruneStrictSeq 100 (seqOf 150))

    it "reports the dropped tail so callers can close their handles" $
      dropped 150 `shouldBe` [101 .. 150]

    it "uses 100 as the production cap (matches upstream's k-fragment heuristic)" $
      ledgerDbCheckpointBufferSize `shouldBe` 100

-- ---------------------------------------------------------------------------
-- Helpers

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
