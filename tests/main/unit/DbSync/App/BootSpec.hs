{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for 'DbSync.App.Boot.decideBoot'.
module DbSync.App.BootSpec (spec) where

import Cardano.Prelude

import qualified Data.ByteString as BS

import Ouroboros.Consensus.Storage.LedgerDB.Snapshots (DiskSnapshot (..))

import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

import DbSync.Db.Schema.SyncState (SyncStateRow (..))
import DbSync.App.Boot
  ( BootDecision (..)
  , BootError (..)
  , FollowRestartContext (..)
  , ResumeContext (..)
  , ResumeIntersection (..)
  , decideBoot
  , mkCardanoPoint
  )

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | A row as it appears immediately after 'seedSyncState' — every
-- counter at 1 and no committed progress.
seededRow :: Bool -> SyncStateRow
seededRow ledgerEnabled = SyncStateRow
  { ssrLastCommittedSlot             = Nothing
  , ssrLastCommittedBlockNo          = Nothing
  , ssrLastCommittedBlockHash        = Nothing
  , ssrLastSnapshotSlot              = Nothing
  , ssrBlockIdCounter                = 1
  , ssrTxIdCounter                   = 1
  , ssrTxOutIdCounter                = 1
  , ssrTxInIdCounter                 = 1
  , ssrCollateralTxInIdCounter       = 1
  , ssrReferenceTxInIdCounter        = 1
  , ssrTxMetadataIdCounter           = 1
  , ssrMaTxMintIdCounter             = 1
  , ssrMaTxOutIdCounter              = 1
  , ssrSlotLeaderIdCounter           = 1
  , ssrAddressIdCounter              = 1
  , ssrStakeAddressIdCounter         = 1
  , ssrPoolHashIdCounter             = 1
  , ssrMultiAssetIdCounter           = 1
  , ssrScriptIdCounter               = 1
  , ssrStakeRegistrationIdCounter    = 1
  , ssrStakeDeregistrationIdCounter  = 1
  , ssrDelegationIdCounter           = 1
  , ssrWithdrawalIdCounter           = 1
  , ssrPoolUpdateIdCounter           = 1
  , ssrPoolMetadataRefIdCounter      = 1
  , ssrPoolOwnerIdCounter            = 1
  , ssrPoolRetireIdCounter           = 1
  , ssrPoolRelayIdCounter            = 1
  , ssrTxCborIdCounter               = 1
  , ssrEpochSyncStatsIdCounter       = 1
  , ssrAdaPotsIdCounter              = 1
  , ssrCollateralTxOutIdCounter              = 1
  , ssrSchemaVersionApplied          = 1
  , ssrLedgerEnabled                 = ledgerEnabled
  , ssrSyncComplete                  = False
  , ssrPendingRollbackSlot           = Nothing
  }

-- | A row with realistic committed progress.
committedRow :: Bool -> SyncStateRow
committedRow ledgerEnabled = (seededRow ledgerEnabled)
  { ssrLastCommittedSlot      = Just 12_000_000
  , ssrLastCommittedBlockNo   = Just 4_200_000
  , ssrLastCommittedBlockHash = Just (BS.replicate 32 0xab)
  , ssrLastSnapshotSlot       = Just 11_900_000
  , ssrBlockIdCounter         = 4_200_001
  , ssrTxIdCounter            = 28_000_000
  }

snapshotAt :: Word64 -> DiskSnapshot
snapshotAt slot = DiskSnapshot { dsNumber = slot, dsSuffix = Nothing }

-- | Run 'decideBoot', expect 'BootResume', and pass the
-- 'ResumeContext' to a continuation. Wraps the boilerplate.
withResume
  :: HasCallStack
  => Maybe SyncStateRow
  -> [DiskSnapshot]
  -> Bool
  -> (ResumeContext -> IO ())
  -> IO ()
withResume mRow snaps ledgerEnabled f =
  case decideBoot mRow snaps ledgerEnabled of
    Right (BootResume rc) -> f rc
    other                 -> expectationFailure $ "unexpected: " <> show other

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "DbSync.App.Boot.decideBoot" $ do

  describe "no sync-state row" $ do
    it "is BootSyncStateMissing when ledger disabled" $
      decideBoot Nothing [] False `shouldBe` Left BootSyncStateMissing

    it "is BootSyncStateMissing when ledger enabled with no snapshots" $
      decideBoot Nothing [] True `shouldBe` Left BootSyncStateMissing

    it "is BootSnapshotsWithoutPgState when ledger enabled with snapshots present" $
      decideBoot Nothing [snapshotAt 1000] True
        `shouldBe` Left BootSnapshotsWithoutPgState

  describe "ledger.enabled mismatch" $ do
    it "row=False, config=True is rejected" $
      decideBoot (Just (seededRow False)) [] True
        `shouldBe` Left (BootLedgerEnabledMismatch False True)

    it "row=True, config=False is rejected" $
      decideBoot (Just (committedRow True)) [snapshotAt 1] False
        `shouldBe` Left (BootLedgerEnabledMismatch True False)

  describe "freshly-seeded but uncommitted row" $ do
    it "is BootFresh (ledger disabled)" $
      decideBoot (Just (seededRow False)) [] False
        `shouldBe` Right BootFresh

    it "is BootFresh (ledger enabled, no snapshots)" $
      decideBoot (Just (seededRow True)) [] True
        `shouldBe` Right BootFresh

    it "is BootSnapshotsWithoutPgState (ledger enabled, stale snapshots present)" $
      decideBoot (Just (seededRow True)) [snapshotAt 11_500_000] True
        `shouldBe` Left BootSnapshotsWithoutPgState

    it "is BootFresh (ledger disabled, ignores any stray on-disk snapshots)" $
      decideBoot (Just (seededRow False)) [snapshotAt 11_500_000] False
        `shouldBe` Right BootFresh

  describe "sync_complete = True (ledger disabled)" $ do
    it "is BootFollowRestart with empty candidate list" $ do
      let row = (committedRow False) { ssrSyncComplete = True }
      case decideBoot (Just row) [] False of
        Right (BootFollowRestart frc) -> do
          frcSyncState          frc `shouldBe` row
          frcCandidateSnapshots frc `shouldBe` []
        other -> expectationFailure $ "unexpected: " <> show other

  describe "sync_complete = True (ledger enabled)" $ do
    let completedRow = (committedRow True) { ssrSyncComplete = True }

    it "carries every in-range candidate snapshot, newest-first" $ do
      let snaps =
            [ snapshotAt 13_000_000   -- newest, past last_committed_slot
            , snapshotAt 11_900_000   -- in-range
            , snapshotAt 11_500_000   -- in-range
            , snapshotAt 11_000_000   -- in-range
            ]
      case decideBoot (Just completedRow) snaps True of
        Right (BootFollowRestart frc) -> do
          frcSyncState frc `shouldBe` completedRow
          frcCandidateSnapshots frc
            `shouldBe` [ snapshotAt 11_900_000
                       , snapshotAt 11_500_000
                       , snapshotAt 11_000_000
                       ]
        other -> expectationFailure $ "unexpected: " <> show other

    it "fails when there are no snapshots on disk" $
      decideBoot (Just completedRow) [] True
        `shouldBe` Left BootResumeStateMissing

    it "fails when every snapshot is past last_committed_slot" $
      decideBoot (Just completedRow) [snapshotAt 99_999_999] True
        `shouldBe` Left (BootNoUsableSnapshot 12_000_000)

    it "rejects rows with no last_committed_slot" $ do
      let row = completedRow { ssrLastCommittedSlot = Nothing }
      decideBoot (Just row) [snapshotAt 1_000_000] True
        `shouldBe` Left BootSyncStateMissing

  describe "ledger-disabled resume" $ do
    let row = committedRow False
    it "is BootResume with no chosen snapshot" $
      withResume (Just row) [] False $ \rc -> do
        rcChosenSnapshot rc `shouldBe` Nothing
        rcSyncState rc      `shouldBe` row

    it "rcIntersection is ReadyPoint with the row's (slot, hash)" $
      withResume (Just row) [] False $ \rc ->
        rcIntersection rc
          `shouldBe` ReadyPoint
            (mkCardanoPoint 12_000_000 (BS.replicate 32 0xab))

  describe "ledger-enabled resume" $ do
    it "fails when there are no snapshots" $
      decideBoot (Just (committedRow True)) [] True
        `shouldBe` Left BootResumeStateMissing

    it "fails when no snapshot is at-or-before last_committed_slot" $
      decideBoot (Just (committedRow True)) [snapshotAt 99_999_999] True
        `shouldBe` Left (BootNoUsableSnapshot 12_000_000)

    it "picks the newest snapshot at-or-before last_committed_slot" $ do
      let snaps = [snapshotAt 13_000_000, snapshotAt 11_500_000, snapshotAt 11_000_000]
      withResume (Just (committedRow True)) snaps True $ \rc ->
        rcChosenSnapshot rc `shouldBe` Just (snapshotAt 11_500_000)

    it "rejects when last_committed_slot is set but block_hash is missing" $ do
      let row = (committedRow True) { ssrLastCommittedBlockHash = Nothing }
      decideBoot (Just row) [snapshotAt 11_500_000] True
        `shouldSatisfy` isLeft

    -- Regression: the boot used to mint
    -- @mkCardanoPoint snap.dsNumber row.lastCommittedHash@, a
    -- synthetic point pairing the snapshot's slot with the
    -- /last-committed/ block's hash. That (slot, hash) pair existed
    -- nowhere on the chain, so chainsync intersection always failed.
    -- The new shape carries only the slot; the caller looks up the
    -- canonical hash in PG.
    it "rcIntersection is NeedsPgHashes [<chosen-slot>] for a single in-range snapshot" $
      withResume (Just (committedRow True)) [snapshotAt 11_500_000] True $ \rc ->
        rcIntersection rc `shouldBe` NeedsPgHashes [11_500_000]

    it "rcIntersection lists every in-range snapshot, newest-first" $ do
      let snaps =
            [ snapshotAt 13_000_000   -- newest, /past/ last_committed_slot
            , snapshotAt 11_900_000   -- in-range
            , snapshotAt 11_500_000   -- in-range
            , snapshotAt 11_000_000   -- in-range
            ]
      withResume (Just (committedRow True)) snaps True $ \rc -> do
        rcChosenSnapshot rc `shouldBe` Just (snapshotAt 11_900_000)
        rcIntersection rc
          `shouldBe` NeedsPgHashes [11_900_000, 11_500_000, 11_000_000]
