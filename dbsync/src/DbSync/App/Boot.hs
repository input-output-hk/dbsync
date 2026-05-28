{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

-- | Pure boot-flow dispatch.
--
-- Given the observed PG state (sync-state row + on-disk snapshots)
-- and the current ledger config, 'decideBoot' classifies the boot
-- into one of three actions:
--
--   * 'BootFresh' — start from genesis. Reached on a fresh DB or
--     when a row exists but no epoch has been committed yet.
--   * 'BootResume' — resume an interrupted Ingest.
--   * 'BootFollowRestart' — a prior run completed sync; resume
--     directly in the Follow phase.
--
-- Mismatch cases (config flipped, missing snapshot, etc.) are
-- returned as 'BootError'. Callers render these via
-- 'renderBootError' and exit.
module DbSync.App.Boot
  ( -- * Types
    BootDecision (..)
  , ResumeContext (..)
  , ResumeIntersection (..)
  , FollowRestartContext (..)
  , BootError (..)

    -- * Decision
  , decideBoot

    -- * Operator-facing rendering
  , renderBootError

    -- * Helpers (exported for tests)
  , mkCardanoPoint
  , candidateSnapshotSlots
  , resumeContextFrom
  ) where

import Cardano.Prelude

import qualified Data.Text as T
import Ouroboros.Consensus.Block.Abstract (fromRawHash)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Cardano.Node ()                       -- 'CanHardFork' orphan
import Ouroboros.Consensus.Shelley.HFEras ()                     -- per-era HFC instances
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()    -- 'LedgerSupportsProtocol' orphans
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots (DiskSnapshot (..))
import Ouroboros.Network.Block (pattern BlockPoint, pattern GenesisPoint)
import Cardano.Slotting.Slot (SlotNo (..))

import DbSync.Block.Types (CardanoPoint)
import DbSync.Db.Schema.SyncState (SyncStateRow (..))
import DbSync.Ledger.Fingerprint (LedgerStateFingerprint, renderFingerprint)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | What the boot flow should do.
data BootDecision
  = BootFresh
    -- ^ Start from genesis (fresh DB or a seeded-but-uncommitted row).
  | BootResume !ResumeContext
    -- ^ Resume past a previous run.
  | BootFollowRestart !FollowRestartContext
    -- ^ A prior run completed sync; resume directly in Follow.
  deriving stock (Eq, Show)

-- | Information needed to resume from a 'SyncStateRow'.
--
-- Invariant: when 'rcChosenSnapshot' is 'Just' (ledger-enabled
-- resume), its 'dsNumber' equals the head of 'rcIntersection'\'s
-- @NeedsPgHashes@ list — that snapshot is the one the in-RAM
-- ledger is restored from, and also the preferred intersection
-- point we offer the node.
data ResumeContext = ResumeContext
  { rcSyncState      :: !SyncStateRow
  , rcChosenSnapshot :: !(Maybe DiskSnapshot)
  , rcIntersection   :: !ResumeIntersection
  }
  deriving stock (Eq, Show)

-- | Information needed to enter the Follow phase on a restart after
-- Ingest+Prep have completed (@sync_complete = true@).
--
-- The candidate list is non-empty exactly when ledger is enabled and
-- at least one snapshot survives at or before @last_committed_slot@.
-- When ledger is disabled the list is empty: PG is the single source
-- of truth and the caller intersects chainsync directly at
-- @last_committed_*@.
--
-- The orchestrator walks the candidates newest-first, picking the
-- first one whose slot has a matching @block.hash@ in PG. That
-- chosen snapshot becomes the restart point: the ledger is loaded
-- from it, the gap to @last_committed_slot@ becomes the replay
-- window, and chainsync intersects at the snapshot\'s point.
data FollowRestartContext = FollowRestartContext
  { frcSyncState           :: !SyncStateRow
  , frcCandidateSnapshots  :: ![DiskSnapshot]
    -- ^ Snapshots at or before @last_committed_slot@, newest-first.
    -- Empty when ledger is disabled.
  }
  deriving stock (Eq, Show)

-- | How the chainsync intersection point(s) are produced.
--
-- Ledger-disabled resume can hand the receiver a fully-formed point
-- straight from the @last_committed_*@ columns. Ledger-enabled resume
-- can\'t: a snapshot only knows its slot (consensus's 'DiskSnapshot'
-- is @dsNumber + dsSuffix@), and PG\'s last-committed hash refers to
-- a /different/ block. So we nominate slots here and let the caller
-- ask PG for the canonical hash at each — mirrors upstream
-- cardano-db-sync's @verifySnapshotPoint@.
data ResumeIntersection
  = ReadyPoint !CardanoPoint
    -- ^ Ledger-disabled: @(slot, hash)@ from 'SyncStateRow'.
  | NeedsPgHashes ![Word64]
    -- ^ Ledger-enabled: candidate snapshot slots, /newest-first/.
    -- Head is the chosen snapshot for ledger restoration; the rest
    -- are fallbacks. The caller resolves each via PG and drops
    -- orphans (no matching @block.hash@) silently.
  deriving stock (Eq, Show)

-- | Boot mismatches that abort the run.
data BootError
  = BootSyncStateMissing
    -- ^ Schema present but no @dbsync_sync_state@ row.
  | BootLedgerEnabledMismatch !Bool !Bool
    -- ^ The row's @ledger_enabled@ disagrees with the current
    -- config's @ledger.enabled@. Fields: @(rowSays, configSays)@.
  | BootResumeStateMissing
    -- ^ Ledger enabled, PG has committed data, on-disk snapshot
    -- directory is empty.
  | BootSnapshotsWithoutPgState
    -- ^ Ledger enabled, snapshot directory has content, PG row
    -- records no committed progress.
  | BootNoUsableSnapshot !Word64
    -- ^ Ledger enabled, the row has @last_committed_slot@, but no
    -- on-disk snapshot exists at or before that slot.
  | BootLedgerStateFingerprintMismatch !LedgerStateFingerprint !LedgerStateFingerprint
    -- ^ The fingerprint file in the ledger state directory describes
    -- a different chain than the current config. Fields:
    -- @(onDisk, expected)@.
  | BootLedgerStateFingerprintMissing !FilePath
    -- ^ The ledger state directory contains data but no readable
    -- fingerprint file. Carries the directory path for the operator
    -- message.
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Decision
-- ---------------------------------------------------------------------------

-- | Classify the boot.
decideBoot
  :: Maybe SyncStateRow
  -> [DiskSnapshot]            -- ^ Disk snapshots, newest-first. Empty when ledger disabled.
  -> Bool                      -- ^ @ledger.enabled@ from current config.
  -> Either BootError BootDecision
decideBoot mRow snapshots ledgerEnabledCfg = case mRow of
  Nothing
    | ledgerEnabledCfg, not (null snapshots) ->
        Left BootSnapshotsWithoutPgState
    | otherwise ->
        Left BootSyncStateMissing

  Just row
    | ssrLedgerEnabled row /= ledgerEnabledCfg ->
        Left $ BootLedgerEnabledMismatch (ssrLedgerEnabled row) ledgerEnabledCfg

    | ssrSyncComplete row ->
        decideFollowRestart row snapshots ledgerEnabledCfg

    | rowHasNoCommittedProgress row ->
        -- Snapshots without committed PG progress would diverge
        -- silently from genesis-seeded ledger state.
        if ledgerEnabledCfg && not (null snapshots)
          then Left BootSnapshotsWithoutPgState
          else Right BootFresh

    | not ledgerEnabledCfg ->
        case (ssrLastCommittedSlot row, ssrLastCommittedBlockHash row) of
          (Just slotNo, Just blockHash) ->
            Right $ BootResume
              ResumeContext
                { rcSyncState     = row
                , rcChosenSnapshot = Nothing
                , rcIntersection  = ReadyPoint (mkCardanoPoint slotNo blockHash)
                }
          _ ->
            Left BootSyncStateMissing

    | otherwise ->
        case (ssrLastCommittedSlot row, ssrLastCommittedBlockHash row) of
          (Just slotNo, Just _)
            | null snapshots -> Left BootResumeStateMissing
            | otherwise ->
                case candidateSnapshotSlots snapshots slotNo of
                  []                 -> Left (BootNoUsableSnapshot slotNo)
                  candidates@(c : _) ->
                    Right $ BootResume
                      ResumeContext
                        { rcSyncState      = row
                        , rcChosenSnapshot = Just c
                        , rcIntersection  = NeedsPgHashes (map dsNumber candidates)
                        }
          _ -> Left BootSyncStateMissing

-- | Build the 'BootFollowRestart' decision for @sync_complete =
-- true@.
--
-- Ledger disabled: PG is the single source of truth, candidate list
-- is empty, the caller intersects chainsync directly at the row's
-- @last_committed_*@.
--
-- Ledger enabled: returns the candidate list (snapshots at or
-- before @last_committed_slot@, newest-first). Same fallback
-- conditions as 'BootResume' — empty snapshot directory is
-- 'BootResumeStateMissing', candidates all beyond the committed
-- slot is 'BootNoUsableSnapshot'.
decideFollowRestart
  :: SyncStateRow
  -> [DiskSnapshot]
  -> Bool
  -> Either BootError BootDecision
decideFollowRestart row snapshots ledgerEnabledCfg
  | not ledgerEnabledCfg =
      Right $ BootFollowRestart
        FollowRestartContext
          { frcSyncState           = row
          , frcCandidateSnapshots  = []
          }
  | otherwise =
      case ssrLastCommittedSlot row of
        Nothing ->
          -- 'sync_complete = true' implies Ingest+Prep ran, which
          -- only happens after at least one epoch boundary was
          -- committed. Treat the inconsistent shape as a missing
          -- resume anchor.
          Left BootSyncStateMissing
        Just slotNo
          | null snapshots -> Left BootResumeStateMissing
          | otherwise ->
              case candidateSnapshotSlots snapshots slotNo of
                []         -> Left (BootNoUsableSnapshot slotNo)
                candidates ->
                  Right $ BootFollowRestart
                    FollowRestartContext
                      { frcSyncState           = row
                      , frcCandidateSnapshots  = candidates
                      }

-- | True when the row has no committed chain position.
rowHasNoCommittedProgress :: SyncStateRow -> Bool
rowHasNoCommittedProgress r =
  isNothing (ssrLastCommittedSlot r)
    && isNothing (ssrLastCommittedBlockNo r)
    && isNothing (ssrLastCommittedBlockHash r)

-- | Build a 'ResumeContext' with no chosen snapshot and a default
-- intersection point. Used by the in-process Ingest → Prep → Follow
-- handoff where chainsync intersects at the row's last committed
-- block (the ledger and PG are already aligned by the receiver's
-- 'latestPointRef').
resumeContextFrom :: SyncStateRow -> Maybe DiskSnapshot -> ResumeContext
resumeContextFrom row mSnap =
  ResumeContext
    { rcSyncState     = row
    , rcChosenSnapshot = mSnap
    , rcIntersection  = case (ssrLastCommittedSlot row, ssrLastCommittedBlockHash row) of
        (Just s, Just h) -> ReadyPoint (mkCardanoPoint s h)
        _                -> ReadyPoint GenesisPoint
    }

-- | Snapshots at or before @lastSlot@, newest-first (relies on
-- @listSnapshots@\'s newest-first ordering).
candidateSnapshotSlots :: [DiskSnapshot] -> Word64 -> [DiskSnapshot]
candidateSnapshotSlots snaps lastSlot =
  filter (\ds -> dsNumber ds <= lastSlot) snaps

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | Build a 'CardanoPoint' from a raw slot number and 32-byte block
-- header hash.
mkCardanoPoint :: Word64 -> ByteString -> CardanoPoint
mkCardanoPoint slotNo blockHash =
  BlockPoint
    (SlotNo slotNo)
    (fromRawHash (Proxy @(CardanoBlock StandardCrypto)) blockHash)

-- ---------------------------------------------------------------------------
-- * Operator-facing rendering
-- ---------------------------------------------------------------------------

-- | Multi-line message suitable for stderr.
renderBootError :: BootError -> Text
renderBootError = \case
  BootSyncStateMissing ->
    T.unlines
      [ "Cannot resume: PG schema is present but the dbsync_sync_state row is empty."
      , ""
      , "This usually means one of:"
      , "  - The schema was manually created without seeding the sync-state row."
      , "  - A failed earlier run left the DB in a partial state."
      , ""
      , "Recovery: restart with --resync-from-genesis to wipe the database and"
      , "re-sync from genesis."
      ]

  BootLedgerEnabledMismatch rowSays cfgSays ->
    T.unlines
      [ "Cannot resume: ledger.enabled has flipped between runs."
      , ""
      , "  dbsync_sync_state.ledger_enabled = " <> tshow rowSays
      , "  current config ledger.enabled    = " <> tshow cfgSays
      , ""
      , "Resuming with a different ledger setting would invalidate the existing"
      , "data. Recovery options:"
      , "  - Restore the previous config so it matches the database."
      , "  - Restart with --resync-from-genesis to wipe both the database and"
      , "    the ledger state directory and re-sync from genesis."
      ]

  BootResumeStateMissing ->
    T.unlines
      [ "Cannot resume: PG database has committed data but the ledger state"
      , "directory is empty."
      , ""
      , "This usually means the state directory was moved, deleted, or never"
      , "copied into place; or that ledger.enabled was previously false and is"
      , "now true."
      , ""
      , "Recovery options:"
      , "  - Restore the state directory from a backup or snapshot bundle."
      , "  - Restart with --resync-from-genesis to wipe and re-sync from genesis."
      ]

  BootSnapshotsWithoutPgState ->
    T.unlines
      [ "Cannot resume: the ledger state directory contains snapshots, but the"
      , "PG database has no committed progress."
      , ""
      , "The two should advance in lock-step. This usually means the PG schema"
      , "was wiped without also wiping the state directory."
      , ""
      , "Recovery: restart with --resync-from-genesis to wipe both and re-sync"
      , "from genesis."
      ]

  BootNoUsableSnapshot lastSlot ->
    T.unlines
      [ "Cannot resume: PG records committed progress through slot "
          <> tshow lastSlot
      , "but no on-disk snapshot at or before that slot is available."
      , ""
      , "This usually means snapshots were manually deleted past the resume"
      , "point."
      , ""
      , "Recovery: restart with --resync-from-genesis to wipe both the database"
      , "and the ledger state directory and re-sync from genesis."
      ]

  BootLedgerStateFingerprintMismatch onDisk expected ->
    T.unlines
      [ "Cannot resume: the ledger state directory was created for a different chain."
      , ""
      , "  On disk  : " <> renderFingerprint onDisk
      , "  This run : " <> renderFingerprint expected
      , ""
      , "Resuming would corrupt the database. Recovery options:"
      , "  - Point --ledger-state-dir at the directory used for this network, or"
      , "  - Restart with --resync-from-genesis to wipe and re-sync from genesis."
      ]

  BootLedgerStateFingerprintMissing dir ->
    T.unlines
      [ "Cannot resume: the ledger state directory contains data but no"
      , "fingerprint file. Path:"
      , ""
      , "  " <> T.pack dir
      , ""
      , "This means the directory was populated by a build that predates the"
      , "fingerprint check, or the file was deleted. Refusing to proceed because"
      , "we cannot verify the directory belongs to this chain."
      , ""
      , "Recovery: restart with --resync-from-genesis to wipe and re-sync from"
      , "genesis."
      ]

tshow :: Show a => a -> Text
tshow = T.pack . show
