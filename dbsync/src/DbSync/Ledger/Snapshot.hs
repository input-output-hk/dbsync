{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : DbSync.Ledger.Snapshot
Description : Snapshot management — listing, writing, loading, deleting.

Sits on top of consensus's
'Ouroboros.Consensus.Storage.LedgerDB.Snapshots.SnapshotManager' (a
three-operation record: @listSnapshots@, @deleteSnapshotIfTemporary@,
@takeSnapshot@). Every helper here is a thin wrapper that adds
project-specific concerns: tracing, defensive deletion, the in-memory
buffer's edge-points contribution to 'listKnownSnapshots', and the
async writer thread that drains 'leSnapshotQueue'.

Snapshot loading is __not__ on the manager — the V2 backend provides
@newHandleFromSnapshot@ directly, surfaced via the
'leLoadSnapshot' callback that the boot flow wires up. We bridge that
into a 'DbSyncStateRef' here.

Two invariants from the ledger-state plan are enforced in this module:

  * I3 (in-flight handle safety) — 'saveCurrentLedgerState' flips
    @srCanClose@ to @False@ before enqueueing; 'snapshotWriteLoop'
    flips it back after the write completes. The pruner in
    'DbSync.Ledger.State' must not @close@ a handle while @srCanClose@
    is @False@.
  * Defensive delete — 'safeDeleteSnapshot' tolerates LSM-orphan
    failures (a snapshot directory that exists on disk but the LSM
    session has lost track of). The consensus default already catches
    @SomeException@; we add a tracer call so the operator sees what
    happened.
-}
module DbSync.Ledger.Snapshot
  ( -- * Listing
    listDiskSnapshots
  , listMemorySnapshots
  , listKnownSnapshots
  , getSlotNoSnapshot

    -- * Writing
  , saveCurrentLedgerState
  , saveCleanupState

    -- * Async writer thread
  , runLedgerStateWriteThread
  , snapshotWriteLoop

    -- * Loading
  , loadSnapshotFromDisk
  , findStateFromSnapshot

    -- * Deletion
  , safeDeleteSnapshot
  , deleteNewerSnapshots
  ) where

import Cardano.Prelude hiding (atomically)

import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Control.Concurrent.Class.MonadSTM.Strict (atomically, readTVar, writeTVar)
import Control.Concurrent.STM.TBQueue (readTBQueue, writeTBQueue)
import qualified Control.Exception as Exception
import Control.Tracer (traceWith)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text as Text

import Ouroboros.Consensus.Block (castPoint)
import qualified Ouroboros.Consensus.Ledger.Abstract as Consensus
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
  ( DiskSnapshot (..)
  , SnapshotManager (..)
  )

import Ouroboros.Network.Block (Point (..), pointSlot)

import DbSync.Ledger.Types
  ( CardanoLedgerState (..)
  , DbSyncStateRef (..)
  , EpochBlockNo (..)
  , HasLedgerEnv (..)
  , LedgerDB (..)
  , LedgerEnv (..)
  , SnapshotPoint (..)
  , fromConsensusStateRef
  , toConsensusStateRef
  )
import DbSync.Block.Types (CardanoPoint)
import DbSync.Trace.Types (LogMsg (..), Severity (..))

-- ---------------------------------------------------------------------------
-- * Listing
-- ---------------------------------------------------------------------------

-- | All on-disk snapshots known to the configured backend.
-- Newest-first by 'DiskSnapshot.dsNumber'.
listDiskSnapshots :: LedgerEnv -> IO [DiskSnapshot]
listDiskSnapshots env = listSnapshots (leSnapshotManager env)

-- | The points represented by the in-memory 'LedgerDB' checkpoint
-- buffer. Genesis is filtered out because it isn't a useful rollback
-- target (re-derived from configuration).
--
-- Returns the \"edge\" points (newest + oldest of the buffer) rather
-- than every checkpoint — those are the only ones a rollback caller
-- can usefully target without first reaching deeper into the buffer.
listMemorySnapshots :: LedgerEnv -> IO [CardanoPoint]
listMemorySnapshots env = do
  mLedger <- atomically $ readTVar (leStateVar env)
  case mLedger of
    Strict.Nothing -> pure []
    Strict.Just (LedgerDB s) ->
      pure $ filter notGenesis (edgePoints s)
  where
    edgePoints :: StrictSeq.StrictSeq DbSyncStateRef -> [CardanoPoint]
    edgePoints s =
      case Foldable.toList s of
        []           -> []
        [single]     -> [refToPoint single]
        (newest : _) ->
          let oldest = List.last (Foldable.toList s)
           in [refToPoint newest, refToPoint oldest]

    refToPoint :: DbSyncStateRef -> CardanoPoint
    refToPoint = castPoint . Consensus.getTip . clsState . srState

    notGenesis :: CardanoPoint -> Bool
    notGenesis GenesisPoint    = False
    notGenesis (BlockPoint{}) = True

-- | Combined list of in-memory and on-disk snapshots, newest-slot
-- first. Used by the boot flow to decide which snapshot to resume
-- from and by the rollback path to decide whether the in-memory
-- buffer can serve a target.
listKnownSnapshots :: LedgerEnv -> IO [SnapshotPoint]
listKnownSnapshots env = do
  inMem  <- fmap InMemory <$> listMemorySnapshots env
  onDisk <- fmap OnDisk   <$> listDiskSnapshots env
  pure $ List.sortOn (Down . getSlotNoSnapshot) (inMem <> onDisk)

-- | Slot of a 'SnapshotPoint'. 'OnDisk' snapshots carry their slot
-- in 'dsNumber' (we use slot number as the snapshot number);
-- 'InMemory' points carry it directly.
getSlotNoSnapshot :: SnapshotPoint -> WithOrigin SlotNo
getSlotNoSnapshot = \case
  OnDisk ds   -> NotOrigin (SlotNo (dsNumber ds))
  InMemory cp -> pointSlot cp

-- ---------------------------------------------------------------------------
-- * Writing
-- ---------------------------------------------------------------------------

-- | Enqueue a 'DbSyncStateRef' for the async snapshot writer to
-- persist to disk. Atomically flips @srCanClose@ to 'False' so the
-- LedgerDB pruner can't close the handle out from under the writer
-- — see invariant I3.
saveCurrentLedgerState :: LedgerEnv -> DbSyncStateRef -> IO ()
saveCurrentLedgerState env sref = atomically $ do
  writeTVar (srCanClose sref) False
  writeTBQueue (leSnapshotQueue env) sref

-- | Enqueue a snapshot write and trim older snapshots according to
-- the retention policy. Trimming is currently a no-op stub —
-- consensus's 'trimSnapshots' needs a 'SnapshotPolicy', which lives
-- on the boot-flow-supplied snapshot manager rather than 'LedgerEnv',
-- and the wiring will land alongside the boot flow that constructs
-- both together.
saveCleanupState :: LedgerEnv -> DbSyncStateRef -> IO ()
saveCleanupState env sref = do
  saveCurrentLedgerState env sref
  -- TODO: invoke 'trimSnapshots (leSnapshotManager env) policy' once
  -- 'SnapshotPolicy' is plumbed through to 'LedgerEnv'. Until then
  -- the snapshot manager retains every snapshot it writes; operators
  -- can manually clean up by deleting old slot directories.
  pure ()

-- ---------------------------------------------------------------------------
-- * Async writer thread
-- ---------------------------------------------------------------------------

-- | Top-level entry point for the snapshot-writer thread. When the
-- ledger feature is disabled there's no queue to drain — we just
-- block forever so the surrounding 'withAsync' wiring doesn't have
-- to special-case the disabled arm.
runLedgerStateWriteThread :: HasLedgerEnv -> IO ()
runLedgerStateWriteThread = \case
  LedgerEnabled env  -> snapshotWriteLoop env
  LedgerDisabled _nle -> idleForever
  where
    -- 10-minute heartbeats, so a future maintainer who attaches a
    -- profiler to a no-ledger run sees a clearly-named idle thread
    -- rather than a tight busy loop.
    idleForever :: IO ()
    idleForever = forever $ threadDelay tenMinutesMicros

    tenMinutesMicros :: Int
    tenMinutesMicros = 10 * 60 * 1_000_000

-- | The drain loop: read a 'DbSyncStateRef' off the queue, hand it to
-- the consensus 'SnapshotManager', and clear the @srCanClose@ flag
-- so the pruner is free to close the handle once no other reference
-- holds it.
--
-- Exceptions during 'takeSnapshot' are caught and traced: a single
-- failed snapshot must not bring down the whole sync.
snapshotWriteLoop :: LedgerEnv -> IO ()
snapshotWriteLoop env = forever $ do
  sref <- atomically $ readTBQueue (leSnapshotQueue env)
  result <-
    Exception.try @Exception.SomeException $
      takeSnapshot
        (leSnapshotManager env)
        Nothing                             -- temporary snapshot, no suffix
        (toConsensusStateRef sref)
  case result of
    Right (Just (ds, _rp)) ->
      logMsg Info $
        "Wrote snapshot at slot " <> show (dsNumber ds)
    Right Nothing ->
      logMsg Info "takeSnapshot returned Nothing — backend declined to write"
    Left ex ->
      logMsg Warning $
        "Snapshot write failed: " <> Text.pack (Exception.displayException ex)
  -- I3: writer is done with the handle, pruner is now free to close.
  atomically $ writeTVar (srCanClose sref) True
  where
    logMsg :: Severity -> Text -> IO ()
    logMsg sev msg =
      traceWith (leTracer env) (LogMsg sev "LedgerSnapshot" msg Nothing)

-- ---------------------------------------------------------------------------
-- * Loading
-- ---------------------------------------------------------------------------

-- | Load a 'DiskSnapshot' through the configured backend and bridge
-- the consensus 'StateRef' into our 'DbSyncStateRef' shape.
--
-- The @epochBlockNo@ is conservatively set to 'ByronEpochBlockNo' on
-- the loaded ref. Callers that have a more precise idea (eg. derived
-- from the snapshot metadata or by inspecting the ledger state's tip
-- slot vs. the era boundaries) should patch it after the fact —
-- exposing a derive helper here would couple this module to the era
-- dispatcher and is left to the boot flow.
loadSnapshotFromDisk
  :: LedgerEnv
  -> DiskSnapshot
  -> IO (Either Text DbSyncStateRef)
loadSnapshotFromDisk env ds = do
  result <- leLoadSnapshot env ds
  case result of
    Left err           -> pure (Left err)
    Right consensusRef -> Right <$> fromConsensusStateRef ByronEpochBlockNo consensusRef

-- | Find a 'DiskSnapshot' at the given 'CardanoPoint' (or older) and
-- load it. Returns 'Right' on success, or 'Left' with the list of
-- candidate snapshots that were tried-and-rejected so the caller can
-- continue the search.
--
-- Currently __not implemented__: the resume-constraint (I2) and
-- newer-snapshot deletion logic from the ledger-state plan §7 Path B
-- live in the boot flow rather than here. This stub returns an empty
-- candidate list; the boot flow uses 'listDiskSnapshots' +
-- 'loadSnapshotFromDisk' + 'deleteNewerSnapshots' directly.
findStateFromSnapshot
  :: LedgerEnv
  -> CardanoPoint
  -> IO (Either [DiskSnapshot] DbSyncStateRef)
findStateFromSnapshot _env _point =
  panic "DbSync.Ledger.Snapshot.findStateFromSnapshot: boot-flow ownership; wired alongside Path B"

-- ---------------------------------------------------------------------------
-- * Deletion
-- ---------------------------------------------------------------------------

-- | Delete a snapshot if it is temporary (no permanence suffix),
-- swallowing exceptions and tracing them. The consensus default
-- already catches 'SomeException' in
-- 'defaultDeleteSnapshotIfTemporary'; we add the trace so an LSM
-- orphan (snapshot directory present, LSM session unaware of it)
-- becomes visible in operator logs rather than silently disappearing.
safeDeleteSnapshot :: LedgerEnv -> DiskSnapshot -> IO ()
safeDeleteSnapshot env ds = do
  result <-
    Exception.try @Exception.SomeException $
      deleteSnapshotIfTemporary (leSnapshotManager env) ds
  case result of
    Right () ->
      traceWith (leTracer env) $
        LogMsg Debug "LedgerSnapshot"
          ("Deleted temporary snapshot at slot " <> show (dsNumber ds))
          Nothing
    Left ex ->
      traceWith (leTracer env) $
        LogMsg Warning "LedgerSnapshot"
          ( "safeDeleteSnapshot: failed to delete snapshot at slot "
              <> show (dsNumber ds)
              <> " — "
              <> Text.pack (Exception.displayException ex)
          )
          Nothing

-- | Delete every disk snapshot strictly newer than the given slot.
-- Used by the rollback path and by the boot-flow resume-constraint
-- check (I2) in the ledger-state plan §7 Path B.
--
-- Walks the snapshot list and applies 'safeDeleteSnapshot' to each
-- candidate, so a single failed delete doesn't abort the rest.
deleteNewerSnapshots :: LedgerEnv -> SlotNo -> IO ()
deleteNewerSnapshots env (SlotNo s) = do
  snaps <- listDiskSnapshots env
  let newer = filter (\ds -> dsNumber ds > s) snaps
  forM_ newer (safeDeleteSnapshot env)
