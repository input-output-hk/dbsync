{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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
import qualified Database.LSMTree as LSMTree

import Ouroboros.Consensus.Block (castPoint)
import qualified Ouroboros.Consensus.Ledger.Abstract as Consensus
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
  ( DiskSnapshot (..)
  , SnapshotManager (..)
  )

import Ouroboros.Network.Block (pattern BlockPoint, pattern GenesisPoint, pointSlot)

import DbSync.AppM (LedgerM, runAppM)
import DbSync.Checkpoint.SyncState (markSnapshotComplete)
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
import DbSync.Trace.Types (LogMsg (..), Severity (..), logThreadExit)

-- ---------------------------------------------------------------------------
-- * Listing
-- ---------------------------------------------------------------------------

-- | All on-disk snapshots known to the configured backend.
-- Newest-first by 'DiskSnapshot.dsNumber'.
listDiskSnapshots :: LedgerM [DiskSnapshot]
listDiskSnapshots = do
  env <- ask
  liftIO $ listSnapshots (leSnapshotManager env)

-- | The points represented by the in-memory 'LedgerDB' checkpoint
-- buffer. Genesis is filtered out because it isn't a useful rollback
-- target (re-derived from configuration).
--
-- Returns the \"edge\" points (newest + oldest of the buffer) rather
-- than every checkpoint — those are the only ones a rollback caller
-- can usefully target without first reaching deeper into the buffer.
listMemorySnapshots :: LedgerM [CardanoPoint]
listMemorySnapshots = do
  env <- ask
  mLedger <- liftIO $ atomically $ readTVar (leStateVar env)
  pure $ case mLedger of
    Strict.Nothing -> []
    Strict.Just (LedgerDB s) ->
      filter notGenesis (edgePoints s)
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
listKnownSnapshots :: LedgerM [SnapshotPoint]
listKnownSnapshots = do
  inMem  <- fmap InMemory <$> listMemorySnapshots
  onDisk <- fmap OnDisk   <$> listDiskSnapshots
  pure $ List.sortOn (Down . getSlotNoSnapshot) (inMem <> onDisk)

-- | Slot of a 'SnapshotPoint'. 'OnDisk' snapshots carry their slot
-- in 'dsNumber' (we use slot number as the snapshot number);
-- 'InMemory' points carry it directly.
getSlotNoSnapshot :: SnapshotPoint -> WithOrigin SlotNo
getSlotNoSnapshot = \case
  OnDisk ds   -> At (SlotNo (dsNumber ds))
  InMemory cp -> pointSlot cp

-- ---------------------------------------------------------------------------
-- * Writing
-- ---------------------------------------------------------------------------

-- | Enqueue a 'DbSyncStateRef' for the async snapshot writer to
-- persist to disk. Atomically flips @srCanClose@ to 'False' so the
-- LedgerDB pruner can't close the handle out from under the writer
-- — see invariant I3.
saveCurrentLedgerState :: DbSyncStateRef -> LedgerM ()
saveCurrentLedgerState sref = do
  env <- ask
  liftIO $ atomically $ do
    writeTVar (srCanClose sref) False
    writeTBQueue (leSnapshotQueue env) sref

-- | Enqueue a snapshot write and trim older snapshots according to
-- the retention policy. Trimming is currently a no-op stub —
-- consensus's 'trimSnapshots' needs a 'SnapshotPolicy', which lives
-- on the boot-flow-supplied snapshot manager rather than 'LedgerEnv',
-- and the wiring will land alongside the boot flow that constructs
-- both together.
saveCleanupState :: DbSyncStateRef -> LedgerM ()
saveCleanupState sref = do
  saveCurrentLedgerState sref
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
--
-- Exceptions escaping the enabled-arm loop are tagged via
-- 'logThreadExit' so an 'AsyncCancelled' from orderly shutdown logs
-- at 'Info' while a real crash logs at 'Error'.
runLedgerStateWriteThread :: HasLedgerEnv -> IO ()
runLedgerStateWriteThread = \case
  LedgerEnabled env  ->
    runAppM env snapshotWriteLoop
      `catch` \(e :: SomeException) -> do
        logThreadExit "LedgerSnapshot" e (leTracer env)
        throwIO e
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
-- Per-snapshot failures during 'takeSnapshot' are caught and traced
-- (one bad snapshot must not bring the whole sync down). Failures
-- elsewhere in the loop bubble up and are reported by the
-- 'runLedgerStateWriteThread' wrapper.
snapshotWriteLoop :: LedgerM ()
snapshotWriteLoop = do
  env <- ask
  liftIO $ traceWith (leTracer env) $ LogMsg Info "LedgerSnapshot"
    "snapshot-writer starting (draining snapshot queue)" Nothing
  forever $ do
    sref <- liftIO $ atomically $ readTBQueue (leSnapshotQueue env)
    result <- liftIO $
      Exception.try @Exception.SomeException $
        takeSnapshot
          (leSnapshotManager env)
          Nothing                             -- temporary snapshot, no suffix
          (toConsensusStateRef sref)
    case result of
      Right (Just (ds, _rp)) -> do
        -- Record the completion in PG so a subsequent boot has a
        -- deterministic anchor. If this UPDATE fails (DB hiccup,
        -- connection lost), the snapshot file is still on disk and
        -- a re-scan via 'listSnapshots' will discover it.
        markResult <- liftIO $
          Exception.try @Exception.SomeException $
            runAppM env (markSnapshotComplete (dsNumber ds))
        case markResult of
          Right () ->
            logMsg env Info $
              "Wrote snapshot at slot " <> show (dsNumber ds)
          Left ex ->
            logMsg env Warning $
              "Snapshot at slot " <> show (dsNumber ds)
                <> " written but markSnapshotComplete failed: "
                <> Text.pack (Exception.displayException ex)
      Right Nothing ->
        logMsg env Info "takeSnapshot returned Nothing — backend declined to write"
      Left ex ->
        logMsg env Warning $
          "Snapshot write failed: " <> Text.pack (Exception.displayException ex)
    -- I3: writer is done with the handle, pruner is now free to close.
    liftIO $ atomically $ writeTVar (srCanClose sref) True
  where
    logMsg :: LedgerEnv -> Severity -> Text -> LedgerM ()
    logMsg env sev msg =
      liftIO $ traceWith (leTracer env) (LogMsg sev "LedgerSnapshot" msg Nothing)

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
  :: DiskSnapshot
  -> LedgerM (Either Text DbSyncStateRef)
loadSnapshotFromDisk ds = do
  env <- ask
  liftIO $ do
    result <- leLoadSnapshot env ds
    case result of
      Left err           -> pure (Left err)
      Right consensusRef -> Right <$> fromConsensusStateRef ByronEpochBlockNo consensusRef

-- ---------------------------------------------------------------------------
-- * Deletion
-- ---------------------------------------------------------------------------

-- | Delete a snapshot if it is temporary (no permanence suffix),
-- tolerating the @SnapshotDoesNotExistError@ that the LSM backend
-- raises when its session has lost track of a snapshot directory
-- that's still on disk (a known issue after a crash mid-write).
--
-- Other exceptions propagate so the operator sees them — we only
-- swallow the well-known LSM-orphan case.
safeDeleteSnapshot :: DiskSnapshot -> LedgerM ()
safeDeleteSnapshot ds = do
  env <- ask
  liftIO $ do
    result <-
      Exception.try @LSMTree.SnapshotDoesNotExistError $
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
            ( "safeDeleteSnapshot: LSM session unaware of snapshot at slot "
                <> show (dsNumber ds)
                <> " (probable orphan from a crashed write); ignoring — "
                <> Text.pack (Exception.displayException ex)
            )
            Nothing

-- | Delete every disk snapshot strictly newer than the given slot.
-- Used by the rollback path and by the boot-flow resume-constraint
-- check (I2) in the ledger-state plan §7 Path B.
--
-- Walks the snapshot list and applies 'safeDeleteSnapshot' to each
-- candidate, so a single failed delete doesn't abort the rest.
deleteNewerSnapshots :: SlotNo -> LedgerM ()
deleteNewerSnapshots (SlotNo s) = do
  snaps <- listDiskSnapshots
  let newer = filter (\ds -> dsNumber ds > s) snaps
  forM_ newer safeDeleteSnapshot
