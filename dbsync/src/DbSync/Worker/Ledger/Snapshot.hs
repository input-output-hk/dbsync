{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Snapshot listing, writing, loading and deletion.
--
-- Wraps consensus's 'SnapshotManager' and adds tracing, defensive
-- deletion, the checkpoint buffer's edge points, and the async writer
-- thread that drains 'leSnapshotQueue'.
--
-- Loading is not on the manager: the V2 backend exposes
-- @newHandleFromSnapshot@, which the boot flow wires up as the
-- 'leLoadSnapshot' callback.
module DbSync.Worker.Ledger.Snapshot
  ( -- * Listing
    listDiskSnapshots
  , listMemorySnapshots
  , listKnownSnapshots
  , getSlotNoSnapshot

    -- * Writing
  , saveCurrentLedgerState

    -- * Async writer thread
  , runLedgerStateWriteThread
  , snapshotWriteLoop

    -- * Loading
  , loadSnapshotFromDisk

    -- * Deletion
  , safeDeleteSnapshot
  , deleteNewerSnapshots

    -- * Retention
  , snapshotRetention
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
  , SnapshotPolicy (..)
  , trimSnapshots
  )

import Ouroboros.Network.Block (data BlockPoint, data GenesisPoint, pointSlot)

import DbSync.AppM (LedgerM, runAppM)
import DbSync.SyncState.Row (markSnapshotComplete)
import DbSync.Worker.Ledger.Types
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
import DbSync.Parser.Types (CardanoPoint)
import DbSync.Error.Render (logThreadExit)
import DbSync.Trace.Types (LogMsg (..), Severity (..))

-- ---------------------------------------------------------------------------
-- * Listing
-- ---------------------------------------------------------------------------

-- | On-disk snapshots known to the backend, newest first by 'dsNumber'.
listDiskSnapshots :: LedgerM [DiskSnapshot]
listDiskSnapshots = do
  env <- ask
  liftIO $ listSnapshots (leSnapshotManager env)

-- | The newest and oldest points of the 'LedgerDB' checkpoint buffer.
-- Those are the only two a rollback caller can target without reaching
-- deeper into the buffer. Genesis is dropped: configuration re-derives
-- it, so it is not a useful rollback target.
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

-- | Buffered and on-disk snapshots, newest slot first. The boot flow
-- picks a resume anchor from this; the rollback path checks whether
-- the checkpoint buffer can serve a target.
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

-- | Enqueue a 'DbSyncStateRef' for the async snapshot writer.
--
-- Flips @srCanClose@ to 'False' in the same transaction as the
-- enqueue, so the 'DbSync.Worker.Ledger.State' pruner cannot close the
-- handle while the write is in flight. 'snapshotWriteLoop' flips it
-- back. Retention runs there too, after each successful write.
-- Skipped once 'leStopVar' is set: the writer is on its way out, so an
-- enqueue on a full queue would never drain.
saveCurrentLedgerState :: DbSyncStateRef -> LedgerM ()
saveCurrentLedgerState sref = do
  env <- ask
  liftIO $ atomically $
    orElse
      (do
        writeTVar (srCanClose sref) False
        writeTBQueue (leSnapshotQueue env) sref)
      (readTVar (leStopVar env) >>= check)

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
    -- Long heartbeats, so a profiler on a no-ledger run shows a named
    -- idle thread instead of a busy loop.
    idleForever :: IO ()
    idleForever = forever $ threadDelay tenMinutesMicros

    tenMinutesMicros :: Int
    tenMinutesMicros = 10 * 60 * 1_000_000

-- | Catch synchronous exceptions only. Asynchronous exceptions
-- (e.g. 'AsyncCancelled' from a cancelled 'Async' during shutdown)
-- propagate so the surrounding thread exits cleanly instead of
-- logging them as recoverable errors and continuing.
trySync :: IO a -> IO (Either Exception.SomeException a)
trySync action =
  (Right <$> action) `Exception.catch` \e ->
    case Exception.fromException e :: Maybe Exception.SomeAsyncException of
      Just _  -> Exception.throwIO e
      Nothing -> pure (Left e)

-- | The drain loop: read a 'DbSyncStateRef' off the queue, hand it to
-- the 'SnapshotManager', then set @srCanClose@ so the pruner may close
-- the handle.
--
-- Every step failure is recoverable: log a warning and continue. The
-- snapshot directory plus boot-time discovery is the durable record,
-- so a missed 'markSnapshotComplete' or 'trimSnapshots' must not stop
-- the sync. Asynchronous exceptions still propagate through 'trySync'.
snapshotWriteLoop :: LedgerM ()
snapshotWriteLoop = do
  env <- ask
  liftIO $ do
    traceWith (leTracer env) $
      LogMsg Info "LedgerSnapshot"
        "snapshot-writer starting (draining snapshot queue)"
    -- Queue first, so a snapshot already handed over still gets written.
    let loop = do
          mSref <- atomically $
            (Just <$> readTBQueue (leSnapshotQueue env))
              `orElse` (Nothing <$ (readTVar (leStopVar env) >>= check))
          case mSref of
            Nothing -> logMsg env Info "shutdown requested; snapshot writer exiting"
            Just sref -> do
              processOneSnapshot env sref
                `Exception.finally` atomically (writeTVar (srCanClose sref) True)
              loop
    loop
  where
    logMsg :: LedgerEnv -> Severity -> Text -> IO ()
    logMsg env sev msg =
      traceWith (leTracer env) (LogMsg sev "LedgerSnapshot" msg)

    processOneSnapshot :: LedgerEnv -> DbSyncStateRef -> IO ()
    processOneSnapshot env sref = do
      result <- trySync $
        takeSnapshot
          (leSnapshotManager env)
          Nothing                             -- temporary snapshot, no suffix
          (toConsensusStateRef sref)
      case result of
        Right (Just (ds, _rp)) -> do
          -- Record the completion in PG so a subsequent boot has a
          -- deterministic anchor. If the UPDATE fails the snapshot
          -- file is still on disk; boot-time 'listSnapshots' will
          -- rediscover it.
          markResult <- trySync $ runAppM env (markSnapshotComplete (dsNumber ds))
          case markResult of
            Right () ->
              logMsg env Info $
                "Wrote snapshot at slot " <> show (dsNumber ds)
            Left ex ->
              logMsg env Warning $
                "Snapshot at slot " <> show (dsNumber ds)
                  <> " written but markSnapshotComplete failed ("
                  <> Text.pack (Exception.displayException ex)
                  <> "); snapshot file remains on disk and will be rediscovered on boot."
          trimRetention env
        Right Nothing ->
          logMsg env Info "takeSnapshot returned Nothing — backend declined to write"
        Left ex ->
          logMsg env Warning $
            "Snapshot write failed ("
              <> Text.pack (Exception.displayException ex)
              <> "); the next epoch's snapshot attempt will retry."

    trimRetention :: LedgerEnv -> IO ()
    trimRetention env = do
      result <- trySync $
        trimSnapshots (leSnapshotManager env) retentionPolicy
      case result of
        Right deleted
          | not (null deleted) ->
              logMsg env Info $
                "Trimmed " <> show (length deleted)
                  <> " snapshot(s) (retention=" <> show snapshotRetention <> ")"
          | otherwise -> pure ()
        Left ex ->
          logMsg env Warning $
            "Snapshot retention trim failed ("
              <> Text.pack (Exception.displayException ex)
              <> "); retention will be re-checked after the next snapshot write."

    -- 'trimSnapshots' ignores 'onDiskShouldTakeSnapshot'; only the
    -- count is consulted.
    retentionPolicy :: SnapshotPolicy
    retentionPolicy = SnapshotPolicy
      { onDiskNumSnapshots       = snapshotRetention
      , onDiskShouldTakeSnapshot = \_ _ -> False
      }

-- ---------------------------------------------------------------------------
-- * Loading
-- ---------------------------------------------------------------------------

-- | Load a 'DiskSnapshot' through the configured backend and bridge
-- the consensus 'StateRef' into a 'DbSyncStateRef'.
--
-- The loaded ref gets a conservative 'ByronEpochBlockNo'. The boot
-- flow owns the era dispatcher, so it patches the real value.
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
      Left ex ->
        traceWith (leTracer env) $
          LogMsg Warning "LedgerSnapshot"
            ( "safeDeleteSnapshot: LSM session unaware of snapshot at slot "
                <> show (dsNumber ds)
                <> " (probable orphan from a crashed write); ignoring — "
                <> Text.pack (Exception.displayException ex)
            )

-- | Delete every disk snapshot strictly newer than the given slot.
-- Deletes go through 'safeDeleteSnapshot', so one failure does not
-- abort the rest.
deleteNewerSnapshots :: SlotNo -> LedgerM ()
deleteNewerSnapshots (SlotNo s) = do
  snaps <- listDiskSnapshots
  let newer = filter (\ds -> dsNumber ds > s) snaps
  forM_ newer safeDeleteSnapshot

-- ---------------------------------------------------------------------------
-- * Retention
-- ---------------------------------------------------------------------------

-- | How many on-disk snapshots to keep after each successful write:
-- the chosen resume anchor, one fallback if it fails to load, and one
-- buffer.
snapshotRetention :: Word
snapshotRetention = 3
