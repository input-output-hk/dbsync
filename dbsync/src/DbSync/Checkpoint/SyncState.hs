-- | Thin hasql wrapper around the @dbsync_sync_state@ singleton row.
--
-- Owns connection lifecycle and IO-level error mapping. The schema
-- type 'SyncStateRow', encoders\/decoders, and 'Statement' bindings
-- live in @dbsync-db@ (re-exported here so existing call sites don't
-- need a new import).
--
-- libpq remains for the loader-stream transport in
-- 'DbSync.Db.Loader.Connection'; the control-plane path here goes
-- through hasql.
module DbSync.Checkpoint.SyncState
  ( -- * Row type (re-export from dbsync-db)
    SyncStateRow (..)

    -- * Connection lifecycle
  , ControlConnection (..)
  , HasControlConnection (..)
  , openControlConnection
  , closeControlConnection

    -- * Read \/ write
  , readSyncState
  , writeSyncState
  , seedSyncState
  , markSnapshotComplete
  , markSyncComplete

    -- * Pending-rollback marker
  , readPendingRollbackSlot
  , writePendingRollbackSlot
  , clearPendingRollbackSlot

    -- * Boot-time canonicalisation
  , fetchBlockHashAtSlot

    -- * Dedup store rebuild
  , rebuildDedupMaps
  ) where

import Cardano.Prelude

import qualified Data.ByteString.Short as SBS
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Hasql.Connection as Conn
import qualified Hasql.Connection.Settings as Settings
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import Control.Tracer (traceWith)

import DbSync.Db.Schema.SyncState (SyncStateRow (..))
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.Resume
  ( selectBlockHashAtSlotStmt
  , selectDedupSingleStmt
  , selectMultiAssetDedupStmt
  )
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Timing (fmtCount, fmtDuration)
import DbSync.Trace.Types (LogMsg (..), Severity (..))
import DbSync.Db.Statement.SyncState
  ( clearPendingRollbackSlotStmt
  , markSnapshotCompleteStmt
  , markSyncCompleteStmt
  , readPendingRollbackSlotStmt
  , readSyncStateStmt
  , seedSyncStateStmt
  , writePendingRollbackSlotStmt
  , writeSyncStateStmt
  )
import DbSync.Error (throwDb)
import DbSync.Phase.Ingest.DedupStore
  ( DedupStore
  , DedupStores (..)
  , insertExisting
  , newStores
  )
import DbSync.Phase.Ingest.LsmSession (LsmSession)
import DbSync.Util.DedupHash (hashDedupKey)

-- ---------------------------------------------------------------------------
-- * Connection lifecycle
-- ---------------------------------------------------------------------------

-- | A hasql connection dedicated to non-COPY operations: sync-state
-- read\/write, dedup-map rebuild, resume-time row cleanup.
newtype ControlConnection = ControlConnection
  { unControlConnection :: Conn.Connection
  }

-- | Access the control connection from env.
class HasControlConnection env where
  getControlConnection :: env -> ControlConnection

-- | Self-instance so boot-time IO code can drive the polymorphic
-- helpers via @runAppM ctrlConn ...@ without building a phase env.
instance HasControlConnection ControlConnection where
  getControlConnection = identity

-- | Open a fresh 'ControlConnection'. Throws 'AppDatabaseError' on
-- handshake failure.
openControlConnection :: HasCallStack => Settings.Settings -> IO ControlConnection
openControlConnection settings = do
  result <- Conn.acquire settings
  case result of
    Left err ->
      throwDb $ "Failed to open control connection: " <> show err
    Right c -> pure (ControlConnection c)

-- | Release the underlying hasql connection.
closeControlConnection :: ControlConnection -> IO ()
closeControlConnection = Conn.release . unControlConnection

-- ---------------------------------------------------------------------------
-- * Read / write
-- ---------------------------------------------------------------------------

-- | Read the singleton row, or 'Nothing' if it has never been seeded.
readSyncState
  :: (HasCallStack, HasControlConnection env, MonadReader env m, MonadIO m)
  => m (Maybe SyncStateRow)
readSyncState = runCtrlStmt "readSyncState" () readSyncStateStmt

-- | Overwrite the consumer-owned columns of the singleton row.
-- Throws 'AppDatabaseError' if zero rows are affected (i.e. when
-- 'seedSyncState' was never called).
writeSyncState
  :: (HasCallStack, HasControlConnection env, MonadReader env m, MonadIO m)
  => SyncStateRow
  -> m ()
writeSyncState row = do
  n <- runCtrlStmt "writeSyncState" row writeSyncStateStmt
  expectOneRowAffected "writeSyncState" n

-- | Insert the singleton row with sensible defaults. Idempotent
-- (@ON CONFLICT DO NOTHING@). Must be invoked once after
-- 'DbSync.Db.Schema.Init.initSchema' creates the table.
seedSyncState
  :: (HasCallStack, HasControlConnection env, MonadReader env m, MonadIO m)
  => Int   -- ^ @schema_version_applied@
  -> Bool  -- ^ @ledger_enabled@
  -> m ()
seedSyncState schemaVersion ledgerEnabled =
  runCtrlStmt "seedSyncState"
    (fromIntegral schemaVersion, ledgerEnabled)
    seedSyncStateStmt

-- | Record that a ledger snapshot at the given slot has been
-- successfully written.
markSnapshotComplete
  :: (HasCallStack, HasControlConnection env, MonadReader env m, MonadIO m)
  => Word64
  -> m ()
markSnapshotComplete slotNo = do
  n <- runCtrlStmt "markSnapshotComplete" slotNo markSnapshotCompleteStmt
  expectOneRowAffected "markSnapshotComplete" n

-- | Flip @sync_complete@ to true at the Ingest → Follow boundary.
-- Subsequent boots take the Follow-restart path.
markSyncComplete
  :: (HasCallStack, HasControlConnection env, MonadReader env m, MonadIO m)
  => m ()
markSyncComplete = do
  n <- runCtrlStmt "markSyncComplete" () markSyncCompleteStmt
  expectOneRowAffected "markSyncComplete" n

-- | Read the pending rollback marker. 'Nothing' is the normal case.
readPendingRollbackSlot
  :: (HasCallStack, HasControlConnection env, MonadReader env m, MonadIO m)
  => m (Maybe Word64)
readPendingRollbackSlot =
  runCtrlStmt "readPendingRollbackSlot" () readPendingRollbackSlotStmt

-- | Persist a rollback target that must run on next boot.
writePendingRollbackSlot
  :: (HasCallStack, HasControlConnection env, MonadReader env m, MonadIO m)
  => Word64 -> m ()
writePendingRollbackSlot slot = do
  n <- runCtrlStmt "writePendingRollbackSlot" slot writePendingRollbackSlotStmt
  expectOneRowAffected "writePendingRollbackSlot" n

-- | Drop the marker after the recovery rollback has committed.
clearPendingRollbackSlot
  :: (HasCallStack, HasControlConnection env, MonadReader env m, MonadIO m)
  => m ()
clearPendingRollbackSlot = do
  n <- runCtrlStmt "clearPendingRollbackSlot" () clearPendingRollbackSlotStmt
  expectOneRowAffected "clearPendingRollbackSlot" n

-- ---------------------------------------------------------------------------
-- * Boot-time canonicalisation
-- ---------------------------------------------------------------------------

-- | Look up the header hash at a given slot in the @block@ table.
-- 'Nothing' means no committed block at that slot.
fetchBlockHashAtSlot
  :: (HasCallStack, HasControlConnection env, MonadReader env m, MonadIO m)
  => Word64
  -> m (Maybe ByteString)
fetchBlockHashAtSlot slot =
  runCtrlStmt "fetchBlockHashAtSlot" slot selectBlockHashAtSlotStmt

-- ---------------------------------------------------------------------------
-- * Dedup-store rebuild
-- ---------------------------------------------------------------------------

-- | Rebuild the dedup stores from the rows already committed to
-- PostgreSQL. Each store's counter is left pointing at
-- @max(existingId) + 1@ so subsequent 'lookupOrInsert' allocations
-- don't collide.
--
-- The supplied 'TableDef' list determines which tables are queried —
-- a dedup table absent from the active schema (e.g. @script@) is
-- silently skipped, leaving its store empty.
--
-- The 'LsmSession' is needed by 'newStores' to materialise the
-- five LSM tables. Restart-resume callers should pass the same
-- session that the consumer will use; the saved snapshots (if any)
-- carry the table contents from a prior run, and the PG repopulate
-- pass below only bumps the counters.
rebuildDedupMaps
  :: ( HasCallStack
     , HasTracer env
     , HasControlConnection env
     , MonadReader env m
     , MonadIO m
     )
  => [TableDef]
  -> LsmSession
  -> m DedupStores
rebuildDedupMaps tableDefs lsmSession = do
  stores <- liftIO (newStores lsmSession)
  let tableNames = map tdName tableDefs
      whenPresent name action =
        when (name `elem` tableNames) action
  whenPresent "slot_leader" $
    populateSingle "slot_leader" "hash" (dstSlotLeader stores)
  whenPresent "stake_address" $
    populateSingle "stake_address" "hash_raw" (dstStakeAddress stores)
  whenPresent "pool_hash" $
    populateSingle "pool_hash" "hash_raw" (dstPoolHash stores)
  whenPresent "multi_asset" $
    populateMultiAsset (dstMultiAsset stores)
  pure stores

populateSingle
  :: ( HasCallStack
     , HasTracer env
     , HasControlConnection env
     , MonadReader env m
     , MonadIO m
     )
  => Text -> Text -> DedupStore -> m ()
populateSingle tableName keyCol store =
  timedRebuild tableName $ do
    rows <- runCtrlStmt ("rebuildDedupMaps[" <> tableName <> "]") ()
              (selectDedupSingleStmt tableName keyCol)
    liftIO $ forM_ rows $ \(rowId, key) ->
      insertExisting (SBS.toShort key) rowId store
    pure (fromIntegral (length rows))

populateMultiAsset
  :: ( HasCallStack
     , HasTracer env
     , HasControlConnection env
     , MonadReader env m
     , MonadIO m
     )
  => DedupStore -> m ()
populateMultiAsset store =
  timedRebuild "multi_asset" $ do
    rows <- runCtrlStmt "rebuildDedupMaps[multi_asset]" ()
              selectMultiAssetDedupStmt
    liftIO $ forM_ rows $ \(rowId, policy, name) ->
      insertExisting (hashDedupKey (policy <> name)) rowId store
    pure (fromIntegral (length rows))

-- | Wrap one table's repopulation in start/end trace lines and time
-- the inner action. The returned row count from the action is
-- formatted into the completion line.
timedRebuild
  :: (HasTracer env, MonadReader env m, MonadIO m)
  => Text -> m Int64 -> m ()
timedRebuild tableName action = do
  tracer <- asks getTracer
  liftIO $ traceWith tracer $ LogMsg Info "DedupRebuild"
    (tableName <> ": loading") Nothing
  start <- liftIO getCurrentTime
  rows  <- action
  end   <- liftIO getCurrentTime
  liftIO $ traceWith tracer $ LogMsg Info "DedupRebuild" (
      tableName <> ": " <> fmtCount rows <> " rows in "
        <> fmtDuration (realToFrac (diffUTCTime end start))
    ) Nothing

-- ---------------------------------------------------------------------------
-- * Internal: statement runner
-- ---------------------------------------------------------------------------

-- | Run a 'Stmt.Statement' against the env's control connection;
-- lift any 'SessionError' into 'AppDatabaseError'.
runCtrlStmt
  :: (HasCallStack, HasControlConnection env, MonadReader env m, MonadIO m)
  => Text
  -> p
  -> Stmt.Statement p r
  -> m r
runCtrlStmt callerName params stmt = do
  ControlConnection conn <- asks getControlConnection
  result <- liftIO $ Conn.use conn (Sess.statement params stmt)
  case result of
    Left err -> throwDb $ callerName <> ": " <> show err
    Right r  -> pure r

-- | Throw a uniform diagnostic when an UPDATE\/INSERT didn't affect
-- exactly one row (the singleton-row invariant).
expectOneRowAffected
  :: (HasCallStack, MonadIO m) => Text -> Int64 -> m ()
expectOneRowAffected callerName = \case
  1 -> pure ()
  n ->
    throwDb $
      callerName
        <> ": UPDATE affected "
        <> show n
        <> " rows, expected exactly 1. Did seedSyncState run?"


