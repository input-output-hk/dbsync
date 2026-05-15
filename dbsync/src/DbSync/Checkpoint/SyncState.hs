-- | Thin hasql wrapper around the @dbsync_sync_state@ singleton row.
--
-- Owns connection lifecycle and IO-level error mapping. The schema
-- type 'SyncStateRow', encoders\/decoders, and 'Statement' bindings
-- live in @dbsync-db@ (re-exported here so existing call sites don't
-- need a new import).
--
-- libpq remains for COPY streaming in 'DbSync.Copy.Connection'; the
-- control-plane path here goes through hasql.
module DbSync.Checkpoint.SyncState
  ( -- * Row type (re-export from dbsync-db)
    SyncStateRow (..)

    -- * Connection lifecycle
  , ControlConnection (..)
  , openControlConnection
  , closeControlConnection

    -- * Read \/ write
  , readSyncState
  , writeSyncState
  , seedSyncState
  , markSnapshotComplete
  , markSyncComplete

    -- * Boot-time canonicalisation
  , fetchBlockHashAtSlot

    -- * Dedup map rebuild (currently a stub)
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
import DbSync.Trace.Timing (fmtDuration, fmtRows)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))
import DbSync.Db.Statement.SyncState
  ( markSnapshotCompleteStmt
  , markSyncCompleteStmt
  , readSyncStateStmt
  , seedSyncStateStmt
  , writeSyncStateStmt
  )
import DbSync.Error (throwDb)
import DbSync.Id.DedupMap
  ( DedupMap
  , DedupMaps (..)
  , insertExisting
  , newMaps
  )
import DbSync.Util.DedupHash (hashDedupKey)

-- ---------------------------------------------------------------------------
-- * Connection lifecycle
-- ---------------------------------------------------------------------------

-- | A hasql connection dedicated to non-COPY operations: sync-state
-- read\/write, dedup-map rebuild, resume-time row cleanup.
newtype ControlConnection = ControlConnection
  { unControlConnection :: Conn.Connection
  }

-- | Open a fresh 'ControlConnection'. Throws 'AppDatabaseError' on
-- handshake failure.
--
-- Settings are built by callers via the @Hasql.Connection.Settings@
-- helpers (@hostAndPort@, @user@, @password@, @dbname@) combined with
-- 'mconcat'.
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
readSyncState :: HasCallStack => ControlConnection -> IO (Maybe SyncStateRow)
readSyncState ctrl = runStmt "readSyncState" ctrl () readSyncStateStmt

-- | Overwrite the consumer-owned columns of the singleton row.
-- Throws 'AppDatabaseError' if zero rows are affected (i.e. when
-- 'seedSyncState' was never called).
writeSyncState :: HasCallStack => ControlConnection -> SyncStateRow -> IO ()
writeSyncState ctrl row = do
  n <- runStmt "writeSyncState" ctrl row writeSyncStateStmt
  expectOneRowAffected "writeSyncState" n

-- | Insert the singleton row with sensible defaults. Idempotent
-- (@ON CONFLICT DO NOTHING@). Must be invoked once after
-- 'DbSync.Db.Schema.Init.initSchema' creates the table.
seedSyncState
  :: HasCallStack
  => ControlConnection
  -> Int   -- ^ @schema_version_applied@
  -> Bool  -- ^ @ledger_enabled@
  -> IO ()
seedSyncState ctrl schemaVersion ledgerEnabled =
  runStmt "seedSyncState" ctrl
    (fromIntegral schemaVersion, ledgerEnabled)
    seedSyncStateStmt

-- | Record that a ledger snapshot at the given slot has been
-- successfully written. Owned by the snapshot-writer thread; the
-- consumer thread does not call this.
markSnapshotComplete :: HasCallStack => ControlConnection -> Word64 -> IO ()
markSnapshotComplete ctrl slotNo = do
  n <- runStmt "markSnapshotComplete" ctrl slotNo markSnapshotCompleteStmt
  expectOneRowAffected "markSnapshotComplete" n

-- | Flip @sync_complete@ to true at the Ingest → Follow boundary.
-- Subsequent boots take the fast path.
markSyncComplete :: HasCallStack => ControlConnection -> IO ()
markSyncComplete ctrl = do
  n <- runStmt "markSyncComplete" ctrl () markSyncCompleteStmt
  expectOneRowAffected "markSyncComplete" n

-- ---------------------------------------------------------------------------
-- * Boot-time canonicalisation
-- ---------------------------------------------------------------------------

-- | Look up the header hash at a given slot in the @block@ table.
-- 'Nothing' means no committed block at that slot (PG wiped,
-- chain rolled back during downtime, etc.). Used by the boot flow
-- to canonicalise snapshot intersection candidates.
fetchBlockHashAtSlot
  :: HasCallStack
  => ControlConnection
  -> Word64
  -> IO (Maybe ByteString)
fetchBlockHashAtSlot ctrl slot =
  runStmt "fetchBlockHashAtSlot" ctrl slot selectBlockHashAtSlotStmt

-- ---------------------------------------------------------------------------
-- * Dedup-map rebuild
-- ---------------------------------------------------------------------------

-- | Rebuild the in-memory dedup maps from the rows already
-- committed to PostgreSQL. Each map's counter is left pointing at
-- @max(existingId) + 1@ so subsequent 'lookupOrInsert' allocations
-- don't collide.
--
-- The supplied 'TableDef' list determines which tables are queried —
-- a dedup table absent from the active schema (e.g. @script@) is
-- silently skipped, leaving its map empty.
rebuildDedupMaps :: HasCallStack => AppTracer -> ControlConnection -> [TableDef] -> IO DedupMaps
rebuildDedupMaps tracer ctrl tableDefs = do
  maps <- newMaps
  let tableNames = map tdName tableDefs
      whenPresent name action =
        when (name `elem` tableNames) action
  whenPresent "slot_leader" $
    populateSingle tracer ctrl "slot_leader" "hash" (dmsSlotLeader maps)
  whenPresent "stake_address" $
    populateSingle tracer ctrl "stake_address" "hash_raw" (dmsStakeAddress maps)
  whenPresent "pool_hash" $
    populateSingle tracer ctrl "pool_hash" "hash_raw" (dmsPoolHash maps)
  whenPresent "multi_asset" $
    populateMultiAsset tracer ctrl (dmsMultiAsset maps)
  pure maps

-- | Populate a dedup map whose natural key is a single column.
populateSingle :: HasCallStack => AppTracer -> ControlConnection -> Text -> Text -> DedupMap -> IO ()
populateSingle tracer ctrl tableName keyCol dm =
  timedRebuild tracer tableName $ do
    rows <- runStmt ("rebuildDedupMaps[" <> tableName <> "]") ctrl ()
              (selectDedupSingleStmt tableName keyCol)
    forM_ rows $ \(rowId, key) ->
      insertExisting (SBS.toShort key) rowId dm
    pure (fromIntegral (length rows))

-- | Populate the multi-asset dedup map. Keys must match the form
-- written by 'DbSync.Extractor.SharedDedup.resolveAndWriteMultiAsset'
-- (Blake2b-224 of @policy ++ name@), otherwise a resumed run will
-- allocate fresh ids for already-known assets.
populateMultiAsset :: HasCallStack => AppTracer -> ControlConnection -> DedupMap -> IO ()
populateMultiAsset tracer ctrl dm =
  timedRebuild tracer "multi_asset" $ do
    rows <- runStmt "rebuildDedupMaps[multi_asset]" ctrl ()
              selectMultiAssetDedupStmt
    forM_ rows $ \(rowId, policy, name) ->
      insertExisting (hashDedupKey (policy <> name)) rowId dm
    pure (fromIntegral (length rows))

-- | Wrap one table's dedup-map repopulation in start/end trace lines
-- so the operator can see which table is in flight and how long each
-- took. The action returns the number of rows loaded; the @SELECT@
-- itself is the slow part, so the timer captures fetch + insert
-- together rather than splitting them.
timedRebuild :: AppTracer -> Text -> IO Int64 -> IO ()
timedRebuild tracer tableName action = do
  traceWith tracer $ LogMsg Info "DedupRebuild"
    (tableName <> ": loading") Nothing
  start <- getCurrentTime
  rows  <- action
  end   <- getCurrentTime
  traceWith tracer $ LogMsg Info "DedupRebuild" (
      tableName <> ": " <> fmtRows rows <> " rows in "
        <> fmtDuration (realToFrac (diffUTCTime end start))
    ) Nothing

-- ---------------------------------------------------------------------------
-- * Internal: statement runner
-- ---------------------------------------------------------------------------

-- | Run a 'Stmt.Statement' in a single-shot session and lift any
-- 'SessionError' into 'AppDatabaseError'.
runStmt
  :: HasCallStack
  => Text
  -> ControlConnection
  -> p
  -> Stmt.Statement p r
  -> IO r
runStmt callerName (ControlConnection conn) params stmt = do
  result <- Conn.use conn (Sess.statement params stmt)
  case result of
    Left err -> throwDb $ callerName <> ": " <> show err
    Right r  -> pure r

-- | Throw a uniform diagnostic when an UPDATE\/INSERT didn't affect
-- exactly one row (the singleton-row invariant).
expectOneRowAffected :: HasCallStack => Text -> Int64 -> IO ()
expectOneRowAffected callerName = \case
  1 -> pure ()
  n ->
    throwDb $
      callerName
        <> ": UPDATE affected "
        <> show n
        <> " rows, expected exactly 1. Did seedSyncState run?"
