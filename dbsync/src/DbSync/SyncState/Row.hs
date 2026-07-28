-- | Thin hasql wrapper around the @dbsync_sync_state@ singleton row.
--
-- Owns connection lifecycle and IO-level error mapping. The schema
-- type 'SyncStateRow', encoders\/decoders, and 'Statement' bindings
-- live in @dbsync-db@ and are re-exported here for convenience.
--
-- libpq handles the loader-stream transport in
-- 'DbSync.Db.Loader.Connection'; the control-plane path here goes
-- through hasql.
module DbSync.SyncState.Row
  ( -- * Row type (re-export from dbsync-db)
    SyncStateRow (..)

    -- * Connection lifecycle
  , ControlConnection (..)
  , HasControlConnection (..)
  , openControlConnection
  , closeControlConnection

    -- * Read \/ write
  , readSyncState
  , readNetwork
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

    -- * Resume-time cache populate
  , populateCostModelCache
  , populateGovActionProposalCache

    -- * Governance enactment lookups
  , queryCommitteeByProposal
  , queryConstitutionByProposal

    -- * Governance status-column updates
  , markGovActionRatified
  , markGovActionEnacted
  , markGovActionDropped
  , markGovActionExpired
  ) where

import Cardano.Prelude

import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Hasql.Connection as Conn
import qualified Hasql.Connection.Settings as Settings
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import Control.Tracer (traceWith)

import DbSync.Db.Schema.Governance
  ( committeeHashTableDef
  , drepHashTableDef
  , votingAnchorTableDef
  )
import DbSync.Db.Schema.MultiAsset (multiAssetTableDef)
import DbSync.Db.Schema.Core (PoolHashCols (..), poolHashCols, poolHashTableDef)
import DbSync.Db.Schema.ScriptsDatums
  ( DatumCols (..)
  , RedeemerDataCols (..)
  , ScriptCols (..)
  , datumCols
  , datumTableDef
  , redeemerDataCols
  , redeemerDataTableDef
  , scriptCols
  , scriptTableDef
  )
import DbSync.Db.Schema.Core
  ( StakeAddressCols (..)
  , stakeAddressCols
  , stakeAddressTableDef
  )
import DbSync.Db.Schema.SyncState (SyncStateRow (..))
import DbSync.Db.Schema.Types (TableColumn (..), TableDef (..))
import DbSync.Schema.Version (Fingerprint (..))
import DbSync.Db.Statement.Worker.Resume
  ( declareDedupSingleCursorStmt
  , declareMultiAssetDedupCursorStmt
  , fetchDedupSinglePageStmt
  , fetchMultiAssetDedupPageStmt
  , selectBlockHashAtSlotStmt
  , selectCommitteeByProposalStmt
  , selectCommitteeHashDedupStmt
  , selectConstitutionByProposalStmt
  , selectDedupSingleStmt
  , selectDrepHashDedupStmt
  , selectGovActionProposalCacheStmt
  , selectVotingAnchorDedupStmt
  , updateGovActionDroppedStmt
  , updateGovActionEnactedStmt
  , updateGovActionExpiredStmt
  , updateGovActionRatifiedStmt
  )
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Timing (fmtCount, fmtDuration)
import DbSync.Trace.Types (LogMsg (..), Severity (..))
import DbSync.Db.Statement.SyncState
  ( clearPendingRollbackSlotStmt
  , markSnapshotCompleteStmt
  , markSyncCompleteStmt
  , readNetworkStmt
  , readPendingRollbackSlotStmt
  , readSyncStateStmt
  , seedSyncStateStmt
  , writePendingRollbackSlotStmt
  , writeSyncStateStmt
  )
import DbSync.Db.Statement.Transaction (beginSql, commitSql)
import DbSync.Error (throwDb)
import DbSync.Phase.Ingest.DedupStore
  ( DedupStore
  , DedupStores (..)
  , insertExisting
  , newStores
  )
import DbSync.Phase.Ingest.LsmSession (LsmSession)
import DbSync.Util.DedupHash
  ( committeeHashDedupKey
  , drepHashDedupKey
  , encodeVotingAnchorKey
  , hashDedupKey
  )

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
openControlConnection :: Settings.Settings -> IO ControlConnection
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
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => m (Maybe SyncStateRow)
readSyncState = runCtrlStmt "readSyncState" () readSyncStateStmt

-- | The @(network_magic, network_name)@ pair recorded when the
-- singleton was seeded. 'Nothing' if it has never been seeded.
readNetwork
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => m (Maybe (Int64, Text))
readNetwork = runCtrlStmt "readNetwork" () readNetworkStmt

-- | Overwrite the consumer-owned columns of the singleton row.
-- Throws 'AppDatabaseError' if zero rows are affected (i.e. when
-- 'seedSyncState' was never called).
writeSyncState
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => SyncStateRow
  -> m ()
writeSyncState row = do
  n <- runCtrlStmt "writeSyncState" row writeSyncStateStmt
  expectOneRowAffected "writeSyncState" n

-- | Insert the singleton row with sensible defaults. Idempotent
-- (@ON CONFLICT DO NOTHING@). Must be invoked once after
-- 'DbSync.Db.Schema.Init.initSchema' creates the table.
seedSyncState
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => Int          -- ^ @schema_version_applied@
  -> Fingerprint  -- ^ @schema_fingerprint@
  -> Bool         -- ^ @ledger_enabled@
  -> [Text]       -- ^ enabled extractor names (@extractors@)
  -> Word32       -- ^ @network_magic@
  -> Text         -- ^ @network_name@
  -> m ()
seedSyncState schemaVersion fingerprint ledgerEnabled extractorNames networkMagic networkName =
  runCtrlStmt "seedSyncState"
    ( fromIntegral schemaVersion
    , unFingerprint fingerprint
    , ledgerEnabled
    , extractorNames
    , fromIntegral networkMagic
    , networkName
    )
    seedSyncStateStmt

-- | Record that a ledger snapshot at the given slot has been
-- successfully written.
markSnapshotComplete
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => Word64
  -> m ()
markSnapshotComplete slotNo = do
  n <- runCtrlStmt "markSnapshotComplete" slotNo markSnapshotCompleteStmt
  expectOneRowAffected "markSnapshotComplete" n

-- | Flip @sync_complete@ to true at the Ingest → Follow boundary.
-- Subsequent boots take the Follow-restart path.
markSyncComplete
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => m ()
markSyncComplete = do
  n <- runCtrlStmt "markSyncComplete" () markSyncCompleteStmt
  expectOneRowAffected "markSyncComplete" n

-- | Read the pending rollback marker. 'Nothing' is the normal case.
readPendingRollbackSlot
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => m (Maybe Word64)
readPendingRollbackSlot =
  runCtrlStmt "readPendingRollbackSlot" () readPendingRollbackSlotStmt

-- | Persist a rollback target that must run on next boot.
writePendingRollbackSlot
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => Word64 -> m ()
writePendingRollbackSlot slot = do
  n <- runCtrlStmt "writePendingRollbackSlot" slot writePendingRollbackSlotStmt
  expectOneRowAffected "writePendingRollbackSlot" n

-- | Drop the marker after the recovery rollback has committed.
clearPendingRollbackSlot
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
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
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => Word64
  -> m (Maybe ByteString)
fetchBlockHashAtSlot slot =
  runCtrlStmt "fetchBlockHashAtSlot" slot selectBlockHashAtSlotStmt

-- | @committee.id@ that originated from the given proposal id, or
-- the genesis row when the input is 'Nothing'.
queryCommitteeByProposal
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => Maybe Int64
  -> m (Maybe Int64)
queryCommitteeByProposal mProposalId =
  runCtrlStmt "queryCommitteeByProposal" mProposalId selectCommitteeByProposalStmt

-- | @constitution.id@ that originated from the given proposal id, or
-- the genesis row when the input is 'Nothing'.
queryConstitutionByProposal
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => Maybe Int64
  -> m (Maybe Int64)
queryConstitutionByProposal mProposalId =
  runCtrlStmt "queryConstitutionByProposal" mProposalId selectConstitutionByProposalStmt

-- | Set @gov_action_proposal.ratified_epoch@ on the given row.
markGovActionRatified
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => Int64 -> Word64 -> m ()
markGovActionRatified gid epoch =
  runCtrlStmt "markGovActionRatified" (gid, epoch) updateGovActionRatifiedStmt

-- | Set @gov_action_proposal.enacted_epoch@ on the given row.
markGovActionEnacted
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => Int64 -> Word64 -> m ()
markGovActionEnacted gid epoch =
  runCtrlStmt "markGovActionEnacted" (gid, epoch) updateGovActionEnactedStmt

-- | Set @gov_action_proposal.dropped_epoch@ on the given row.
markGovActionDropped
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => Int64 -> Word64 -> m ()
markGovActionDropped gid epoch =
  runCtrlStmt "markGovActionDropped" (gid, epoch) updateGovActionDroppedStmt

-- | Set @gov_action_proposal.expired_epoch@ on the given row.
markGovActionExpired
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => Int64 -> Word64 -> m ()
markGovActionExpired gid epoch =
  runCtrlStmt "markGovActionExpired" (gid, epoch) updateGovActionExpiredStmt

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
  :: ( HasTracer env
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
  whenPresent (tdName stakeAddressTableDef) $
    populateSingle (tdName stakeAddressTableDef) stakeAddressCols.sacHashRaw.tcName (dstStakeAddress stores)
  whenPresent (tdName poolHashTableDef) $
    populateSingle (tdName poolHashTableDef) poolHashCols.phcHashRaw.tcName (dstPoolHash stores)
  whenPresent (tdName multiAssetTableDef) $
    populateMultiAsset (dstMultiAsset stores)
  whenPresent (tdName scriptTableDef) $
    populateSingle (tdName scriptTableDef) scriptCols.sccHash.tcName (dstScriptHash stores)
  whenPresent (tdName datumTableDef) $
    populateSingle (tdName datumTableDef) datumCols.dmcHash.tcName (dstDatum stores)
  whenPresent (tdName redeemerDataTableDef) $
    populateSingle (tdName redeemerDataTableDef) redeemerDataCols.rddcHash.tcName (dstRedeemerData stores)
  whenPresent (tdName drepHashTableDef) $
    populateDrepHash (dstDrepHash stores)
  whenPresent (tdName committeeHashTableDef) $
    populateCommitteeHash (dstCommitteeHash stores)
  whenPresent (tdName votingAnchorTableDef) $
    populateVotingAnchor (dstVotingAnchor stores)
  pure stores

-- | Page size for the boot-time dedup rebuild. Each page is read,
-- inserted into the LSM store, then released before the next, so peak
-- memory stays flat regardless of the table's size.
dedupPageSize :: Int64
dedupPageSize = 50000

-- | Stream a whole dedup table through a server-side cursor, inserting
-- each fetched page before requesting the next. One sequential scan in
-- physical order — no @ORDER BY@ and no @id@ index — with at most one
-- page resident. The scan runs in a single transaction; boot is
-- single-threaded on the control connection, so holding it open is safe.
cursorInsert
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => Text                  -- ^ caller label for errors
  -> Stmt.Statement () ()  -- ^ DECLARE … CURSOR
  -> Stmt.Statement () [r] -- ^ FETCH FORWARD …
  -> (r -> IO ())          -- ^ insert one fetched row
  -> m Int64
cursorInsert callerName declareStmt fetchStmt insertRow = do
  ControlConnection conn <- asks getControlConnection
  result <- liftIO $ Conn.use conn $ do
    Sess.script beginSql
    Sess.statement () declareStmt
    let loop acc = do
          rows <- Sess.statement () fetchStmt
          if null rows
            then pure acc
            else do
              liftIO (forM_ rows insertRow)
              loop $! acc + fromIntegral (length rows)
    total <- loop (0 :: Int64)
    Sess.script commitSql
    pure total
  case result of
    Left err -> throwDb $ callerName <> ": " <> show err
    Right n -> pure n

-- | Cursor name for a table's dedup rebuild. Each rebuild runs in its
-- own transaction, so a per-table name never collides.
dedupCursorName :: Text -> Text
dedupCursorName tableName = "dedup_cur_" <> tableName

populateSingle
  :: ( HasTracer env
     , HasControlConnection env
     , MonadReader env m
     , MonadIO m
     )
  => Text -> Text -> DedupStore -> m ()
populateSingle tableName keyCol store =
  timedRebuild tableName $
    cursorInsert
      ("rebuildDedupMaps[" <> tableName <> "]")
      (declareDedupSingleCursorStmt (dedupCursorName tableName) tableName keyCol)
      (fetchDedupSinglePageStmt (dedupCursorName tableName) dedupPageSize)
      (\(rowId, key) -> insertExisting (SBS.toShort key) rowId store)

populateMultiAsset
  :: ( HasTracer env
     , HasControlConnection env
     , MonadReader env m
     , MonadIO m
     )
  => DedupStore -> m ()
populateMultiAsset store =
  timedRebuild "multi_asset" $
    cursorInsert
      "rebuildDedupMaps[multi_asset]"
      (declareMultiAssetDedupCursorStmt (dedupCursorName "multi_asset"))
      (fetchMultiAssetDedupPageStmt (dedupCursorName "multi_asset") dedupPageSize)
      (\(rowId, policy, name) -> insertExisting (hashDedupKey (policy <> name)) rowId store)

populateDrepHash
  :: ( HasTracer env
     , HasControlConnection env
     , MonadReader env m
     , MonadIO m
     )
  => DedupStore -> m ()
populateDrepHash store =
  timedRebuild "drep_hash" $ do
    rows <- runCtrlStmt "rebuildDedupMaps[drep_hash]" ()
              selectDrepHashDedupStmt
    liftIO $ forM_ rows $ \(rowId, mRaw, view, _hasScript) ->
      insertExisting (SBS.toShort (drepHashDedupKey mRaw view)) rowId store
    pure (fromIntegral (length rows))

populateCommitteeHash
  :: ( HasTracer env
     , HasControlConnection env
     , MonadReader env m
     , MonadIO m
     )
  => DedupStore -> m ()
populateCommitteeHash store =
  timedRebuild "committee_hash" $ do
    rows <- runCtrlStmt "rebuildDedupMaps[committee_hash]" ()
              selectCommitteeHashDedupStmt
    liftIO $ forM_ rows $ \(rowId, raw, hasScript) ->
      insertExisting (SBS.toShort (committeeHashDedupKey raw hasScript)) rowId store
    pure (fromIntegral (length rows))

populateVotingAnchor
  :: ( HasTracer env
     , HasControlConnection env
     , MonadReader env m
     , MonadIO m
     )
  => DedupStore -> m ()
populateVotingAnchor store =
  timedRebuild "voting_anchor" $ do
    rows <- runCtrlStmt "rebuildDedupMaps[voting_anchor]" ()
              selectVotingAnchorDedupStmt
    liftIO $ forM_ rows $ \(rowId, url, dataHash, anchorType) ->
      insertExisting
        (SBS.toShort (encodeVotingAnchorKey url dataHash anchorType))
        rowId store
    pure (fromIntegral (length rows))

-- ---------------------------------------------------------------------------
-- * Resume-time cache populate
-- ---------------------------------------------------------------------------

-- | Seed the in-process cost_model dedup cache from PG so a resumed
-- Ingest that re-encounters a known cost model finds the existing
-- row id rather than allocating a fresh one (and conflicting on the
-- UNIQUE(hash) at the LOGGED flip).
--
-- Skipped when @cost_model@ is absent from the active schema —
-- callers run this unconditionally; the empty result is harmless.
populateCostModelCache
  :: ( HasTracer env
     , HasControlConnection env
     , MonadReader env m
     , MonadIO m
     )
  => [TableDef]
  -> m (Map ByteString Int64)
populateCostModelCache tableDefs
  | "cost_model" `notElem` map tdName tableDefs = pure Map.empty
  | otherwise = do
      tracer <- asks getTracer
      liftIO $ traceWith tracer $ LogMsg Info "DedupRebuild"
        "cost_model: loading"
      start <- liftIO getCurrentTime
      rows  <- runCtrlStmt "populateCostModelCache" ()
                 (selectDedupSingleStmt "cost_model" "hash")
      end   <- liftIO getCurrentTime
      let !cache = Map.fromList [(hash, rowId) | (rowId, hash) <- rows]
      liftIO $ traceWith tracer $ LogMsg Info "DedupRebuild" (
          "cost_model: " <> fmtCount (fromIntegral (Map.size cache) :: Int64)
            <> " rows in "
            <> fmtDuration (realToFrac (diffUTCTime end start))
        )
      pure cache

-- | Seed @(tx_hash, proposal_index) -> gov_action_proposal.id@ from PG
-- so vote rows landing in resumed blocks can resolve their proposal
-- targets without a SELECT round-trip.
populateGovActionProposalCache
  :: ( HasTracer env
     , HasControlConnection env
     , MonadReader env m
     , MonadIO m
     )
  => [TableDef]
  -> m (Map (ByteString, Word64) Int64)
populateGovActionProposalCache tableDefs
  | "gov_action_proposal" `notElem` map tdName tableDefs = pure Map.empty
  | otherwise = do
      tracer <- asks getTracer
      liftIO $ traceWith tracer $ LogMsg Info "DedupRebuild"
        "gov_action_proposal cache: loading"
      start <- liftIO getCurrentTime
      rows  <- runCtrlStmt "populateGovActionProposalCache" ()
                 selectGovActionProposalCacheStmt
      end   <- liftIO getCurrentTime
      let !cache = Map.fromList [((h, ix), rid) | (h, ix, rid) <- rows]
      liftIO $ traceWith tracer $ LogMsg Info "DedupRebuild" (
          "gov_action_proposal cache: "
            <> fmtCount (fromIntegral (Map.size cache) :: Int64)
            <> " rows in "
            <> fmtDuration (realToFrac (diffUTCTime end start))
        )
      pure cache

-- | Wrap one table's repopulation in start/end trace lines and time
-- the inner action. The returned row count from the action is
-- formatted into the completion line.
timedRebuild
  :: (HasTracer env, MonadReader env m, MonadIO m)
  => Text -> m Int64 -> m ()
timedRebuild tableName action = do
  tracer <- asks getTracer
  liftIO $ traceWith tracer $ LogMsg Info "DedupRebuild"
    (tableName <> ": loading")
  start <- liftIO getCurrentTime
  rows  <- action
  end   <- liftIO getCurrentTime
  liftIO $ traceWith tracer $ LogMsg Info "DedupRebuild" (
      tableName <> ": " <> fmtCount rows <> " rows in "
        <> fmtDuration (realToFrac (diffUTCTime end start))
    )

-- ---------------------------------------------------------------------------
-- * Internal: statement runner
-- ---------------------------------------------------------------------------

-- | Run a 'Stmt.Statement' against the env's control connection;
-- lift any 'SessionError' into 'AppDatabaseError'.
runCtrlStmt
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
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
  :: MonadIO m => Text -> Int64 -> m ()
expectOneRowAffected callerName = \case
  1 -> pure ()
  n ->
    throwDb $
      callerName
        <> ": UPDATE affected "
        <> show n
        <> " rows, expected exactly 1. Did seedSyncState run?"


