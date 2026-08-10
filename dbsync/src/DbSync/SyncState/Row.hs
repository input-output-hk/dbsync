-- | Thin hasql wrapper around the @dbsync_sync_state@ singleton row.
-- Owns the control connection and maps IO-level errors. The schema
-- type, codecs and 'Statement' bindings live in @dbsync-db@.
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

class HasControlConnection env where
  getControlConnection :: env -> ControlConnection

-- | Self-instance so boot-time IO code can drive the polymorphic
-- helpers via @runAppM ctrlConn ...@ without building a phase env.
instance HasControlConnection ControlConnection where
  getControlConnection = identity

-- | Throws 'AppDatabaseError' on handshake failure.
openControlConnection :: Settings.Settings -> IO ControlConnection
openControlConnection settings = do
  result <- Conn.acquire settings
  case result of
    Left err ->
      throwDb $ "Failed to open control connection: " <> show err
    Right c -> pure (ControlConnection c)

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
-- Throws 'AppDatabaseError' when it affects zero rows, which means
-- nobody called 'seedSyncState'.
writeSyncState
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => SyncStateRow
  -> m ()
writeSyncState row = do
  n <- runCtrlStmt "writeSyncState" row writeSyncStateStmt
  expectOneRowAffected "writeSyncState" n

-- | Insert the singleton row with defaults. Idempotent through
-- @ON CONFLICT DO NOTHING@. Call this once, after
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

markGovActionRatified
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => Int64 -> Word64 -> m ()
markGovActionRatified gid epoch =
  runCtrlStmt "markGovActionRatified" (gid, epoch) updateGovActionRatifiedStmt

markGovActionEnacted
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => Int64 -> Word64 -> m ()
markGovActionEnacted gid epoch =
  runCtrlStmt "markGovActionEnacted" (gid, epoch) updateGovActionEnactedStmt

markGovActionDropped
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => Int64 -> Word64 -> m ()
markGovActionDropped gid epoch =
  runCtrlStmt "markGovActionDropped" (gid, epoch) updateGovActionDroppedStmt

markGovActionExpired
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => Int64 -> Word64 -> m ()
markGovActionExpired gid epoch =
  runCtrlStmt "markGovActionExpired" (gid, epoch) updateGovActionExpiredStmt

-- ---------------------------------------------------------------------------
-- * Dedup-store rebuild
-- ---------------------------------------------------------------------------

-- | Rebuild the dedup stores from the rows PostgreSQL already holds.
-- Each store's counter ends at @max(existingId) + 1@, so a later
-- 'lookupOrInsert' allocation cannot collide. A dedup table missing
-- from the 'TableDef' list stays empty.
--
-- Pass the same 'LsmSession' the consumer will use: its snapshots
-- carry the table contents, and this pass only bumps the counters.
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

-- | Rows per page in the boot-time dedup rebuild. 'cursorInsert'
-- releases each page before it fetches the next, so peak memory stays
-- flat whatever the table size.
dedupPageSize :: Int64
dedupPageSize = 50000

-- | Stream a dedup table through a server-side cursor and insert each
-- page before requesting the next. The scan runs in physical order,
-- with no @ORDER BY@ and no @id@ index. Boot is single-threaded on
-- the control connection, so holding one transaction open is safe.
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

-- | Each rebuild runs in its own transaction, so a per-table name
-- never collides.
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

-- | Seed the in-process cost_model dedup cache from PG. A resumed
-- Ingest that meets a known cost model then finds the existing row
-- id, instead of allocating a fresh one and conflicting on the
-- UNIQUE(hash) at the LOGGED flip. Returns an empty map when
-- @cost_model@ is absent from the active schema.
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

-- | Seed @(tx_hash, proposal_index) -> gov_action_proposal.id@ from
-- PG, so vote rows in resumed blocks resolve their proposal target
-- without a SELECT round-trip.
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

-- | Wrap one table's repopulation in start and end trace lines. The
-- action returns the row count for the completion line.
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

-- | Lifts any 'SessionError' into 'AppDatabaseError'.
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

-- | Enforce the singleton-row invariant: an UPDATE or INSERT must
-- affect exactly one row.
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


