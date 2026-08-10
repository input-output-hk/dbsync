{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Resume-time row cleanup. Delete the rows that sit past the
-- recorded @last_committed_slot@, so a restart can re-process the
-- blocks above it. 'CleanupMode' selects which passes run.
module DbSync.SyncState.Resume
  ( CleanupMode (..)
  , deleteRowsPastSlot
  ) where

import Cardano.Prelude

import Control.Tracer (traceWith)
import Data.List (lookup)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt
import Text.Printf (printf)

import DbSync.SyncState.Row
  ( ControlConnection (..)
  , HasControlConnection (..)
  , SyncStateRow (..)
  )
import qualified DbSync.Db.Schema.Core as Core
import DbSync.Db.Schema.SyncState (idCounterByTable)
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , ParentRef (..)
  , TableColumn (..)
  , TableDef (..)
  , childrenOf
  )
import qualified DbSync.Db.Schema.UTxO as UTxO
import DbSync.Db.Statement.Worker.EpochAnchor
  ( EpochAnchor
  , deleteEpochRowsStmt
  , epochAnchorFor
  )
import DbSync.Db.Statement.Worker.Resume
  ( deleteByBlockSlotStmt
  , deleteByIdCounterStmt
  , deleteByParentCounterStmt
  , deleteBySlotStmt
  , deleteByTxFkSlotStmt
  , deleteByTxOutFkSlotStmt
  , selectEpochNoAtSlotStmt
  )
import DbSync.Error (throwDb)
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Timing (fmtCount, fmtDuration, withHeartbeatIO)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- | Which boot scenario the cleanup runs under.
data CleanupMode
  = IngestResume
    -- ^ Run every pass. The COPY writer commits at epoch boundaries
    -- and the 'SyncStateRow' counters lag by one epoch, so rows can
    -- sit past both the committed slot and the counters.
  | FollowRestart
    -- ^ Skip the counter pass. Follow commits one block per
    -- transaction, so no anchored table holds rows past the committed
    -- slot, and 'writeSyncStateSlotStmt' leaves the counters stale —
    -- the counter DELETE would wipe live rows.
  deriving stock (Eq, Show)

-- | No-op when the row reports no committed progress.
deleteRowsPastSlot
  :: ( HasTracer env
     , HasControlConnection env
     , MonadReader env m
     , MonadIO m
     )
  => CleanupMode
  -> [TableDef]
  -> SyncStateRow
  -> m Int64
deleteRowsPastSlot mode tableDefs row =
  case ssrLastCommittedSlot row of
    Nothing -> pure 0
    Just slotNo -> do
      tracer <- asks getTracer
      let classified  = map (\td -> (td, classify td)) tableDefs
          byBlockId   = [ td        | (td, sh) <- classified
                                    , Just HasBlockId  <- [csSlotBlock sh] ]
          bySlot      = [ td        | (td, sh) <- classified
                                    , Just HasSlotNo   <- [csSlotBlock sh] ]
          byTxFk      = [ (td, col) | (td, sh) <- classified
                                    , Just (HasTxFk col) <- [csSlotBlock sh] ]
          byTxOutFk   = [ (td, col) | (td, sh) <- classified
                                    , Just (HasTxOutFk col) <- [csSlotBlock sh] ]
          byEpoch     = [ (td, c, a) | (td, sh) <- classified
                                     , Just (HasEpochAnchor c a) <- [csSlotBlock sh] ]
          byIdCounter = [ (td, ctr) | (td, sh) <- classified
                                    , Just ctr <- [csIdCounter sh] ]
          -- Neither an anchor pass nor a counter of their own reaches
          -- these; a counter-tracked parent is their only route to the
          -- cutoff.
          unanchored  = [ td        | (td, sh) <- classified
                                    , Nothing <- [csSlotBlock sh]
                                    , Nothing <- [csIdCounter sh] ]

      emit tracer $ "starting (cutoff slot > " <> show slotNo <> ")"
      startWall <- liftIO getCurrentTime

      -- Deepest-first: each FK pass joins through its parent chain, so
      -- @ma_tx_out@ must run before the tx pass trims the @tx_out@ rows
      -- its join needs.
      acc1 <- foldM (runByFk tracer slotNo deleteByTxOutFkSlotStmt) 0 byTxOutFk
      acc2 <- foldM (runByFk tracer slotNo deleteByTxFkSlotStmt) acc1 byTxFk
      -- By-block-id tables join through @block.slot_no@, so they
      -- must run before @block@ itself is trimmed.
      acc3 <- foldM (runByParam tracer slotNo deleteByBlockSlotStmt) acc2 byBlockId
      acc4 <- foldM (runByParam tracer slotNo deleteBySlotStmt) acc3 bySlot
      -- Epoch-keyed tables carry no slot/block/tx anchor, so they are
      -- trimmed against the cutoff slot's epoch instead.
      acc5 <- trimByEpoch tracer slotNo byEpoch acc4
      acc6 <- case mode of
        IngestResume  -> foldM (runByCounter tracer unanchored row) acc5 byIdCounter
        FollowRestart -> pure acc5

      endWall <- liftIO getCurrentTime
      let totalDur = fmtDuration (realToFrac (diffUTCTime endWall startWall))
      emit tracer $
        if acc6 == 0
          then "complete in " <> totalDur <> " (no rows to clean)"
          else "complete in " <> totalDur <> " (" <> fmtCount acc6 <> " rows)"
      pure acc6

-- ---------------------------------------------------------------------------
-- Per-table step
-- ---------------------------------------------------------------------------

runByParam
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => AppTracer
  -> Word64
  -> (Text -> Stmt.Statement Word64 Int64)
  -> Int64
  -> TableDef
  -> m Int64
runByParam tracer slotNo mkStmt acc td =
  stepTable tracer (tdName td)
    (runStmt tracer slotNo (mkStmt (tdName td)) (tdName td))
    acc

runByFk
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => AppTracer
  -> Word64
  -> (Text -> Text -> Stmt.Statement Word64 Int64)
  -> Int64
  -> (TableDef, Text)
  -> m Int64
runByFk tracer slotNo mkStmt acc (td, fkCol) =
  stepTable tracer (tdName td)
    (runStmt tracer slotNo (mkStmt (tdName td) fkCol) (tdName td))
    acc

-- | Children first, and only the ones no anchor pass reaches: deleting
-- the parent ahead of them would both orphan the rows and violate the
-- parent's foreign key.
runByCounter
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => AppTracer
  -> [TableDef]
  -- ^ Tables carrying no slot, block or @tx@ anchor of their own.
  -> SyncStateRow
  -> Int64
  -> (TableDef, SyncStateRow -> Int64)
  -> m Int64
runByCounter tracer unanchored rowSnapshot acc (td, counter) = do
  acc' <- foldM childStep acc (childrenOf unanchored (tdName td))
  stepTable tracer (tdName td)
    (runStmt tracer threshold (deleteByIdCounterStmt (tdName td)) (tdName td))
    acc'
  where
    threshold = counter rowSnapshot
    childStep accIn (child, fkCol) =
      stepTable tracer child
        (runStmt tracer threshold (deleteByParentCounterStmt child fkCol) child)
        accIn

-- | Resolve the cutoff slot to its epoch, then trim each epoch-keyed
-- table by its own 'EpochAnchor'. The pass must keep every row an
-- already-committed block produced, and delete only the rows a
-- re-processed block emits again; the anchor decides which epochs
-- those are. A cutoff slot with no epoch-carrying block below it means
-- the chain never reached these tables, so nothing needs a trim.
trimByEpoch
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => AppTracer
  -> Word64
  -> [(TableDef, TableColumn, EpochAnchor)]
  -> Int64
  -> m Int64
trimByEpoch tracer slotNo tables acc = do
  mCutoff <- runStmt tracer slotNo selectEpochNoAtSlotStmt (tdName Core.blockTableDef)
  case mCutoff of
    Nothing -> pure acc
    Just cutoffEpoch -> foldM (runByAnchor tracer cutoffEpoch) acc tables

runByAnchor
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => AppTracer
  -> Word64
  -> Int64
  -> (TableDef, TableColumn, EpochAnchor)
  -> m Int64
runByAnchor tracer cutoffEpoch acc (td, c, anchor) =
  stepTable tracer (tdName td)
    (runStmt tracer cutoffEpoch (deleteEpochRowsStmt anchor c) (tdName td))
    acc

-- | Run one table's DELETE and time it. A zero-row delete stays
-- silent, so a 'FollowRestart' cleanup does not spam the log.
stepTable
  :: MonadIO m
  => AppTracer
  -> Text
  -> m Int64
  -> Int64
  -> m Int64
stepTable tracer tableName action acc = do
  start <- liftIO getCurrentTime
  rows  <- action
  end   <- liftIO getCurrentTime
  when (rows > 0) $
    emit tracer $
      tableName
        <> " [✓] ("
        <> fmtCountCompact rows
        <> ", "
        <> fmtDuration (realToFrac (diffUTCTime end start))
        <> ")"
  pure (acc + rows)

-- | Compact integer rendering: @1234@ → @1.2K@, @6500000@ → @6.5M@.
-- 'fmtCount' renders the same number as @6,500,000@.
fmtCountCompact :: Int64 -> Text
fmtCountCompact n
  | n < 1_000          = T.pack (show n)
  | n < 1_000_000      = T.pack (printf "%.1fK" (fromIntegral n / 1_000          :: Double))
  | n < 1_000_000_000  = T.pack (printf "%.1fM" (fromIntegral n / 1_000_000      :: Double))
  | otherwise          = T.pack (printf "%.1fB" (fromIntegral n / 1_000_000_000  :: Double))

-- ---------------------------------------------------------------------------
-- Per-table classification
-- ---------------------------------------------------------------------------

-- | At most one slot/block strategy plus an optional counter
-- strategy. The two axes are orthogonal: @block@ carries both a
-- @slot_no@ and a counter, and the counter pass then only guards.
data CleanupShape = CleanupShape
  { csSlotBlock :: !(Maybe SlotBlockShape)
  , csIdCounter :: !(Maybe (SyncStateRow -> Int64))
  }

-- | How a table reaches the cutoff slot. 'classify' checks these in
-- constructor order and takes the first match. The slot, block and FK
-- anchors are exact, so a table that carries one uses it even when it
-- also has an epoch column. The 'Text' on the FK variants names the
-- table's own FK column.
data SlotBlockShape
  = HasSlotNo
  | HasBlockId
  | HasTxFk !Text
  | HasTxOutFk !Text
  | HasEpochAnchor !TableColumn !EpochAnchor
  deriving stock (Eq, Show)

classify :: TableDef -> CleanupShape
classify td = CleanupShape
  { csSlotBlock = slotBlock
  , csIdCounter = lookup (tdName td) idCounterByTable
  }
  where
    columnNames = map cdName (tdColumns td)
    hasColumn c = c `elem` columnNames
    fkTo parent =
      prColumn <$> find ((== parent) . prParentTable) (tdParentRefs td)
    slotBlock
      | hasColumn "slot_no"                        = Just HasSlotNo
      | hasColumn "block_id"                       = Just HasBlockId
      | Just c <- fkTo (tdName Core.txTableDef)    = Just (HasTxFk c)
      | Just c <- fkTo (tdName UTxO.txOutTableDef) = Just (HasTxOutFk c)
      | Just (c, a) <- epochAnchorFor (tdName td)  = Just (HasEpochAnchor c a)
      | otherwise                                  = Nothing

-- ---------------------------------------------------------------------------
-- Internal IO
-- ---------------------------------------------------------------------------

-- | Run one statement on the control connection under a 5-second
-- heartbeat, so a slow DELETE still reports liveness.
runStmt
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => AppTracer
  -> p
  -> Stmt.Statement p r
  -> Text   -- ^ Table name (used as the heartbeat label)
  -> m r
runStmt tracer params stmt label = do
  ControlConnection conn <- asks getControlConnection
  result <- liftIO $
    withHeartbeatIO tracer "ResumeCleanup" (label <> ": still running") 5
      (Conn.use conn (Sess.statement params stmt))
  case result of
    Left err -> throwDb $ "deleteRowsPastSlot: " <> T.pack (show err)
    Right r  -> pure r

emit :: MonadIO m => AppTracer -> Text -> m ()
emit tracer msg = liftIO $ traceWith tracer $ LogMsg Info "ResumeCleanup" msg
