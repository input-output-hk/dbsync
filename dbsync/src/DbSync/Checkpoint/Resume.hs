{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Resume-time row cleanup.
--
-- Two boot scenarios use different strategies:
--
--   * 'IngestResume' — full cleanup. The COPY writer commits at
--     epoch boundaries and the @*_id_counter@ snapshot in
--     'SyncStateRow' lags by one epoch, so rows can sit past both
--     'ssrLastCommittedSlot' and the recorded counters. Tables
--     without @slot_no@ or @block_id@ rely on the counter pass for
--     pruning; tables with one of those columns also get the
--     counter pass as a belt-and-braces guard, which is a no-op
--     once the slot pass has finished.
--
--   * 'FollowRestart' — defensive only. Follow's per-block
--     transaction is atomic, so no orphan rows past the recorded
--     slot are possible. Counter columns are stale on this path
--     because 'writeSyncStateSlotStmt' deliberately doesn't touch
--     them — running the counter DELETE would wipe legitimate rows
--     that fact-table FKs reference.
--
-- == Progress logging
--
-- Each DELETE emits one log line on completion if it actually
-- removed rows; the line carries every previously-completed table
-- as @name [✓]@ separated by @" - "@, with the most recent table's
-- row count and duration in parens at the end. A 5-second
-- heartbeat fires while any single DELETE is in flight, so the
-- long ones (typically @tx_out@ and @ma_tx_out@) still report
-- liveness. Zero-row tables are skipped from the tally so a
-- 'FollowRestart' cleanup stays quiet.
module DbSync.Checkpoint.Resume
  ( CleanupMode (..)
  , deleteRowsPastSlot
  ) where

import Cardano.Prelude

import Control.Tracer (traceWith)
import Data.List (lookup)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt
import Text.Printf (printf)

import DbSync.Checkpoint.SyncState
  ( ControlConnection (..)
  , HasControlConnection (..)
  , SyncStateRow (..)
  )
import DbSync.Db.Schema.SyncState (idCounterByTable)
import DbSync.Db.Schema.Types (ColumnDef (..), TableDef (..))
import DbSync.Db.Statement.Resume
  ( deleteByBlockSlotStmt
  , deleteByIdCounterStmt
  , deleteBySlotStmt
  )
import DbSync.Error (throwDb)
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Timing (fmtCount, fmtDuration, withHeartbeatIO)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- | Which boot scenario the cleanup is running under. See module Haddock.
data CleanupMode
  = IngestResume
    -- ^ Full cleanup against both the @last_committed_slot@ and the
    -- 'SyncStateRow' counters.
  | FollowRestart
    -- ^ Skip the counter DELETE; the counter columns are stale on
    -- this path and the DELETE would wipe live rows.
  deriving stock (Eq, Show)

-- | Delete every row past the row's @last_committed_slot@ across the
-- given tables. Returns the total number of rows deleted. No-op when
-- the row reports no committed progress.
deleteRowsPastSlot
  :: ( HasCallStack
     , HasTracer env
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
                                    , csSlotBlock sh == Just HasBlockId ]
          bySlot      = [ td        | (td, sh) <- classified
                                    , csSlotBlock sh == Just HasSlotNo  ]
          byIdCounter = [ (td, ctr) | (td, sh) <- classified
                                    , Just ctr <- [csIdCounter sh] ]

      emit tracer $ "starting (cutoff slot > " <> show slotNo <> ")"
      startWall <- liftIO getCurrentTime

      -- By-block-id tables join through @block.slot_no@, so they
      -- must run before @block@ itself is trimmed.
      (acc1, tally1) <-
        foldM (runByParam tracer slotNo deleteByBlockSlotStmt) (0, []) byBlockId
      (acc2, tally2) <-
        foldM (runByParam tracer slotNo deleteBySlotStmt) (acc1, tally1) bySlot
      (acc3, tally3) <- case mode of
        IngestResume ->
          foldM (runByCounter tracer row) (acc2, tally2) byIdCounter
        FollowRestart ->
          pure (acc2, tally2)

      endWall <- liftIO getCurrentTime
      let totalDur = fmtDuration (realToFrac (diffUTCTime endWall startWall))
      emit tracer $
        if null tally3
          then "complete in " <> totalDur <> " (no rows to clean)"
          else "complete in " <> totalDur <> " (" <> fmtCount acc3 <> " rows)"
      pure acc3

-- ---------------------------------------------------------------------------
-- Tally bookkeeping
-- ---------------------------------------------------------------------------

-- | One entry in the running tally: table name, row count, duration.
type TallyEntry = (Text, Int64, NominalDiffTime)

runByParam
  :: (HasCallStack, HasControlConnection env, MonadReader env m, MonadIO m)
  => AppTracer
  -> Word64
  -> (Text -> Stmt.Statement Word64 Int64)
  -> (Int64, [TallyEntry])
  -> TableDef
  -> m (Int64, [TallyEntry])
runByParam tracer slotNo mkStmt acc td =
  stepTally tracer td
    (runDelete tracer slotNo (mkStmt (tdName td)) (tdName td))
    acc

runByCounter
  :: (HasCallStack, HasControlConnection env, MonadReader env m, MonadIO m)
  => AppTracer
  -> SyncStateRow
  -> (Int64, [TallyEntry])
  -> (TableDef, SyncStateRow -> Int64)
  -> m (Int64, [TallyEntry])
runByCounter tracer rowSnapshot acc (td, counter) =
  stepTally tracer td
    (runDelete tracer (counter rowSnapshot) (deleteByIdCounterStmt (tdName td)) (tdName td))
    acc

-- | Run one table's DELETE, time it, and conditionally append to the
-- tally + emit a fresh log line carrying the running list. Zero-row
-- deletes are silent so 'FollowRestart' (where almost every table is
-- 0 rows) doesn't spam the log.
stepTally
  :: MonadIO m
  => AppTracer
  -> TableDef
  -> m Int64
  -> (Int64, [TallyEntry])
  -> m (Int64, [TallyEntry])
stepTally tracer td action (acc, tally) = do
  start <- liftIO getCurrentTime
  rows  <- action
  end   <- liftIO getCurrentTime
  let dur = diffUTCTime end start
  if rows > 0
    then do
      let !entry = (tdName td, rows, dur)
          tally' = tally ++ [entry]
      emit tracer (renderTally tally' entry)
      pure (acc + rows, tally')
    else
      pure (acc + rows, tally)

-- | Render @"name [✓] - name [✓] - name [✓] (rows, dur)"@. The
-- caller passes the most recently completed entry explicitly so the
-- renderer never has to call partial 'last' on the list; the same
-- entry is also the final element of @entries@.
renderTally :: [TallyEntry] -> TallyEntry -> Text
renderTally entries (_, lastRows, lastDur) =
  let names  = map (\(n, _, _) -> n <> " [✓]") entries
      joined = T.intercalate " - " names
  in joined
       <> " ("
       <> fmtCountCompact lastRows
       <> ", "
       <> fmtDuration (realToFrac lastDur)
       <> ")"

-- | Compact integer rendering for the tally line — @1234@ → @1.2K@,
-- @6_500_000@ → @6.5M@. Distinct from 'fmtCount' (which produces
-- @6,500,000@) because the tally line stays dense across many
-- tables.
fmtCountCompact :: Int64 -> Text
fmtCountCompact n
  | n < 1_000          = T.pack (show n)
  | n < 1_000_000      = T.pack (printf "%.1fK" (fromIntegral n / 1_000          :: Double))
  | n < 1_000_000_000  = T.pack (printf "%.1fM" (fromIntegral n / 1_000_000      :: Double))
  | otherwise          = T.pack (printf "%.1fB" (fromIntegral n / 1_000_000_000  :: Double))

-- ---------------------------------------------------------------------------
-- Per-table classification
-- ---------------------------------------------------------------------------

-- | Per-table classification: at most one slot/block strategy plus
-- an optional counter strategy. The two axes are orthogonal — a
-- table can have both (e.g. @block@ has @slot_no@ and a counter on
-- 'SyncStateRow'), and the counter pass then acts as a redundant
-- guard.
data CleanupShape = CleanupShape
  { csSlotBlock :: !(Maybe SlotBlockShape)
  , csIdCounter :: !(Maybe (SyncStateRow -> Int64))
  }

-- | Whether a table carries its own @slot_no@ or only references it
-- via @block_id@. Mutually exclusive — @block_id@ tables get the
-- inner-join variant of the cleanup.
data SlotBlockShape = HasSlotNo | HasBlockId
  deriving stock (Eq, Show)

classify :: TableDef -> CleanupShape
classify td = CleanupShape
  { csSlotBlock = slotBlock
  , csIdCounter = lookup (tdName td) idCounterByTable
  }
  where
    columnNames = map cdName (tdColumns td)
    hasColumn c = c `elem` columnNames
    slotBlock
      | hasColumn "slot_no"  = Just HasSlotNo
      | hasColumn "block_id" = Just HasBlockId
      | otherwise            = Nothing

-- ---------------------------------------------------------------------------
-- Internal IO
-- ---------------------------------------------------------------------------

-- | Run a 'Stmt.Statement' against the env's control connection,
-- wrapping it in a 5-second heartbeat so a slow DELETE still emits
-- progress while it runs.
runDelete
  :: (HasCallStack, HasControlConnection env, MonadReader env m, MonadIO m)
  => AppTracer
  -> p
  -> Stmt.Statement p r
  -> Text   -- ^ Table name (used as the heartbeat label)
  -> m r
runDelete tracer params stmt label = do
  ControlConnection conn <- asks getControlConnection
  result <- liftIO $
    withHeartbeatIO tracer "ResumeCleanup" (label <> ": still running") 5
      (Conn.use conn (Sess.statement params stmt))
  case result of
    Left err -> throwDb $ "deleteRowsPastSlot: " <> T.pack (show err)
    Right r  -> pure r

emit :: MonadIO m => AppTracer -> Text -> m ()
emit tracer msg = liftIO $ traceWith tracer $ LogMsg Info "ResumeCleanup" msg Nothing
