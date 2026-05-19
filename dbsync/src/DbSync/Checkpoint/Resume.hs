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
module DbSync.Checkpoint.Resume
  ( CleanupMode (..)
  , deleteRowsPastSlot
  ) where

import Cardano.Prelude

import Data.List (lookup)
import qualified Data.Text as T
import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

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
      let classified  = map (\td -> (td, classify td)) tableDefs
          byBlockId   = [ td        | (td, sh) <- classified
                                    , csSlotBlock sh == Just HasBlockId ]
          bySlot      = [ td        | (td, sh) <- classified
                                    , csSlotBlock sh == Just HasSlotNo  ]
          byIdCounter = [ (td, ctr) | (td, sh) <- classified
                                    , Just ctr <- [csIdCounter sh] ]
      -- By-block-id tables join through @block.slot_no@, so they
      -- must run before @block@ itself is trimmed.
      n1 <- sum <$> traverse
        (\td -> runCtrl slotNo (deleteByBlockSlotStmt (tdName td)))
        byBlockId
      n2 <- sum <$> traverse
        (\td -> runCtrl slotNo (deleteBySlotStmt (tdName td)))
        bySlot
      n3 <- case mode of
        IngestResume ->
          sum <$> traverse
            (\(td, counter) ->
               runCtrl (counter row) (deleteByIdCounterStmt (tdName td)))
            byIdCounter
        FollowRestart -> pure 0
      pure (n1 + n2 + n3)

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

runCtrl
  :: (HasCallStack, HasControlConnection env, MonadReader env m, MonadIO m)
  => p
  -> Stmt.Statement p r
  -> m r
runCtrl params stmt = do
  ControlConnection conn <- asks getControlConnection
  result <- liftIO $ Conn.use conn (Sess.statement params stmt)
  case result of
    Left err -> throwDb $ "deleteRowsPastSlot: " <> T.pack (show err)
    Right r  -> pure r
