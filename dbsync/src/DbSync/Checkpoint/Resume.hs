{-# LANGUAGE LambdaCase #-}

-- | Resume-time row cleanup.
--
-- A crash between the COPY commit and the sync-state UPDATE can
-- leave rows in PG past 'ssrLastCommittedSlot'. 'deleteRowsPastSlot'
-- runs at boot to remove those rows so the consumer starts with a
-- consistent view.
--
-- Per-table dispatch:
--
--   * Tables with a @slot_no@ column are deleted by slot.
--   * Tables with a @block_id@ FK (but no @slot_no@) are deleted by
--     joining to @block.slot_no@.
--   * Dedup tables (no slot, no block) are deleted by id, using the
--     corresponding @*_id_counter@ from 'SyncStateRow' as the
--     \"first id past the committed point\" boundary.
--
-- Tables are processed in dependency order: the @block_id@-referencing
-- tables run their @block.slot_no@ subqueries against the still-intact
-- @block@ table, before @block@ itself is trimmed.
module DbSync.Checkpoint.Resume
  ( deleteRowsPastSlot
  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Checkpoint.SyncState (ControlConnection (..), SyncStateRow (..))
import DbSync.Db.Schema.Types (ColumnDef (..), TableDef (..))
import DbSync.Db.Statement.Resume
  ( deleteByBlockSlotStmt
  , deleteBySlotStmt
  , deleteDedupByCounterStmt
  )
import DbSync.Error (throwDb)

-- | Delete every row past the row's @last_committed_slot@ across the
-- given tables. Returns the total number of rows deleted (for log
-- output). No-op when the row reports no committed progress.
deleteRowsPastSlot :: HasCallStack => ControlConnection -> [TableDef] -> SyncStateRow -> IO Int64
deleteRowsPastSlot ctrl tableDefs row =
  case ssrLastCommittedSlot row of
    Nothing -> pure 0
    Just slotNo -> do
      let classified = map (\td -> (td, classify td)) tableDefs
          byBlockId  = [td        | (td, HasBlockId)    <- classified]
          bySlot     = [td        | (td, HasSlotNo)     <- classified]
          dedup      = [(td, ctr) | (td, IsDedup ctr)   <- classified]
      -- Order matters: by-block-id tables must be cleaned before the
      -- 'block' table is itself trimmed, otherwise their @SELECT id
      -- FROM block WHERE slot_no > $1@ subquery returns nothing.
      n1 <- sum <$> traverse (\td -> runStmt ctrl slotNo (deleteByBlockSlotStmt (tdName td))) byBlockId
      n2 <- sum <$> traverse (\td -> runStmt ctrl slotNo (deleteBySlotStmt      (tdName td))) bySlot
      n3 <- sum <$> traverse
              (\(td, counter) ->
                 runStmt ctrl (counter row) (deleteDedupByCounterStmt (tdName td)))
              dedup
      pure (n1 + n2 + n3)

-- | Per-table cleanup strategy.
data TableShape
  = HasSlotNo
  | HasBlockId
  | IsDedup !(SyncStateRow -> Int64)
  | Skip

classify :: TableDef -> TableShape
classify td
  | hasColumn "slot_no"  = HasSlotNo
  | hasColumn "block_id" = HasBlockId
  | otherwise = case dedupCounterFor (tdName td) of
      Just counter -> IsDedup counter
      Nothing      -> Skip
  where
    columnNames = map cdName (tdColumns td)
    hasColumn c = c `elem` columnNames

-- | One entry per dedup table, mapping its name to the counter on
-- 'SyncStateRow' that records \"the next id we'd assign\".
--
-- @address@ is included so partial-epoch rows from the background
-- 'DbSync.Resolver.AddressWorker.AddressResolver' get cleaned up:
-- a crash between the worker's INSERTs and 'writeSyncState' leaves
-- @address@ rows with @id >= ssrAddressIdCounter@; without this
-- entry the next run's SELECT-before-INSERT dedup races with the
-- orphan rows and produces duplicates that fail the post-load
-- @UNIQUE (raw)@ index build in 'PreparingForChainTip'.
dedupCounterFor :: Text -> Maybe (SyncStateRow -> Int64)
dedupCounterFor = \case
  "slot_leader"   -> Just ssrSlotLeaderIdCounter
  "stake_address" -> Just ssrStakeAddressIdCounter
  "pool_hash"     -> Just ssrPoolHashIdCounter
  "multi_asset"   -> Just ssrMultiAssetIdCounter
  "script"        -> Just ssrScriptIdCounter
  "address"       -> Just ssrAddressIdCounter
  _               -> Nothing

-- | Run a 'Stmt.Statement' on the control connection, lifting any
-- 'SessionError' into 'AppDatabaseError'.
runStmt :: HasCallStack => ControlConnection -> p -> Stmt.Statement p r -> IO r
runStmt (ControlConnection conn) params stmt = do
  result <- Conn.use conn (Sess.statement params stmt)
  case result of
    Left err -> throwDb $ "deleteRowsPastSlot: " <> T.pack (show err)
    Right r  -> pure r
