{-# LANGUAGE LambdaCase #-}

-- | Resume-time row cleanup.
--
-- Two boot scenarios use different strategies:
--
--   * 'IngestResume' — full cleanup. The COPY writer commits at
--     epoch boundaries and the dedup-counter snapshot in
--     'SyncStateRow' lags by one epoch, so rows can sit past both
--     'ssrLastCommittedSlot' and the recorded counters.
--
--   * 'FollowRestart' — defensive only. Follow's per-block
--     transaction is atomic, so no orphan rows past the recorded
--     slot are possible. Dedup-counter columns are stale here
--     because 'writeSyncStateSlotStmt' deliberately doesn't touch
--     them — running the counter DELETE would wipe legitimate rows
--     that fact-table FKs reference.
module DbSync.Checkpoint.Resume
  ( CleanupMode (..)
  , deleteRowsPastSlot
  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Checkpoint.SyncState
  ( ControlConnection (..)
  , HasControlConnection (..)
  , SyncStateRow (..)
  )
import DbSync.Db.Schema.Types (ColumnDef (..), TableDef (..))
import DbSync.Db.Statement.Resume
  ( deleteByBlockSlotStmt
  , deleteBySlotStmt
  , deleteDedupByCounterStmt
  )
import DbSync.Error (throwDb)

-- | Which boot scenario the cleanup is running under. See module Haddock.
data CleanupMode
  = IngestResume
    -- ^ Full cleanup against the 'SyncStateRow' counters.
  | FollowRestart
    -- ^ Skip the dedup-counter DELETE; the counters are stale on
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
      let classified = map (\td -> (td, classify td)) tableDefs
          byBlockId  = [td        | (td, HasBlockId)    <- classified]
          bySlot     = [td        | (td, HasSlotNo)     <- classified]
          dedup      = [(td, ctr) | (td, IsDedup ctr)   <- classified]
      -- By-block-id tables join through 'block.slot_no', so they
      -- must run before 'block' itself is trimmed.
      n1 <- sum <$> traverse (\td -> runCtrl slotNo (deleteByBlockSlotStmt (tdName td))) byBlockId
      n2 <- sum <$> traverse (\td -> runCtrl slotNo (deleteBySlotStmt      (tdName td))) bySlot
      n3 <- case mode of
        IngestResume ->
          sum <$> traverse
            (\(td, counter) ->
               runCtrl (counter row) (deleteDedupByCounterStmt (tdName td)))
            dedup
        FollowRestart -> pure 0
      pure (n1 + n2 + n3)

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

-- | Dedup table to its "next id to assign" counter on 'SyncStateRow'.
-- @address@ is included because the background AddressResolver
-- allocates IDs during Ingest and a crash between its INSERTs and
-- 'writeSyncState' leaves rows past the recorded counter.
dedupCounterFor :: Text -> Maybe (SyncStateRow -> Int64)
dedupCounterFor = \case
  "slot_leader"   -> Just ssrSlotLeaderIdCounter
  "stake_address" -> Just ssrStakeAddressIdCounter
  "pool_hash"     -> Just ssrPoolHashIdCounter
  "multi_asset"   -> Just ssrMultiAssetIdCounter
  "script"        -> Just ssrScriptIdCounter
  "address"       -> Just ssrAddressIdCounter
  _               -> Nothing

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
