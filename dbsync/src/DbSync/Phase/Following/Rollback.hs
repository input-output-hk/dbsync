{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Rollback cascade for 'FollowingChainTip'.
--
-- 'rollbackToPoint' resolves the target to a @block_id@, computes
-- the per-FK-family minimum ids that mark the threshold past which
-- rows are to be deleted, then runs the cascade and the sync-state
-- advance inside one PG transaction. Mirrors upstream
-- cardano-db-sync's @deleteBlocksBlockId@; slow path only.
--
-- The cascade tables come from each 'TableDef'\'s 'tdParentRefs' so
-- the rollback automatically picks up new dependent tables when they
-- declare the right parent; no hand-maintained list to drift. Rows
-- keyed by epoch rather than by a chain position are scoped by
-- 'epochKeyedColumns' instead. A table can appear in both — the two
-- DELETEs agree on which rows go, so the second is a no-op.
--
-- Passes run deepest-first: a table is deleted before the table it
-- declares a 'ParentRef' to, which is what the PG constraints created
-- in 'PreparingForVolatileTail' require.
module DbSync.Phase.Following.Rollback
  ( rollbackToPoint
  , rollbackToSlot

    -- * Schema walk
    -- | Re-exported so a test can pin the cascade's per-parent child
    -- lists against the live schema.
  , childrenOf
  ) where

import Cardano.Prelude

import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt
import Ouroboros.Consensus.Block.Abstract (fromRawHash, toRawHash)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Cardano.Node ()                       -- CanHardFork orphan
import Ouroboros.Consensus.Shelley.HFEras ()                     -- per-era HFC instances
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()    -- LedgerSupportsProtocol orphans
import Ouroboros.Network.Block (data BlockPoint, data GenesisPoint)
import Cardano.Slotting.Slot (SlotNo (..))

import DbSync.Parser.Types (CardanoPoint)
import qualified DbSync.Db.Schema.Core as Core
import DbSync.Db.Schema.Ids (getBlockId, getPoolUpdateId, getTxId, getTxOutId)
import qualified DbSync.Db.Schema.Pool as Pool
import DbSync.Db.Schema.Types (TableColumn (..), TableDef (..), childrenOf)
import qualified DbSync.Db.Schema.UTxO as UTxO
import DbSync.Db.Statement.Worker.EpochAnchor (deleteEpochRowsStmt, epochKeyedColumns)
import DbSync.Db.Statement.Worker.Rollback
  ( deleteBlockAfterIdStmt
  , deleteWhereGtStmt
  , deleteWhereGteStmt
  , nullConsumedByFromTxStmt
  , queryBlockAtOrAfterSlotStmt
  , queryBlockAtPointStmt
  , queryMinPoolUpdateIdAfterTxStmt
  , queryMinTxIdAfterBlockStmt
  , queryMinTxOutIdAfterBlockStmt
  , queryTipBlockNoStmt
  )
import DbSync.Db.Run (useConn)
import DbSync.Db.Statement.SyncState (writeSyncStateSlotStmt)
import DbSync.Db.Transaction (HasHasqlConnection (..), withTransaction)
import DbSync.App.Env (HasSecurityParam (..))

-- | Delete every row past the rollback target and advance
-- @dbsync_sync_state.last_committed_*@ to match.
--
-- The supplied 'TableDef' list scopes the cascade: only declared
-- tables are considered, and a table's 'tdParentRefs' (the parent
-- table referenced by 'prParentTable') decide which family it
-- belongs to. Tables that reference none of @block@ / @tx@ /
-- @tx_out@ / @pool_update@ are skipped by that cascade; the
-- epoch-keyed tables are cleaned via 'epochKeyedColumns'.
--
-- The k-safety horizon comes from 'getSecurityParam' on the env. A
-- target more than @k@ blocks behind the current PG tip is rejected
-- with 'panic' — chainsync can't deliver a deeper rollback, and a
-- CLI @--rollback-to-slot@ past that depth means the operator
-- should @--resync-from-genesis@ instead.
--
-- Refuses to roll back to 'GenesisPoint' — that would empty the DB
-- and is almost always a protocol bug rather than a real rollback.
-- Panics on a target the @block@ table doesn't know about (the node
-- sent us a point we never received).
rollbackToPoint
  :: ( HasHasqlConnection env, HasSecurityParam env
     , MonadReader env m, MonadUnliftIO m
     )
  => [TableDef] -> CardanoPoint -> m ()
rollbackToPoint tableDefs point = case point of
  GenesisPoint ->
    panic "rollbackToPoint: rollback to genesis is not supported"
  BlockPoint slotNo hash -> do
    let rawHash = toRawHash (Proxy @(CardanoBlock StandardCrypto)) hash
        rawSlot = unSlotNo slotNo

    mTarget <- runSess "queryBlockAtPointStmt"
      ((rawSlot, rawHash), queryBlockAtPointStmt)
    (targetBlockId, targetBlockNo, mTargetEpoch) <- case mTarget of
      Just t  -> pure t
      Nothing -> panic $
        "rollbackToPoint: no block in PG at slot " <> show rawSlot
          <> " — node sent a rollback target we never received"

    -- k-safety guard. Reads the live tip rather than relying on
    -- @dbsync_sync_state.last_committed_block_no@: the latter can
    -- lag mid-Follow whereas the @block@ table is the ground truth
    -- for what would be deleted.
    kBlocks    <- asks getSecurityParam
    mTipBlockNo <- runSess "queryTipBlockNoStmt" ((), queryTipBlockNoStmt)
    for_ mTipBlockNo $ \tipBlockNo ->
      when (tipBlockNo > targetBlockNo + kBlocks) $
        panic $
          "rollbackToPoint: target block " <> show targetBlockNo
            <> " is more than k=" <> show kBlocks
            <> " behind current tip " <> show tipBlockNo
            <> ". Use --resync-from-genesis for rollbacks past the"
            <> " k-safety horizon."

    -- Whether a table exists in PG. Sound because the boot extractor
    -- gate rejects any profile whose table set differs from the one
    -- the database was built with, so the set can't change mid-run.
    let hasTable td = any ((== tdName td) . tdName) tableDefs

    -- Single-threaded Follow loop guarantees no concurrent inserts
    -- shift these thresholds between the reads and the deletes, so
    -- caching them outside the transaction is safe. Both min-id
    -- queries read their own family's parent table, so they only run
    -- when that table is part of the profile.
    mMinTxId <- runSess "queryMinTxIdAfterBlockStmt"
      (targetBlockId, queryMinTxIdAfterBlockStmt)
    mMinTxOutId <- case mMinTxId of
      Just minTxId | hasTable UTxO.txOutTableDef ->
        runSess "queryMinTxOutIdAfterBlockStmt"
          (minTxId, queryMinTxOutIdAfterBlockStmt)
      _ -> pure Nothing
    mMinPoolUpdateId <- case mMinTxId of
      Just minTxId | hasTable Pool.poolUpdateTableDef ->
        runSess "queryMinPoolUpdateIdAfterTxStmt"
          (minTxId, queryMinPoolUpdateIdAfterTxStmt)
      _ -> pure Nothing

    -- Pre-compute per-family delete lists from the schema. Each entry
    -- is @(this-table, this-table's FK column to the parent)@. The
    -- parent table itself is deleted separately at the end.
    let txKeyed         = childrenOf tableDefs (tdName Core.txTableDef)
        txOutKeyed      = childrenOf tableDefs (tdName UTxO.txOutTableDef)
        poolUpdateKeyed = childrenOf tableDefs (tdName Pool.poolUpdateTableDef)
        -- tx declares block_id too, but is deleted by id below.
        blockKeyed      = filter ((/= tdName Core.txTableDef) . fst)
                            (childrenOf tableDefs (tdName Core.blockTableDef))

    withTransaction $ do
      -- tx_out and pool_update are themselves tx-keyed, so their
      -- families have to be cleared before the tx pass reaches them.
      for_ mMinTxOutId $ \minTxOutId -> do
        let !i = getTxOutId minTxOutId
        for_ txOutKeyed $ \(tbl, col) ->
          void $ runSess ("delete " <> tbl)
            (i, deleteWhereGteStmt tbl col)

      -- pool_update goes here rather than being left to the tx pass, so
      -- it lands after its own children; that pass then no-ops on it.
      for_ mMinPoolUpdateId $ \minPoolUpdateId -> do
        let !i      = getPoolUpdateId minPoolUpdateId
            poolTbl = tdName Pool.poolUpdateTableDef
        for_ poolUpdateKeyed $ \(tbl, col) ->
          void $ runSess ("delete " <> tbl)
            (i, deleteWhereGteStmt tbl col)
        void $ runSess ("delete " <> poolTbl)
          (i, deleteWhereGteStmt poolTbl "id")

      -- Tx-keyed cascade, including tx_out and pool_update themselves.
      for_ mMinTxId $ \minTxId -> do
        let !i = getTxId minTxId
        for_ txKeyed $ \(tbl, col) ->
          void $ runSess ("delete " <> tbl)
            (i, deleteWhereGteStmt tbl col)

      -- Surviving producer rows must drop their consumed-by marks
      -- pointing at the txs deleted above, or the re-applied fork
      -- can never re-consume them.
      for_ mMinTxId $ \minTxId ->
        when (hasTable UTxO.txOutTableDef) $ do
          let c = UTxO.tocConsumedByTxId UTxO.txOutCols
          void $ runSess ("null " <> tdName (tcTable c) <> "." <> tcName c)
            (minTxId, nullConsumedByFromTxStmt)

      -- Block-keyed cascade. No min-id hop needed: the target block
      -- is the new tip, so anything above it goes.
      for_ blockKeyed $ \(tbl, c) ->
        void $ runSess ("delete " <> tbl)
          (getBlockId targetBlockId, deleteWhereGtStmt tbl c)

      -- Finally tx and block themselves.
      let txTbl = tdName Core.txTableDef
      for_ mMinTxId $ \minTxId ->
        void $ runSess ("delete " <> txTbl)
          (getTxId minTxId, deleteWhereGteStmt txTbl "id")
      void $ runSess ("delete " <> tdName Core.blockTableDef)
        (targetBlockId, deleteBlockAfterIdStmt)

      -- Epoch-keyed boundary tables, whose rows carry no chain
      -- position of their own. Tables whose extractor is disabled are
      -- absent from tableDefs and skipped; a target without an
      -- epoch_no (Byron EBB) skips the lot.
      for_ mTargetEpoch $ \targetEpoch ->
        for_ epochKeyedColumns $ \(c, anchor) ->
          when (hasTable (tcTable c)) $
            void $ runSess ("delete " <> tdName (tcTable c))
              (targetEpoch, deleteEpochRowsStmt anchor c)

      -- Sync-state advance. The target block is the new chain tip.
      void $ runSess "writeSyncStateSlotStmt"
        ((rawSlot, targetBlockNo, rawHash), writeSyncStateSlotStmt)

-- | Roll back to the nearest block at-or-after a slot number.
--
-- The CLI gives a bare slot, but Cardano slots can be empty so the
-- exact slot may not contain a block. This resolves to the smallest
-- block with @slot_no >= targetSlot@ and delegates to
-- 'rollbackToPoint' for the cascade. Returns the block_no rolled
-- back to, or 'Nothing' if the DB has no block at-or-after the
-- target (database is already below the requested point — no work).
rollbackToSlot
  :: ( HasHasqlConnection env, HasSecurityParam env
     , MonadReader env m, MonadUnliftIO m
     )
  => [TableDef] -> Word64 -> m (Maybe Word64)
rollbackToSlot tableDefs targetSlot = do
  mTarget <- runSess "queryBlockAtOrAfterSlotStmt"
    (targetSlot, queryBlockAtOrAfterSlotStmt)
  case mTarget of
    Nothing -> pure Nothing
    Just (_, resolvedSlot, resolvedBlockNo, resolvedHash) -> do
      let hash  = fromRawHash (Proxy @(CardanoBlock StandardCrypto)) resolvedHash
          -- Use the resolved block's slot, not the requested one —
          -- 'rollbackToPoint' looks the block up by @(slot, hash)@
          -- and the two only match for the actual on-chain slot.
          point = BlockPoint (SlotNo resolvedSlot) hash
      rollbackToPoint tableDefs point
      pure (Just resolvedBlockNo)

-- | Run a single 'Stmt.Statement' against the env's connection.
-- Failures surface as 'AppDatabaseError' tagged with the caller-
-- supplied label.
runSess
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => Text                          -- ^ caller label
  -> (a, Stmt.Statement a b)
  -> m b
runSess label (params, stmt) = do
  conn <- asks getHasqlConnection
  useConn ("rollbackToPoint: " <> label) conn (Sess.statement params stmt)
