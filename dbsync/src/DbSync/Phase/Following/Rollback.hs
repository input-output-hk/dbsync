{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Rollback cascade for 'FollowingChainTip'.
--
-- 'rollbackToPoint' resolves the target to a @block_id@, computes
-- the per-FK-family minimum ids that mark the threshold past which
-- rows are to be deleted, then runs the cascade and the sync-state
-- advance inside one PG transaction. Mirrors the original
-- cardano-db-sync's @deleteBlocksBlockId@ — slow path only.
--
-- The cascade tables come from each 'TableDef'\'s 'tdForeignKeys' so
-- the rollback automatically picks up new dependent tables when they
-- declare the right FK; no hand-maintained list to drift.
module DbSync.Phase.Following.Rollback
  ( rollbackToPoint
  , rollbackToSlot

    -- * Schema-walk helpers (exported for tests)
  , childrenOf
  ) where

import Cardano.Prelude

import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt
import Ouroboros.Consensus.Block.Abstract (fromRawHash, toRawHash)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Cardano.Node ()                       -- CanHardFork orphan
import Ouroboros.Consensus.Shelley.HFEras ()                     -- per-era HFC instances
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()    -- LedgerSupportsProtocol orphans
import Ouroboros.Network.Block (pattern BlockPoint, pattern GenesisPoint)
import Cardano.Slotting.Slot (SlotNo (..))

import DbSync.Block.Types (CardanoPoint)
import qualified DbSync.Db.Schema.Core as Core
import DbSync.Db.Schema.Ids (getPoolUpdateId, getTxId, getTxOutId)
import qualified DbSync.Db.Schema.Pool as Pool
import DbSync.Db.Schema.Types (ForeignKey (..), TableDef (..))
import qualified DbSync.Db.Schema.UTxO as UTxO
import DbSync.Db.Statement.Rollback
  ( deleteBlockAfterIdStmt
  , deleteWhereGteStmt
  , queryBlockAtOrAfterSlotStmt
  , queryBlockAtPointStmt
  , queryMinPoolUpdateIdAfterTxStmt
  , queryMinTxIdAfterBlockStmt
  , queryMinTxOutIdAfterBlockStmt
  , queryTipBlockNoStmt
  )
import DbSync.Db.Statement.SyncState (writeSyncStateSlotStmt)
import DbSync.Db.Transaction (HasHasqlConnection (..), withTransaction)
import DbSync.Env (HasSecurityParam (..))

-- | Delete every row past the rollback target and advance
-- @dbsync_sync_state.last_committed_*@ to match.
--
-- The supplied 'TableDef' list scopes the cascade: only declared
-- tables are considered, and a table's outgoing FKs (the parent
-- table referenced by 'fkParentTable') decide which family it
-- belongs to. Tables that don't reference @tx@ / @tx_out@ /
-- @pool_update@ are silently skipped.
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
    (targetBlockId, targetBlockNo) <- case mTarget of
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

    -- Single-threaded Follow loop guarantees no concurrent inserts
    -- shift these thresholds between the reads and the deletes, so
    -- caching them outside the transaction is safe.
    mMinTxId <- runSess "queryMinTxIdAfterBlockStmt"
      (targetBlockId, queryMinTxIdAfterBlockStmt)
    mMinTxOutId <- case mMinTxId of
      Nothing      -> pure Nothing
      Just minTxId -> runSess "queryMinTxOutIdAfterBlockStmt"
        (minTxId, queryMinTxOutIdAfterBlockStmt)
    mMinPoolUpdateId <- case mMinTxId of
      Nothing      -> pure Nothing
      Just minTxId -> runSess "queryMinPoolUpdateIdAfterTxStmt"
        (minTxId, queryMinPoolUpdateIdAfterTxStmt)

    -- Pre-compute per-family delete lists from the schema. Each entry
    -- is @(this-table, this-table's FK column to the parent)@. The
    -- parent table itself is deleted separately at the end.
    let txKeyed         = childrenOf tableDefs (tdName Core.txTableDef)
        txOutKeyed      = childrenOf tableDefs (tdName UTxO.txOutTableDef)
        poolUpdateKeyed = childrenOf tableDefs (tdName Pool.poolUpdateTableDef)

    withTransaction $ do
      -- Tx-keyed cascade.
      for_ mMinTxId $ \minTxId -> do
        let !i = getTxId minTxId
        for_ txKeyed $ \(tbl, col) ->
          void $ runSess ("delete " <> tbl)
            (i, deleteWhereGteStmt tbl col)

      -- TxOut-keyed cascade.
      for_ mMinTxOutId $ \minTxOutId -> do
        let !i = getTxOutId minTxOutId
        for_ txOutKeyed $ \(tbl, col) ->
          void $ runSess ("delete " <> tbl)
            (i, deleteWhereGteStmt tbl col)

      -- PoolUpdate-keyed cascade. The pool_update parent itself is
      -- also deleted here because removing the children first lets
      -- the parent delete proceed without violating any future FK
      -- constraint additions.
      for_ mMinPoolUpdateId $ \minPoolUpdateId -> do
        let !i      = getPoolUpdateId minPoolUpdateId
            poolTbl = tdName Pool.poolUpdateTableDef
        for_ poolUpdateKeyed $ \(tbl, col) ->
          void $ runSess ("delete " <> tbl)
            (i, deleteWhereGteStmt tbl col)
        void $ runSess ("delete " <> poolTbl)
          (i, deleteWhereGteStmt poolTbl "id")

      -- Finally tx and block themselves.
      let txTbl = tdName Core.txTableDef
      for_ mMinTxId $ \minTxId ->
        void $ runSess ("delete " <> txTbl)
          (getTxId minTxId, deleteWhereGteStmt txTbl "id")
      void $ runSess ("delete " <> tdName Core.blockTableDef)
        (targetBlockId, deleteBlockAfterIdStmt)

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

-- | All tables that declare an outgoing FK to @parentTable@, paired
-- with the FK column name. Walks every supplied 'TableDef' and pulls
-- out the matching entries from 'tdForeignKeys'.
childrenOf :: [TableDef] -> Text -> [(Text, Text)]
childrenOf tableDefs parentTable =
  [ (tdName td, fkColumn fk)
  | td <- tableDefs
  , fk <- tdForeignKeys td
  , fkParentTable fk == parentTable
  ]

-- | Run a single 'Stmt.Statement' against the env's connection.
-- Panics with the caller-supplied label on a session error so the
-- rollback's stack trace stays legible.
runSess
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => Text                          -- ^ caller label
  -> (a, Stmt.Statement a b)
  -> m b
runSess label (params, stmt) = do
  conn <- asks getHasqlConnection
  r <- liftIO $ Conn.use conn (Sess.statement params stmt)
  case r of
    Right b -> pure b
    Left e  -> panic $ "rollbackToPoint: " <> label <> ": " <> show e
