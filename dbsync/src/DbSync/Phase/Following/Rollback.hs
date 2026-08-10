{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Rollback cascade for 'FollowingChainTip'. 'rollbackToPoint'
-- resolves the target to a @block_id@, computes the per-family
-- minimum ids, then deletes and advances the sync state in one PG
-- transaction.
--
-- Each table's 'tdParentRefs' drives the cascade, so a new dependent
-- table joins it as soon as it declares its parent. Epoch-keyed rows
-- carry no chain position, so 'epochKeyedColumns' scopes those
-- instead. A table can appear in both lists: the two DELETEs pick the
-- same rows, so the second one does nothing.
--
-- The passes run deepest-first. A table goes before the table it
-- declares a 'ParentRef' to, which is what the PG constraints
-- require.
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
import qualified DbSync.Db.Schema.Governance as Gov
import DbSync.Db.Schema.Ids (getBlockId, getPoolUpdateId, getTxId, getTxOutId)
import qualified DbSync.Db.Schema.Pool as Pool
import DbSync.Db.Schema.Types (TableColumn (..), TableDef (..), childrenOf)
import qualified DbSync.Db.Schema.UTxO as UTxO
import DbSync.Db.Statement.Worker.EpochAnchor (deleteEpochRowsStmt, epochKeyedColumns)
import DbSync.Db.Statement.Worker.Rollback
  ( deleteBlockAfterIdStmt
  , deleteByProposalTxStmt
  , deleteCommitteeMembersByProposalTxStmt
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
-- @dbsync_sync_state.last_committed_*@ to match. The 'TableDef' list
-- scopes the cascade.
--
-- Panics in three cases: a 'GenesisPoint' target, which would empty
-- the database; a target the @block@ table does not hold, which means
-- the node sent a point we never received; and a target more than
-- @k@ blocks behind the PG tip, where the operator needs
-- @--resync-from-genesis@ instead. 'getSecurityParam' supplies @k@.
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

    -- The guard reads the live tip, not
    -- @dbsync_sync_state.last_committed_block_no@: that column lags
    -- mid-Follow, but the @block@ table holds what the cascade
    -- deletes.
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

    -- The boot gate rejects a config whose table set differs from the
    -- one the database holds, so this set cannot change mid-run.
    let hasTable td = any ((== tdName td) . tdName) tableDefs

    -- The Follow loop is single-threaded, so no concurrent insert
    -- shifts these thresholds between the reads and the deletes.
    -- Each min-id query reads its own family's parent table, so it
    -- only runs when the config enables that table.
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

    -- Per-family delete lists, each entry @(table, its FK column to
    -- the parent)@. The parent table itself goes at the end.
    let txKeyed         = childrenOf tableDefs (tdName Core.txTableDef)
        txOutKeyed      = childrenOf tableDefs (tdName UTxO.txOutTableDef)
        poolUpdateKeyed = childrenOf tableDefs (tdName Pool.poolUpdateTableDef)
        proposalKeyed   = childrenOf tableDefs (tdName Gov.govActionProposalTableDef)
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

      -- gov_action_proposal is tx-keyed too, so the rows it owns go
      -- first. committee_member reaches the proposal through its
      -- committee, so it precedes the one-hop tables.
      for_ mMinTxId $ \minTxId -> do
        when (hasTable Gov.committeeMemberTableDef) $
          void $ runSess ("delete " <> tdName Gov.committeeMemberTableDef)
            (minTxId, deleteCommitteeMembersByProposalTxStmt)
        for_ proposalKeyed $ \(tbl, col) ->
          void $ runSess ("delete " <> tbl)
            (minTxId, deleteByProposalTxStmt tbl col)

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
      -- position. A disabled extractor leaves its table out of
      -- tableDefs. A target with no epoch_no, a Byron EBB, skips all
      -- of them.
      for_ mTargetEpoch $ \targetEpoch ->
        for_ epochKeyedColumns $ \(c, anchor) ->
          when (hasTable (tcTable c)) $
            void $ runSess ("delete " <> tdName (tcTable c))
              (targetEpoch, deleteEpochRowsStmt anchor c)

      -- The target block becomes the new chain tip.
      void $ runSess "writeSyncStateSlotStmt"
        ((rawSlot, targetBlockNo, rawHash), writeSyncStateSlotStmt)

-- | Roll back to the nearest block at or after a slot number. The
-- CLI gives a bare slot, and a Cardano slot can hold no block, so
-- this picks the smallest block with @slot_no >= targetSlot@ and
-- hands it to 'rollbackToPoint'.
--
-- 'Nothing' means the database holds no block at or after the
-- target, so it already sits below the requested point.
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
          -- Use the resolved block's slot, not the requested one:
          -- 'rollbackToPoint' looks the block up by @(slot, hash)@,
          -- and only the on-chain slot matches.
          point = BlockPoint (SlotNo resolvedSlot) hash
      rollbackToPoint tableDefs point
      pure (Just resolvedBlockNo)

-- | A failure raises 'AppDatabaseError' tagged with the label.
runSess
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => Text                          -- ^ caller label
  -> (a, Stmt.Statement a b)
  -> m b
runSess label (params, stmt) = do
  conn <- asks getHasqlConnection
  useConn ("rollbackToPoint: " <> label) conn (Sess.statement params stmt)
