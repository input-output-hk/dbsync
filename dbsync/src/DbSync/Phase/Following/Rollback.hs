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
-- The cascade tables come from each 'TableDef'\'s 'tdForeignKeys' so
-- the rollback automatically picks up new dependent tables when they
-- declare the right FK; no hand-maintained list to drift. The
-- epoch-keyed boundary tables carry no FKs and are scoped by
-- 'epochKeyedDeletes' instead.
module DbSync.Phase.Following.Rollback
  ( rollbackToPoint
  , rollbackToSlot

    -- * Schema-walk helpers (exported for tests)
  , childrenOf
  , EpochAnchor (..)
  , epochKeyedDeletes
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
import DbSync.Db.Schema.AdaPots (AdaPotsCols (..), adaPotsCols)
import qualified DbSync.Db.Schema.Core as Core
import DbSync.Db.Schema.EpochBoundary
  ( EpochParamCols (..)
  , EpochStateCols (..)
  , epochParamCols
  , epochStateCols
  )
import DbSync.Db.Schema.EpochSyncStats (EpochSyncStatsCols (..), epochSyncStatsCols)
import DbSync.Db.Schema.EpochView (EpochFinalizedCols (..), epochFinalizedCols)
import DbSync.Db.Schema.Governance (DrepDistrCols (..), drepDistrCols)
import DbSync.Db.Schema.Ids (getPoolUpdateId, getTxId, getTxOutId)
import qualified DbSync.Db.Schema.Pool as Pool
import DbSync.Db.Schema.Pool (PoolStatCols (..))
import DbSync.Db.Schema.StakeDelegation
  ( EpochStakeCols (..)
  , EpochStakeProgressCols (..)
  , PotRewardCols (..)
  , RewardCols (..)
  , epochStakeCols
  , epochStakeProgressCols
  , potRewardCols
  , rewardCols
  )
import DbSync.Db.Schema.Types (ForeignKey (..), TableColumn (..), TableDef (..))
import qualified DbSync.Db.Schema.UTxO as UTxO
import DbSync.Db.Statement.Worker.Rollback
  ( deleteBlockAfterIdStmt
  , deleteWhereEpochGtStmt
  , deleteWhereEpochGteStmt
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
-- tables are considered, and a table's outgoing FKs (the parent
-- table referenced by 'fkParentTable') decide which family it
-- belongs to. Tables that don't reference @tx@ / @tx_out@ /
-- @pool_update@ are skipped by the FK cascade; the epoch-keyed
-- boundary tables among them are cleaned via 'epochKeyedDeletes'.
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
      -- also deleted here; removing the children first keeps the
      -- parent delete safe against FK constraints.
      for_ mMinPoolUpdateId $ \minPoolUpdateId -> do
        let !i      = getPoolUpdateId minPoolUpdateId
            poolTbl = tdName Pool.poolUpdateTableDef
        for_ poolUpdateKeyed $ \(tbl, col) ->
          void $ runSess ("delete " <> tbl)
            (i, deleteWhereGteStmt tbl col)
        void $ runSess ("delete " <> poolTbl)
          (i, deleteWhereGteStmt poolTbl "id")

      -- Surviving producer rows must drop their consumed-by marks
      -- pointing at the txs deleted above, or the re-applied fork
      -- can never re-consume them.
      for_ mMinTxId $ \minTxId ->
        when (any ((== tdName UTxO.txOutTableDef) . tdName) tableDefs) $ do
          let c = UTxO.tocConsumedByTxId UTxO.txOutCols
          void $ runSess ("null " <> tdName (tcTable c) <> "." <> tcName c)
            (minTxId, nullConsumedByFromTxStmt)

      -- Finally tx and block themselves.
      let txTbl = tdName Core.txTableDef
      for_ mMinTxId $ \minTxId ->
        void $ runSess ("delete " <> txTbl)
          (getTxId minTxId, deleteWhereGteStmt txTbl "id")
      void $ runSess ("delete " <> tdName Core.blockTableDef)
        (targetBlockId, deleteBlockAfterIdStmt)

      -- Epoch-keyed boundary tables (no FKs, so the cascade above
      -- never touches them). Tables whose extractor is disabled are
      -- absent from tableDefs and skipped; a target without an
      -- epoch_no (Byron EBB) skips the lot.
      for_ mTargetEpoch $ \targetEpoch ->
        for_ epochKeyedDeletes $ \(c, anchor) ->
          when (any ((== tdName (tcTable c)) . tdName) tableDefs) $ do
            let (param, delStmt) = case anchor of
                  EnteredEpoch      -> (targetEpoch, deleteWhereEpochGtStmt c)
                  NextEpochSnapshot -> (targetEpoch + 1, deleteWhereEpochGtStmt c)
                  CompletedEpoch    -> (targetEpoch, deleteWhereEpochGteStmt c)
            void $ runSess ("delete " <> tdName (tcTable c)) (param, delStmt)

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

-- | How rows of an epoch-keyed boundary table anchor to the rollback
-- target's epoch.
--
--   * 'EnteredEpoch' — written entering the epoch they carry; the
--     target epoch's own rows stay valid, so delete @> target@.
--   * 'NextEpochSnapshot' — stake slices of the /next/ epoch's mark
--     snapshot, emitted across the preceding epoch. The snapshot for
--     @target+1@ froze before the target block, so delete
--     @> target+1@ and let the replay re-emit identical slices.
--   * 'CompletedEpoch' — describe a finished epoch; rolling back
--     into an epoch un-finishes it, so delete @>= target@.
data EpochAnchor = EnteredEpoch | NextEpochSnapshot | CompletedEpoch
  deriving stock (Eq, Show)

epochKeyedDeletes :: [(TableColumn, EpochAnchor)]
epochKeyedDeletes =
  [ (epochStateCols.esccEpochNo,         EnteredEpoch)
  , (epochParamCols.epcEpochNo,          EnteredEpoch)
  , (adaPotsCols.apcEpochNo,             EnteredEpoch)
  , (Pool.poolStatCols.pstcEpochNo,      EnteredEpoch)
  , (drepDistrCols.ddcEpochNo,           EnteredEpoch)
    -- reward rows land entering their spendable epoch; pot_reward
    -- rows land entering their earned epoch (spendable is one later).
  , (rewardCols.rcSpendableEpoch,        EnteredEpoch)
  , (potRewardCols.prcEarnedEpoch,       EnteredEpoch)
  , (epochStakeCols.escEpochNo,          NextEpochSnapshot)
  , (epochStakeProgressCols.espcEpochNo, NextEpochSnapshot)
  , (epochSyncStatsCols.esscEpochNo,     CompletedEpoch)
  , (epochFinalizedCols.efcNo,           CompletedEpoch)
  ]

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
