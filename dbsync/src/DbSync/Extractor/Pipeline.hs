{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Assigns the shared ids centrally, builds a 'BlockContext', then
-- runs every enabled extractor. 'IngestChainHistory' and
-- 'FollowingChainTip' behave the same here; only the 'IdResolver' and
-- 'Writer' the env carries differ.
module DbSync.Extractor.Pipeline
  ( -- * Processing
    processBlock
  ) where

import Cardano.Prelude

import qualified Data.Sequence as Seq

import DbSync.Parser.Types (BlockEra (..), GenericBlock (..), GenericTx (..), GenericTxOut (..))
import DbSync.Db.Types (DbLovelace (..))
import DbSync.App.Env (HasNetwork (..))
import DbSync.Extractor
  ( BlockContext (..)
  , ExtractorDef (..)
  , HasExtractors (..)
  , HasLedgerData (..)
  , TxContext (..)
  , scriptsDatumsEnabled
  , utxoEnabled
  )
import DbSync.Extractor.Core (mkSlotLeader)
import DbSync.Extractor.SharedDedup (resolveStakeCred)
import DbSync.Extractor.UTxO (extractStakeCred)
import DbSync.Db.Schema.Ids (PoolHashId, StakeAddressId)
import DbSync.Phase.Current (HasCurrentPhase (..))
import DbSync.Phase.Ingest.UtxoStore (UtxoTxEntry (..))
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.Writer (HasWriter (..))

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

-- | Assigns 'BlockId', 'SlotLeaderId', per-tx 'TxId', per-output
-- 'TxOutId' and per-redeemer 'RedeemerId', puts them on a
-- 'BlockContext', then calls each extractor's 'pdProcess'. Extractors
-- read these ids, so none of them depends on the execution order.
processBlock
  :: ( MonadReader env m
     , HasResolver env
     , HasWriter env
     , HasExtractors env
     , HasNetwork env
     , HasLedgerData env
     , HasCurrentPhase env
     , MonadIO m
     )
  => GenericBlock
  -> m ()
processBlock block = do
  resolver   <- asks getResolver
  extractors <- asks getExtractors
  env        <- ask
  phase      <- liftIO $ getCurrentPhase env
  ledgerData <- liftIO $ getLedgerData env block

  -- For Shelley+ blocks the slot-leader hash IS a pool key hash;
  -- resolve it before extractors run so @slot_leader.pool_hash_id@
  -- can be set without giving @core@ a circular dependency on @pool@.
  mPoolHashId <- resolveSlotLeaderPoolHash block

  let leader = mkSlotLeader mPoolHashId block
  (slId, _isNew) <- liftIO $ resolveSlotLeader resolver (blkSlotLeader block) leader

  prevId  <- liftIO $ resolvePrevBlock resolver (blkPreviousHash block)
  blockId <- liftIO $ assignBlockId resolver

  -- Per-tx and per-output IDs plus the per-output stake-address FK.
  -- Recording each tx in the cache before extractors run lets the
  -- UTxO extractor resolve intra-block inputs (a later tx in the same
  -- block spending an earlier tx's output) without ordering games.
  -- The cache stores (tx_out.id, value) per output so the consumed-by
  -- UPDATE matches by PK rather than (tx_id, index).
  --
  -- The bang on @v@ forces the Word64 before the tuple is built. Without
  -- it the tuple's second field is a thunk that retains its captured
  -- @o :: GenericTxOut@ (and through it the raw address ByteString),
  -- pinning that memory for every tx held in the cache.
  let redeemersOn = scriptsDatumsEnabled extractors
  txCtxs <- forM (blkTxs block) $ \gtx -> do
    txId <- liftIO $ assignTxId resolver
    outIds <- forM (txOutputs gtx) $ \_ -> liftIO $ assignTxOutId resolver
    stakeIds <- forM (txOutputs gtx) resolveOutStakeId
    -- Redeemer ids are drawn only when the scripts_datums extractor
    -- will write the rows: a failed phase-2 tx materialises none, and
    -- a disabled extractor must not advance the sequence.
    redeemerIds <-
      if redeemersOn && txValidContract gtx
        then forM (txRedeemers gtx) $ \_ -> liftIO $ assignRedeemerId resolver
        else pure []
    liftIO $ recordTxOutputs resolver (txHash gtx) UtxoTxEntry
      { uteTxId    = txId
      , uteOutputs = Seq.fromList
          [ let !v = txOutValue o
            in (outId, DbLovelace v)
          | (outId, o) <- zip outIds (txOutputs gtx)
          ]
      }
    pure $ TxContext txId gtx outIds stakeIds redeemerIds

  network <- asks getNetwork
  let ctx = BlockContext
        { bcBlockId              = blockId
        , bcSlotLeaderId         = slId
        , bcSlotLeaderNew        = _isNew
        , bcSlotLeaderPoolHashId = mPoolHashId
        , bcPrevBlockId          = prevId
        , bcGenBlock             = block
        , bcTxs                  = txCtxs
        , bcNetwork              = network
        , bcLedgerData           = ledgerData
        , bcSyncPhase            = phase
        }

  forM_ extractors $ \ext -> pdProcess ext ctx

  -- A spend redeemer's script hash is the payment credential of the
  -- output it unlocks, so the parser leaves it empty. Filling it needs
  -- both the @redeemer@ rows and the @tx_in@ rows pointing at them,
  -- hence the position after every extractor has run.
  let blockRedeemerIds = concatMap tcRedeemerIds txCtxs
  when (redeemersOn && utxoEnabled extractors && not (null blockRedeemerIds)) $
    liftIO $ fillSpendScriptHashes resolver blockRedeemerIds

-- | Byron blocks delegate slot leadership through genesis keys, not
-- pool keys, and EBBs carry a synthetic null leader, so both give
-- 'Nothing'. Every other block queries the registered pool hashes
-- without inserting: a genesis-key delegate misses and stays
-- 'Nothing', so it never gets a fabricated @pool_hash@ row.
resolveSlotLeaderPoolHash
  :: (HasResolver env, MonadReader env m, MonadIO m)
  => GenericBlock -> m (Maybe PoolHashId)
resolveSlotLeaderPoolHash block
  | blkEra block == Byron = pure Nothing
  | otherwise = do
      resolver <- asks getResolver
      liftIO $ resolvePoolHashQuery resolver (blkSlotLeader block)

-- | Pre-resolve the @stake_address@ FK for one tx output. It lives
-- here, not in the UTxO extractor, so the @utxo@ and
-- @stake_delegation@ extractors stay independent; the pipeline is the
-- only caller of both.
resolveOutStakeId
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => GenericTxOut -> m (Maybe StakeAddressId)
resolveOutStakeId gout =
  case extractStakeCred (txOutAddressRaw gout) of
    Nothing -> pure Nothing
    Just cred -> Just <$> resolveStakeCred cred
