{-# LANGUAGE OverloadedStrings #-}

-- | Writes transaction outputs and inputs into @tx_out@, @tx_in@,
-- @collateral_tx_in@ and @reference_tx_in@.
--
-- 'IngestChainHistory' leaves @tx_in.tx_out_id@ NULL and stores only
-- the spent tx hash and output index. A SQL join in
-- 'PreparingForVolatileTail' resolves the FK after the load.
module DbSync.Extractor.UTxO
  ( utxoExtractor

    -- * Internal helpers (exported for tests)
  , extractPaymentCred
  , extractStakeCred
  , mkTxOut
  , rawHasScript
  ) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import Data.List (zip3)

import DbSync.Parser.Types (CredHash (..), GenericTx (..), GenericTxDatum (..), GenericTxIn (..))
import qualified DbSync.Parser.Types as G
import DbSync.Phase.Type (SyncPhase, isFollowPath)
import DbSync.Db.Schema.Address (addressFromRaw, addressTableDef, extractPaymentCred, rawHasScript)
import DbSync.Db.Schema.Ids (AddressId, DatumId, RedeemerId, ScriptId, StakeAddressId, TxId (..))
import DbSync.Db.Schema.ScriptsDatums (Datum (..))
import DbSync.Db.Schema.UTxO
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Extractor (ExtractorDef (..), ProcessBlockFn, BlockContext (..), TxContext (..), redeemerIdAt)
import DbSync.Extractor.SharedDedup (resolveAndWriteDatum, resolveAndWriteTxScript, resolveStakeCred)
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.Writer (HasWriter (..), Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

utxoExtractor :: ExtractorDef
utxoExtractor = ExtractorDef
  { pdName    = "utxo"
  , pdTables  =
      [ addressTableDef
      , txOutTableDef
      , txInTableDef
      , collateralTxInTableDef
      , collateralTxOutTableDef
      , referenceTxInTableDef
      ]
  , pdProcess = processUTxO
  }

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

processUTxO :: ProcessBlockFn
processUTxO ctx = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  let phase      = bcSyncPhase ctx
      followPath = isFollowPath phase
  forM_ (bcTxs ctx) $ \tc -> do
    let txId    = tcTxId tc
        gtx     = tcGenTx tc
        outIds  = tcOutIds tc
        stakeIds = tcOutStakeIds tc

    let valid = G.txValidContract gtx

    -- For a valid tx these are its real outputs; for a failed phase-2 tx
    -- the parser has already substituted the single collateral-return
    -- output (numbered at the tx's output count), so the chain's surviving
    -- output lands in tx_out either way. 'stakeIds' are pre-resolved by the
    -- pipeline so the address record and the tx_out row share the same
    -- StakeAddressId.
    forM_ (zip3 outIds stakeIds (txOutputs gtx)) $ \(outId, mStakeId, gout) -> do
      let raw = G.txOutAddressRaw gout
      mAid       <- followAddressId phase resolver raw mStakeId
      mInlineId  <- resolveInlineDatum txId gout
      mRefSid    <- resolveRefScript txId gout
      liftIO $ writeTxOut writer outId (mkTxOut txId mAid mStakeId mInlineId mRefSid gout)
      unless followPath $
        liftIO $ recordTxOutAddress resolver outId raw mStakeId

    -- For a valid tx these are its regular inputs; for a failed phase-2 tx
    -- the parser has already substituted the collateral inputs, since those
    -- are what the chain actually consumed.
    forM_ (txInputs gtx) $ \gin -> do
      mProducer <- liftIO $ resolveInputUtxo resolver
        (txInHash gin) (txInIndex gin)
      liftIO $ writeTxIn writer
        (mkTxIn txId gin (producerTxIdFrom mProducer)
          (redeemerIdAt tc (txInRedeemerIx gin)))
      for_ mProducer $ \(_, producerOutId, _) -> do
        liftIO $ recordConsumed resolver producerOutId txId
        liftIO $ deleteCachedUtxo resolver (txInHash gin) (txInIndex gin)

    -- The remaining child tables only exist for a valid tx: a failed
    -- phase-2 tx's collateral return / inputs were folded into tx_out /
    -- tx_in above, and it commits no reference inputs.
    when valid $ do
      forM_ (txReferenceInputs gtx) $ \gin -> do
        mProducer <- liftIO $ resolveInputUtxo resolver
          (txInHash gin) (txInIndex gin)
        liftIO $ writeReferenceTxIn writer
          (mkReferenceTxIn txId gin (producerTxIdFrom mProducer))

      forM_ (txCollateralOutput gtx) $ \gout -> do
        outId <- liftIO $ assignCollateralTxOutId resolver
        mStakeId <- resolveCollateralStake gout
        let raw = G.txOutAddressRaw gout
        mAid      <- followAddressId phase resolver raw mStakeId
        mInlineId <- resolveInlineDatum txId gout
        mRefSid   <- resolveRefScript txId gout
        liftIO $ writeCollateralTxOut writer outId (mkCollateralTxOut txId mAid mStakeId mInlineId mRefSid gout)
        unless followPath $
          liftIO $ recordCollateralTxOutAddress resolver outId raw mStakeId

      forM_ (txCollateralInputs gtx) $ \gin -> do
        mProducer <- liftIO $ resolveInputUtxo resolver
          (txInHash gin) (txInIndex gin)
        liftIO $ writeCollateralTxIn writer
          (mkCollateralTxIn txId gin (producerTxIdFrom mProducer))
  where
    -- Resolve the inline stake credential of a collateral-return
    -- output, if its address carries one. Reads resolver/writer/network
    -- from env via 'resolveStakeCred'.
    resolveCollateralStake gout =
      case extractStakeCred (G.txOutAddressRaw gout) of
        Nothing  -> pure Nothing
        Just cred ->
          Just <$> resolveStakeCred cred

    -- Write the deduplicated 'datum' row for a Babbage+ inline datum and
    -- return its id for the @inline_datum_id@ FK. Hash-only outputs carry
    -- their hash in @data_hash@ only and yield 'Nothing' here.
    resolveInlineDatum txId gout =
      case G.txOutInlineDatum gout of
        Nothing  -> pure Nothing
        Just gtd -> Just <$> resolveAndWriteDatum (gtdHash gtd) (mkDatum txId gtd)

    -- Write the deduplicated 'script' row for a Babbage+ output
    -- reference script and return its id for @reference_script_id@.
    -- The id is forced before entering the row: ScriptId is a newtype,
    -- so an unforced id would smuggle the resolver's closure into the
    -- long-lived row buffers.
    resolveRefScript txId gout =
      case G.txOutRefScript gout of
        Nothing  -> pure Nothing
        Just gts -> do
          !sid <- resolveAndWriteTxScript txId gts
          pure (Just sid)

    -- Extract just the producer tx_id (for tx_in.tx_out_id) from the
    -- cache lookup's full result.
    producerTxIdFrom = fmap (\(producerTxId, _, _) -> producerTxId)

-- | Follow resolves @address_id@ at once, so the @tx_out@ or
-- @collateral_tx_out@ row carries the FK from the start. Ingest gives
-- 'Nothing': the row goes in with @address_id = NULL@, and the async
-- worker fills it with a bulk UPDATE an epoch later.
followAddressId
  :: MonadIO m
  => SyncPhase -> IdResolver IO -> ByteString -> Maybe StakeAddressId -> m (Maybe AddressId)
followAddressId phase resolver raw mStakeId
  | isFollowPath phase = Just <$> liftIO (resolveAddressId resolver raw (addressFromRaw raw mStakeId))
  | otherwise          = pure Nothing

-- ---------------------------------------------------------------------------
-- * Record builders
-- ---------------------------------------------------------------------------

mkTxOut
  :: TxId
  -> Maybe AddressId
  -> Maybe StakeAddressId
  -> Maybe DatumId
  -> Maybe ScriptId
  -> G.GenericTxOut
  -> TxOut
mkTxOut txId addrId mStakeId mInlineId mRefScriptId gout = TxOut
  { txOutTxId              = txId
  , txOutIndex             = fromIntegral (G.txOutIndex gout)
  , txOutAddressId         = addrId  -- 'Nothing' until the AddressResolver worker fills it in
  , txOutStakeAddressId    = mStakeId
  , txOutValue             = DbLovelace (G.txOutValue gout)
  , txOutDataHash          = G.txOutDataHash gout
  , txOutInlineDatumId     = mInlineId
  , txOutReferenceScriptId = mRefScriptId
  , txOutConsumedByTxId    = Nothing  -- resolved post-load
  }

mkDatum :: TxId -> GenericTxDatum -> Datum
mkDatum txId gtd = Datum
  { datumHash  = gtdHash gtd
  , datumTxId  = txId
  , datumValue = gtdValue gtd
  , datumBytes = gtdBytes gtd
  }

-- | The @tx_out_id@ argument is 'Just' when the cache resolved the
-- producer. The post-load resolve fills the remaining NULLs.
mkTxIn :: TxId -> GenericTxIn -> Maybe TxId -> Maybe RedeemerId -> TxIn
mkTxIn txId gin mTxOutId mRedeemerId = TxIn
  { txInTxInId     = txId
  , txInTxOutId    = mTxOutId
  , txInTxOutIndex = fromIntegral (txInIndex gin)
  , txInTxOutHash  = txInHash gin
  , txInRedeemerId = mRedeemerId
  }

mkCollateralTxIn :: TxId -> GenericTxIn -> Maybe TxId -> CollateralTxIn
mkCollateralTxIn txId gin mTxOutId = CollateralTxIn
  { collateralTxInTxInId     = txId
  , collateralTxInTxOutId    = mTxOutId
  , collateralTxInTxOutIndex = fromIntegral (txInIndex gin)
  , collateralTxInTxOutHash  = txInHash gin
  }

mkReferenceTxIn :: TxId -> GenericTxIn -> Maybe TxId -> ReferenceTxIn
mkReferenceTxIn txId gin mTxOutId = ReferenceTxIn
  { referenceTxInTxInId     = txId
  , referenceTxInTxOutId    = mTxOutId
  , referenceTxInTxOutIndex = fromIntegral (txInIndex gin)
  , referenceTxInTxOutHash  = txInHash gin
  }

mkCollateralTxOut
  :: TxId
  -> Maybe AddressId
  -> Maybe StakeAddressId
  -> Maybe DatumId
  -> Maybe ScriptId
  -> G.GenericTxOut
  -> CollateralTxOut
mkCollateralTxOut txId addrId mStakeId mInlineId mRefScriptId gout = CollateralTxOut
  { collateralTxOutTxId              = txId
  , collateralTxOutIndex             = fromIntegral (G.txOutIndex gout)
  , collateralTxOutAddressId         = addrId  -- 'Nothing' until the AddressResolver worker fills it in
  , collateralTxOutStakeAddressId    = mStakeId
  , collateralTxOutValue             = DbLovelace (G.txOutValue gout)
  , collateralTxOutDataHash          = G.txOutDataHash gout
    -- A collateral-return output cannot carry multi-assets, but the
    -- column holds a textual rendering of whatever the body declared.
    -- A failed tx always gives @[]@ here.
  , collateralTxOutMultiAssetsDescr  = show (G.txOutMultiAssets gout)
  , collateralTxOutInlineDatumId     = mInlineId
  , collateralTxOutReferenceScriptId = mRefScriptId
  }

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | Returns 'Just' for a base address, where CIP-19 header types
-- @0x00@, @0x10@, @0x20@ and @0x30@ put the stake key or script hash
-- in bytes 30-57. Header bit @0x20@ marks that credential as a script.
-- Pointer, enterprise, reward and Byron addresses carry no inline
-- credential, so they give 'Nothing'.
extractStakeCred :: ByteString -> Maybe CredHash
extractStakeCred bs =
  case BS.uncons bs of
    Just (header, _) | BS.length bs >= 57 ->
      let typeBits = header .&. 0xF0
      in if typeBits == 0x00
           || typeBits == 0x10
           || typeBits == 0x20
           || typeBits == 0x30
           then Just (CredHash (BS.take 28 (BS.drop 29 bs)) (header .&. 0x20 /= 0))
           else Nothing
    _ -> Nothing
