{-# LANGUAGE OverloadedStrings #-}

-- | UTxO extractor.
--
-- Extracts transaction outputs and inputs into @tx_out@, @tx_in@,
-- @collateral_tx_in@, and @reference_tx_in@ tables.
--
-- During 'IngestChainHistory', @tx_in.tx_out_id@ is NULL — only
-- the spent tx hash and output index are stored. The FK is resolved
-- post-load via a SQL join in 'PreparingForVolatileTail'.
module DbSync.Extractor.UTxO
  ( utxoExtractor

    -- * Internal helpers (exported for tests)
  , extractPaymentCred
  , extractStakeCred
  , mkAddress
  , mkTxOut
  , rawHasScript
  ) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import Data.List (zip3)

import DbSync.Block.Types (GenericTx (..), GenericTxIn (..))
import qualified DbSync.Block.Types as G
import DbSync.Db.Schema.Address (Address (..), addressTableDef)
import DbSync.Db.Schema.Ids (AddressId, StakeAddressId, TxId (..))
import DbSync.Db.Schema.UTxO
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Extractor (ExtractorDef (..), ProcessBlockFn, BlockContext (..), TxContext (..))
import DbSync.Extractor.SharedDedup (resolveAndWriteStakeAddress)
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.Writer (HasWriter (..), Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

utxoExtractor :: ExtractorDef
utxoExtractor = ExtractorDef
  { pdName         = "utxo"
  , pdVersion      = 1
  , pdDependencies = [("core", 1), ("stake_delegation", 1)]
  , pdTables       =
      [ addressTableDef
      , txOutTableDef
      , txInTableDef
      , collateralTxInTableDef
      , collateralTxOutTableDef
      , referenceTxInTableDef
      ]
  , pdProcess      = processUTxO
  }

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

processUTxO :: ProcessBlockFn
processUTxO ctx = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  forM_ (bcTxs ctx) $ \tc -> do
    let txId    = tcTxId tc
        gtx     = tcGenTx tc
        outIds  = tcOutIds tc
        stakeIds = tcOutStakeIds tc

    if G.txValidContract gtx
      then do
        -- Pipeline pre-resolves @stakeIds@ so the address record and
        -- the tx_out row share the same StakeAddressId.
        --
        -- Each output appends its raw address + derived fields to the
        -- per-epoch address buffer via 'recordTxOutAddress'; the
        -- background 'AddressResolver' worker fills in
        -- @tx_out.address_id@ an epoch later.
        forM_ (zip3 outIds stakeIds (txOutputs gtx)) $ \(outId, mStakeId, gout) -> do
          let raw  = G.txOutAddressRaw gout
              addr = mkAddress mStakeId gout
          -- Write the tx_out row with @address_id = Nothing@ first,
          -- then hand the (id, raw, derived) tuple to the resolver.
          -- The Follow resolver runs an UPDATE to fill in @address_id@
          -- synchronously; the Ingest resolver appends the tuple to
          -- a per-epoch buffer the worker drains an epoch later.
          liftIO $ writeTxOut writer outId (mkTxOut txId Nothing mStakeId gout)
          liftIO $ recordTxOutAddress resolver outId raw addr

        forM_ (txInputs gtx) $ \gin -> do
          inId <- liftIO $ assignTxInId resolver
          liftIO $ writeTxIn writer inId (mkTxIn txId gin)

        forM_ (txReferenceInputs gtx) $ \gin -> do
          inId <- liftIO $ assignReferenceTxInId resolver
          liftIO $ writeReferenceTxIn writer inId (mkReferenceTxIn txId gin)
      else
        -- Phase-2 failure: the chain only records the collateral
        -- inputs (consumed) and the optional collateral return.
        -- Regular inputs / outputs / reference inputs do not exist
        -- on-chain for a failed tx.
        forM_ (txCollateralOutput gtx) $ \gout -> do
          outId <- liftIO $ assignCollateralTxOutId resolver
          mStakeId <- resolveCollateralStake gout
          let raw  = G.txOutAddressRaw gout
              addr = mkAddress mStakeId gout
          liftIO $ writeCollateralTxOut writer outId (mkCollateralTxOut txId Nothing mStakeId gout)
          liftIO $ recordCollateralTxOutAddress resolver outId raw addr

    -- Collateral inputs are written for every tx — valid txs record them
    -- as a script-witness commitment, failed txs record them as the
    -- inputs that were actually consumed.
    forM_ (txCollateralInputs gtx) $ \gin -> do
      inId <- liftIO $ assignCollateralTxInId resolver
      liftIO $ writeCollateralTxIn writer inId (mkCollateralTxIn txId gin)
  where
    -- Resolve the inline stake credential of a collateral-return
    -- output, if its address carries one. Reads resolver/writer/network
    -- from env via 'resolveAndWriteStakeAddress'.
    resolveCollateralStake gout =
      case extractStakeCred (G.txOutAddressRaw gout) of
        Nothing  -> pure Nothing
        Just cred ->
          Just <$> resolveAndWriteStakeAddress cred

-- ---------------------------------------------------------------------------
-- * Record builders
-- ---------------------------------------------------------------------------

mkAddress :: Maybe StakeAddressId -> G.GenericTxOut -> Address
mkAddress mStakeId gout = Address
  { addressAddress        = G.txOutAddress gout
  , addressRaw            = G.txOutAddressRaw gout
  , addressHasScript      = rawHasScript (G.txOutAddressRaw gout)
  , addressPaymentCred    = extractPaymentCred (G.txOutAddressRaw gout)
  , addressStakeAddressId = mStakeId
  }

mkTxOut :: TxId -> Maybe AddressId -> Maybe StakeAddressId -> G.GenericTxOut -> TxOut
mkTxOut txId addrId mStakeId gout = TxOut
  { txOutTxId              = txId
  , txOutIndex             = fromIntegral (G.txOutIndex gout)
  , txOutAddressId         = addrId  -- 'Nothing' until the AddressResolver worker fills it in
  , txOutStakeAddressId    = mStakeId
  , txOutValue             = DbLovelace (G.txOutValue gout)
  , txOutDataHash          = G.txOutDataHash gout
  , txOutInlineDatumId     = Nothing  -- resolved by ScriptsDatums extractor
  , txOutReferenceScriptId = Nothing  -- resolved by ScriptsDatums extractor
  , txOutConsumedByTxId    = Nothing  -- resolved post-load
  }

mkTxIn :: TxId -> GenericTxIn -> TxIn
mkTxIn txId gin = TxIn
  { txInTxInId     = txId
  , txInTxOutId    = Nothing  -- deferred: resolved post-load via SQL join
  , txInTxOutIndex = fromIntegral (txInIndex gin)
  , txInTxOutHash  = txInHash gin
  , txInRedeemerId = Nothing  -- resolved by ScriptsDatums extractor
  }

mkCollateralTxIn :: TxId -> GenericTxIn -> CollateralTxIn
mkCollateralTxIn txId gin = CollateralTxIn
  { collateralTxInTxInId     = txId
  , collateralTxInTxOutId    = Nothing
  , collateralTxInTxOutIndex = fromIntegral (txInIndex gin)
  , collateralTxInTxOutHash  = txInHash gin
  }

mkReferenceTxIn :: TxId -> GenericTxIn -> ReferenceTxIn
mkReferenceTxIn txId gin = ReferenceTxIn
  { referenceTxInTxInId     = txId
  , referenceTxInTxOutId    = Nothing
  , referenceTxInTxOutIndex = fromIntegral (txInIndex gin)
  , referenceTxInTxOutHash  = txInHash gin
  }

mkCollateralTxOut
  :: TxId -> Maybe AddressId -> Maybe StakeAddressId -> G.GenericTxOut -> CollateralTxOut
mkCollateralTxOut txId addrId mStakeId gout = CollateralTxOut
  { collateralTxOutTxId              = txId
  , collateralTxOutIndex             = fromIntegral (G.txOutIndex gout)
  , collateralTxOutAddressId         = addrId  -- 'Nothing' until the AddressResolver worker fills it in
  , collateralTxOutStakeAddressId    = mStakeId
  , collateralTxOutValue             = DbLovelace (G.txOutValue gout)
  , collateralTxOutDataHash          = G.txOutDataHash gout
    -- The collateral-return output cannot carry multi-assets, but
    -- the original schema records a textual rendering of whatever
    -- the body declared. Failed txs always produce @[]@ here.
  , collateralTxOutMultiAssetsDescr  = show (G.txOutMultiAssets gout)
  , collateralTxOutInlineDatumId     = Nothing
  , collateralTxOutReferenceScriptId = Nothing
  }

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | Check if an address contains a script (bit 4 of header byte).
-- Shelley+ addresses encode this in the header. Byron addresses
-- never contain scripts.
rawHasScript :: ByteString -> Bool
rawHasScript bs
  | BS.null bs = False
  | otherwise  =
      let header = BS.head bs
      in (header .&. 0x10) /= 0  -- bit 4 set = script address

-- | Extract the 28-byte payment credential from a Shelley address
-- (bytes 1..28 after the header).
--
-- Returns 'Just' for the eight Shelley address types that carry a
-- payment credential (base/pointer/enterprise, header high nibble
-- 0x0..0x7), and 'Nothing' for everything else:
--
--   * Byron raws — CBOR-wrapped, not Shelley header+payload, so
--     bytes 1..28 would be CBOR-frame bytes rather than a credential.
--   * Reward addresses (0xE0/0xF0) — those bytes are the stake hash,
--     not a payment credential.
--   * Anything shorter than 29 bytes.
extractPaymentCred :: ByteString -> Maybe ByteString
extractPaymentCred bs
  | BS.length bs < 29 = Nothing
  | otherwise =
      let typeBits = BS.head bs .&. 0xF0
      in if typeBits <= 0x70
           then Just (BS.take 28 (BS.drop 1 bs))
           else Nothing

-- | Extract the inline 28-byte stake credential from a Shelley address.
--
-- Returns 'Just' for base addresses (header types @0x00@\/@0x10@\/@0x20@\/@0x30@,
-- per CIP-19) where bytes 30-57 carry the stake key or script hash.
-- Pointer, enterprise, reward, and Byron addresses have no inline cred
-- and yield 'Nothing'.
extractStakeCred :: ByteString -> Maybe ByteString
extractStakeCred bs
  | BS.length bs < 57 = Nothing
  | otherwise =
      let typeBits = BS.head bs .&. 0xF0
      in if typeBits == 0x00
           || typeBits == 0x10
           || typeBits == 0x20
           || typeBits == 0x30
           then Just (BS.take 28 (BS.drop 29 bs))
           else Nothing
