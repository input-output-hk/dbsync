{-# LANGUAGE OverloadedStrings #-}

-- | Byron block conversion.
--
-- Byron blocks have a completely different structure from Shelley+ blocks.
-- They use a different crypto library, have no VRF\/OpCert, and include
-- Epoch Boundary Blocks (EBBs) which have no slot number or transactions.
--
-- Ported from @Cardano.DbSync.Era.Byron.Util@ and @Byron.Insert@ in the
-- original cardano-db-sync.
module DbSync.Block.Parser.Byron
  ( fromByronBlock
  ) where

import Cardano.Prelude

import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.Slotting as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Chain.Update as Byron
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Crypto.Wallet as Crypto
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Encoding as Text
import Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))

import Cardano.Binary (serialize')

import DbSync.Block.Parser.Types (EpochSlotInfo (..))
import DbSync.Block.Types
  ( BlockEra (..)
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxIn (..)
  , GenericTxOut (..)
  )

-- ---------------------------------------------------------------------------
-- * Top-level dispatch
-- ---------------------------------------------------------------------------

-- | Convert a 'ByronBlock' into a 'GenericBlock'.
-- Dispatches between regular blocks and Epoch Boundary Blocks (EBBs).
fromByronBlock :: EpochSlotInfo -> ByronBlock -> GenericBlock
fromByronBlock esi blk =
  case byronBlockRaw blk of
    Byron.ABOBBlock ablk    -> fromByronRegularBlock esi ablk
    Byron.ABOBBoundary abblk -> fromByronEBB esi abblk

-- ---------------------------------------------------------------------------
-- * Regular Byron block
-- ---------------------------------------------------------------------------

fromByronRegularBlock :: EpochSlotInfo -> Byron.ABlock ByteString -> GenericBlock
fromByronRegularBlock esi blk =
  let slotNo = SlotNo (byronSlotNumber blk)
      pv = Byron.headerProtocolVersion (Byron.blockHeader blk)
      txs = Byron.unTxPayload (Byron.bodyTxPayload (Byron.blockBody blk))
  in GenericBlock
    { blkEra           = Byron
    , blkHash          = byronRegularBlockHash blk
    , blkPreviousHash  = byronPreviousHash blk
    , blkSlotNo        = slotNo
    , blkBlockNo       = BlockNo (byronBlockNumber blk)
    , blkEpochNo       = esiSlotToEpochNo esi slotNo
    , blkEpochSlotNo   = esiSlotToEpochSlot esi slotNo
    , blkSize          = fromIntegral (Byron.blockLength blk)
    , blkTime          = esiSlotToUTCTime esi slotNo
    , blkSlotLeader    = byronSlotLeaderHash blk
    , blkProtoMajor    = fromIntegral (Byron.pvMajor pv)
    , blkProtoMinor    = fromIntegral (Byron.pvMinor pv)
    , blkVrfKey        = Nothing   -- Byron has no VRF
    , blkOpCert        = Nothing   -- Byron has no operational certificates
    , blkOpCertCounter = Nothing
    , blkTxs           = zipWith (fromByronTx blk) [0 ..] txs
    }

-- ---------------------------------------------------------------------------
-- * Epoch Boundary Block (EBB)
-- ---------------------------------------------------------------------------

-- | EBBs are Byron-era artifacts with no transactions and no real slot number.
-- We use slot 0 as a placeholder and the epoch from the EBB header.
fromByronEBB :: EpochSlotInfo -> Byron.ABoundaryBlock ByteString -> GenericBlock
fromByronEBB esi blk =
  let slotNo = SlotNo 0  -- EBBs don't have a real slot
  in GenericBlock
    { blkEra           = Byron
    , blkHash          = Crypto.abstractHashToBytes (Byron.boundaryHashAnnotated blk)
    , blkPreviousHash  = byronEbbPrevHash blk
    , blkSlotNo        = slotNo
    , blkBlockNo       = BlockNo 0  -- EBBs don't have a block number
    , blkEpochNo       = esiSlotToEpochNo esi slotNo
    , blkEpochSlotNo   = 0
    , blkSize          = fromIntegral (Byron.boundaryBlockLength blk)
    , blkTime          = esiSlotToUTCTime esi slotNo
    , blkSlotLeader    = BS.replicate 28 '\0'  -- synthetic null leader for EBBs
    , blkProtoMajor    = 0
    , blkProtoMinor    = 0
    , blkVrfKey        = Nothing
    , blkOpCert        = Nothing
    , blkOpCertCounter = Nothing
    , blkTxs           = []  -- EBBs have no transactions
    }

-- ---------------------------------------------------------------------------
-- * Byron transaction extraction
-- ---------------------------------------------------------------------------

-- | Convert a Byron 'TxAux' into a 'GenericTx'.
-- Fee is computed as @sum(inputs) - sum(outputs)@ but since we don't have
-- UTxO lookups during parsing, we set fee to 0 and compute it later.
fromByronTx :: Byron.ABlock ByteString -> Word64 -> Byron.TxAux -> GenericTx
fromByronTx _parentBlk blockIndex txAux =
  let tx = Byron.taTx txAux
      outputs = toList (Byron.txOutputs tx)
      inputs = toList (Byron.txInputs tx)
      outSum = sum $ map (Byron.lovelaceToInteger . Byron.txOutValue) outputs
  in GenericTx
    { txHash             = Crypto.abstractHashToBytes (Crypto.serializeCborHash tx)
    , txBlockIndex       = blockIndex
    , txSize             = fromIntegral $ BS.length (serialize' tx)
    , txFee              = 0  -- Byron fees require UTxO lookup; set 0, compute later
    , txOutSum           = fromIntegral outSum
    , txValidContract    = True  -- no Plutus in Byron
    , txScriptSize       = 0
    , txTreasuryDonation = 0
    , txInvalidBefore    = Nothing
    , txInvalidHereafter = Nothing
    , txInputs           = map fromByronTxIn inputs
    , txOutputs          = zipWith fromByronTxOut [0 ..] outputs
    , txCollateralInputs = []
    , txReferenceInputs  = []
    , txCollateralOutput = Nothing
    , txCertificates     = []
    , txWithdrawals      = []
    , txMetadata         = Nothing
    , txMint             = []
    }

fromByronTxIn :: Byron.TxIn -> GenericTxIn
fromByronTxIn (Byron.TxInUtxo txId idx) =
  GenericTxIn
    { txInHash  = Crypto.abstractHashToBytes txId
    , txInIndex = fromIntegral idx
    }

fromByronTxOut :: Word16 -> Byron.TxOut -> GenericTxOut
fromByronTxOut idx txOut =
  GenericTxOut
    { txOutIndex       = idx
    , txOutAddress     = Text.decodeUtf8 (Byron.addrToBase58 (Byron.txOutAddress txOut))
    , txOutAddressRaw  = serialize' (Byron.txOutAddress txOut)
    , txOutValue       = fromIntegral (Byron.lovelaceToInteger (Byron.txOutValue txOut))
    , txOutDataHash    = Nothing
    , txOutInlineDatum = Nothing
    , txOutRefScript   = Nothing
    , txOutMultiAssets  = []
    }

-- ---------------------------------------------------------------------------
-- * Byron-specific helpers
-- ---------------------------------------------------------------------------

byronRegularBlockHash :: Byron.ABlock ByteString -> ByteString
byronRegularBlockHash = Crypto.abstractHashToBytes . Byron.blockHashAnnotated

byronBlockNumber :: Byron.ABlock ByteString -> Word64
byronBlockNumber = Byron.unChainDifficulty . Byron.headerDifficulty . Byron.blockHeader

byronSlotNumber :: Byron.ABlock ByteString -> Word64
byronSlotNumber = Byron.unSlotNumber . Byron.headerSlot . Byron.blockHeader

byronPreviousHash :: Byron.ABlock a -> ByteString
byronPreviousHash = Crypto.abstractHashToBytes . Byron.headerPrevHash . Byron.blockHeader

byronEbbPrevHash :: Byron.ABoundaryBlock a -> ByteString
byronEbbPrevHash bblock =
  case Byron.boundaryPrevHash (Byron.boundaryHeader bblock) of
    Left gh  -> Crypto.abstractHashToBytes (Byron.unGenesisHash gh)
    Right hh -> Crypto.abstractHashToBytes hh

-- | Slot leader hash: genesis key -> XPub -> hash -> take 28 bytes.
byronSlotLeaderHash :: Byron.ABlock ByteString -> ByteString
byronSlotLeaderHash =
  BS.take 28
    . Crypto.abstractHashToBytes
    . Crypto.hashRaw
    . LBS.fromStrict
    . Crypto.xpubPublicKey
    . Crypto.unVerificationKey
    . Byron.headerGenesisKey
    . Byron.blockHeader

-- | Slot leader description for Byron blocks.
_byronSlotLeaderDesc :: Byron.ABlock ByteString -> Text
_byronSlotLeaderDesc blk =
  "ByronGenesisKey-" <> Text.decodeUtf8 (Base16.encode (BS.take 8 (byronSlotLeaderHash blk)))
