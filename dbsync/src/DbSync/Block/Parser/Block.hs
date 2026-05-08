{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Post-Byron block-level converters and shared helpers.
--
-- All post-Byron eras (Shelley through Dijkstra) use the consensus
-- 'ShelleyBlock' wrapper. Each @from*Block@ function converts an
-- era-specific block into our era-independent 'GenericBlock'.
--
-- The block-level converters are intentionally thin — they delegate to
-- shared helpers for header extraction and differ only in:
--
--   1. The 'BlockEra' tag
--   2. TPraos vs Praos helpers for VRF\/OpCert\/ProtVer
--   3. Which @from*Tx@ function is mapped over transactions (currently stubbed)
--
-- __First pass:__ @blkTxs = []@ — transaction extraction wired in Steps 6+7.
module DbSync.Block.Parser.Block
  ( -- * Block converters (Shelley+ eras)
    fromShelleyBlock
  , fromAllegraBlock
  , fromMaryBlock
  , fromAlonzoBlock
  , fromBabbageBlock
  , fromConwayBlock
  , fromDijkstraBlock

    -- * Shared helpers (exported for Byron module and tests)
  , blockHash
  , blockPrevHash
  , blockIssuerRaw
  , getTxs
  ) where

import Cardano.Prelude

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.KES.Class as KES
import Cardano.Crypto.VRF.Class (VerKeyVRF, rawSerialiseVerKeyVRF)
import qualified Data.ByteString as BS
import Lens.Micro ((^.))

import DbSync.Util.Bech32 (serialiseVrfVkToBech32)

import Cardano.Ledger.Keys (hashKey, unKeyHash)
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Protocol.Crypto (StandardCrypto, VRF)
import qualified Cardano.Protocol.TPraos.BHeader as TPraos
import qualified Cardano.Protocol.TPraos.OCert as TPraos

import Ouroboros.Consensus.Byron.Node ()
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Consensus
import Ouroboros.Consensus.Shelley.Protocol.Abstract
  ( ProtocolHeaderSupportsEnvelope
  , ShelleyProtocol
  , ShelleyProtocolHeader
  , pHeaderBlock
  , pHeaderBlockSize
  , pHeaderIssuer
  , pHeaderPrevHash
  )
import Ouroboros.Consensus.Cardano.Block
  ( AllegraEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra
  , DijkstraEra
  , MaryEra
  , ShelleyEra
  )
import Ouroboros.Network.Block (BlockNo)

import DbSync.Block.Parser.Tx
  ( fromShelleyTx
  , fromAllegraTx
  , fromMaryTx
  , fromAlonzoTx
  , fromBabbageTx
  , fromConwayTx
  , fromDijkstraTx
  )
import DbSync.StateQuery (SlotDetails (..))
import DbSync.Block.Types
  ( BlockEra (..)
  , GenericBlock (..)
  , GenericTx
  )

-- ---------------------------------------------------------------------------
-- * Block converters: TPraos eras (Shelley, Allegra, Mary, Alonzo)
-- ---------------------------------------------------------------------------

fromShelleyBlock :: SlotDetails -> ShelleyBlock (TPraos StandardCrypto) ShelleyEra -> GenericBlock
fromShelleyBlock = mkShelleyBlockTPraos Shelley fromShelleyTx

fromAllegraBlock :: SlotDetails -> ShelleyBlock (TPraos StandardCrypto) AllegraEra -> GenericBlock
fromAllegraBlock = mkShelleyBlockTPraos Allegra fromAllegraTx

fromMaryBlock :: SlotDetails -> ShelleyBlock (TPraos StandardCrypto) MaryEra -> GenericBlock
fromMaryBlock = mkShelleyBlockTPraos Mary fromMaryTx

fromAlonzoBlock :: SlotDetails -> ShelleyBlock (TPraos StandardCrypto) AlonzoEra -> GenericBlock
fromAlonzoBlock = mkShelleyBlockTPraos Alonzo fromAlonzoTx

-- | Shared TPraos block converter — all pre-Babbage eras use the same pattern.
--
-- In cardano-node 10.7.1 'Ledger.Tx' (a.k.a. 'Core.Tx') gained a 'TxLevel'
-- parameter.  Consensus blocks only contain top-level transactions, so we
-- use 'Ledger.TopTx' here and in 'getTxs' below.
mkShelleyBlockTPraos
  :: Ledger.EraBlockBody era
  => BlockEra
  -> ((Word64, Ledger.Tx Ledger.TopTx era) -> GenericTx)
  -> SlotDetails
  -> ShelleyBlock (TPraos StandardCrypto) era
  -> GenericBlock
mkShelleyBlockTPraos era txConvert sd blk =
  let (protoMaj, protoMin) = splitProtoVer (blockProtoVersionTPraos blk)
  in GenericBlock
    { blkEra           = era
    , blkHash          = blockHash blk
    , blkPreviousHash  = blockPrevHash blk
    , blkSlotNo        = sdSlotNo sd
    , blkBlockNo       = blockNumber blk
    , blkEpochNo       = sdEpochNo sd
    , blkEpochSlotNo   = sdEpochSlot sd
    , blkSize          = blockSize blk
    , blkTime          = sdSlotTime sd
    , blkSlotLeader    = blockIssuerRaw blk
    , blkProtoMajor    = protoMaj
    , blkProtoMinor    = protoMin
    , blkVrfKey        = Just (blockVrfKeyViewTPraos blk)
    , blkOpCert        = Just (blockOpCertRawTPraos blk)
    , blkOpCertCounter = Just (blockOpCertCounterTPraos blk)
    , blkIsEBB         = False
    , blkTxs           = map txConvert (getTxs blk)
    }

-- ---------------------------------------------------------------------------
-- * Block converters: Praos eras (Babbage, Conway, Dijkstra)
-- ---------------------------------------------------------------------------

fromBabbageBlock :: SlotDetails -> ShelleyBlock (Praos StandardCrypto) BabbageEra -> GenericBlock
fromBabbageBlock = mkShelleyBlockPraos Babbage fromBabbageTx

fromConwayBlock :: SlotDetails -> ShelleyBlock (Praos StandardCrypto) ConwayEra -> GenericBlock
fromConwayBlock = mkShelleyBlockPraos Conway fromConwayTx

fromDijkstraBlock :: SlotDetails -> ShelleyBlock (Praos StandardCrypto) DijkstraEra -> GenericBlock
fromDijkstraBlock = mkShelleyBlockPraos Dijkstra fromDijkstraTx

-- | Shared Praos block converter — Babbage+ eras use the same pattern.
mkShelleyBlockPraos
  :: Ledger.EraBlockBody era
  => BlockEra
  -> ((Word64, Ledger.Tx Ledger.TopTx era) -> GenericTx)
  -> SlotDetails
  -> ShelleyBlock (Praos StandardCrypto) era
  -> GenericBlock
mkShelleyBlockPraos era txConvert sd blk =
  let (protoMaj, protoMin) = splitProtoVer (blockProtoVersionPraos blk)
  in GenericBlock
    { blkEra           = era
    , blkHash          = blockHash blk
    , blkPreviousHash  = blockPrevHash blk
    , blkSlotNo        = sdSlotNo sd
    , blkBlockNo       = blockNumber blk
    , blkEpochNo       = sdEpochNo sd
    , blkEpochSlotNo   = sdEpochSlot sd
    , blkSize          = blockSize blk
    , blkTime          = sdSlotTime sd
    , blkSlotLeader    = blockIssuerRaw blk
    , blkProtoMajor    = protoMaj
    , blkProtoMinor    = protoMin
    , blkVrfKey        = Just (blockVrfKeyViewPraos blk)
    , blkOpCert        = Just (blockOpCertRawPraos blk)
    , blkOpCertCounter = Just (blockOpCertCounterPraos blk)
    , blkIsEBB         = False
    , blkTxs           = map txConvert (getTxs blk)
    }

-- ---------------------------------------------------------------------------
-- * Shared block helpers (protocol-agnostic)
-- ---------------------------------------------------------------------------

-- | Extract the block header from a 'ShelleyBlock'.
blockHeader :: ShelleyBlock p era -> ShelleyProtocolHeader p
blockHeader = Ledger.blockHeader . Consensus.shelleyBlockRaw

-- | Block header hash as raw bytes (32 bytes).
blockHash :: ShelleyBlock p era -> ByteString
blockHash =
  Crypto.hashToBytes
    . Consensus.unShelleyHash
    . Consensus.shelleyBlockHeaderHash

-- | Previous block hash as raw bytes.
-- Returns empty 'ByteString' for the first block after genesis.
blockPrevHash :: ProtocolHeaderSupportsEnvelope p => ShelleyBlock p era -> ByteString
blockPrevHash blk =
  case pHeaderPrevHash (blockHeader blk) of
    TPraos.GenesisHash                      -> BS.empty
    TPraos.BlockHash (TPraos.HashHeader h)  -> Crypto.hashToBytes h

-- | Block issuer as raw 28-byte key hash.
-- The original returns 'KeyHash BlockIssuer'; we pre-serialize to 'ByteString'.
blockIssuerRaw :: ShelleyProtocol p => ShelleyBlock p era -> ByteString
blockIssuerRaw = Crypto.hashToBytes . unKeyHash . hashKey . pHeaderIssuer . blockHeader

-- | Block number (from header).
blockNumber :: ShelleyProtocol p => ShelleyBlock p era -> BlockNo
blockNumber = pHeaderBlock . blockHeader

-- TODO: re-enable when a caller needs the header slot directly (currently we
-- read the slot from 'SlotDetails' provided by the state-query interpreter).
--
-- -- | Slot number (from header).
-- slotNumber :: ShelleyProtocol p => ShelleyBlock p era -> SlotNo
-- slotNumber = pHeaderSlot . blockHeader

-- | Block size in bytes.
blockSize :: ProtocolHeaderSupportsEnvelope p => ShelleyBlock p era -> Word64
blockSize = fromIntegral . pHeaderBlockSize . blockHeader

-- | Extract indexed transactions from the block body.
-- Returns @[(blockIndex, tx)]@ where @blockIndex@ is 0-based.
getTxs :: forall p era. Ledger.EraBlockBody era => ShelleyBlock p era -> [(Word64, Ledger.Tx Ledger.TopTx era)]
getTxs blk = zip [0 ..] $ toList (Ledger.blockBody (Consensus.shelleyBlockRaw blk) ^. Ledger.txSeqBlockBodyL)

-- ---------------------------------------------------------------------------
-- * TPraos-specific helpers (Shelley, Allegra, Mary, Alonzo)
-- ---------------------------------------------------------------------------

-- | VRF verification key as text (hex-encoded).
-- TODO: Switch to proper Bech32 encoding with @vrf_vk@ human-readable prefix.
blockVrfKeyViewTPraos :: ShelleyBlock (TPraos StandardCrypto) era -> Text
blockVrfKeyViewTPraos = vrfKeyToText . TPraos.bheaderVrfVk . TPraos.bhbody . blockHeader

-- | Operational certificate hot key as raw bytes.
blockOpCertRawTPraos :: ShelleyBlock (TPraos StandardCrypto) era -> ByteString
blockOpCertRawTPraos = KES.rawSerialiseVerKeyKES . TPraos.ocertVkHot . blockOpCertTPraos

-- | Operational certificate counter.
blockOpCertCounterTPraos :: ShelleyBlock (TPraos StandardCrypto) era -> Word64
blockOpCertCounterTPraos = TPraos.ocertN . blockOpCertTPraos

-- | Full OCert from TPraos header.
blockOpCertTPraos :: ShelleyBlock (TPraos StandardCrypto) era -> TPraos.OCert StandardCrypto
blockOpCertTPraos = TPraos.bheaderOCert . TPraos.bhbody . blockHeader

-- | Protocol version from TPraos header.
blockProtoVersionTPraos :: ShelleyBlock (TPraos StandardCrypto) era -> Ledger.ProtVer
blockProtoVersionTPraos = TPraos.bprotver . TPraos.bhbody . blockHeader

-- ---------------------------------------------------------------------------
-- * Praos-specific helpers (Babbage, Conway, Dijkstra)
-- ---------------------------------------------------------------------------

-- | VRF verification key as text (hex-encoded).
-- TODO: Switch to proper Bech32 encoding with @vrf_vk@ human-readable prefix.
blockVrfKeyViewPraos :: ShelleyBlock (Praos StandardCrypto) era -> Text
blockVrfKeyViewPraos = vrfKeyToText . Praos.hbVrfVk . getHeaderBodyPraos . blockHeader

-- | Operational certificate hot key as raw bytes.
blockOpCertRawPraos :: ShelleyBlock (Praos StandardCrypto) era -> ByteString
blockOpCertRawPraos = KES.rawSerialiseVerKeyKES . TPraos.ocertVkHot . blockOpCertPraos

-- | Operational certificate counter.
blockOpCertCounterPraos :: ShelleyBlock (Praos StandardCrypto) era -> Word64
blockOpCertCounterPraos = TPraos.ocertN . blockOpCertPraos

-- | Full OCert from Praos header.
blockOpCertPraos :: ShelleyBlock (Praos StandardCrypto) era -> TPraos.OCert StandardCrypto
blockOpCertPraos = Praos.hbOCert . getHeaderBodyPraos . blockHeader

-- | Protocol version from Praos header.
blockProtoVersionPraos :: ShelleyBlock (Praos StandardCrypto) era -> Ledger.ProtVer
blockProtoVersionPraos = Praos.hbProtVer . getHeaderBodyPraos . blockHeader

-- | Extract the Praos header body.
getHeaderBodyPraos :: Praos.Header StandardCrypto -> Praos.HeaderBody StandardCrypto
getHeaderBodyPraos (Praos.Header hdrBody _) = hdrBody

-- ---------------------------------------------------------------------------
-- * Internal utilities
-- ---------------------------------------------------------------------------

-- | Split a 'ProtVer' into @(major, minor)@ as 'Word16' values.
splitProtoVer :: Ledger.ProtVer -> (Word16, Word16)
splitProtoVer pv =
  ( fromIntegral (Ledger.getVersion (Ledger.pvMajor pv) :: Word64)
  , fromIntegral (Ledger.pvMinor pv :: Natural)
  )

-- | Serialise a VRF verification key as Bech32 with HRP @vrf_vk@.
vrfKeyToText :: VerKeyVRF (VRF StandardCrypto) -> Text
vrfKeyToText = serialiseVrfVkToBech32 . rawSerialiseVerKeyVRF
