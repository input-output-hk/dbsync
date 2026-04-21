{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Shelley+ block-level converters and shared helpers.
--
-- Ported from @Cardano.DbSync.Era.Shelley.Generic.Block@ in the original
-- cardano-db-sync. Each @from*Block@ function converts an era-specific
-- 'ShelleyBlock' into our era-independent 'GenericBlock'.
--
-- The block-level converters are intentionally thin — they delegate to
-- shared helpers for header extraction and differ only in:
--
--   1. The 'BlockEra' tag
--   2. TPraos vs Praos helpers for VRF\/OpCert\/ProtVer
--   3. Which @from*Tx@ function is mapped over transactions (currently stubbed)
--
-- __First pass:__ @blkTxs = []@ — transaction extraction wired in Steps 6+7.
module DbSync.Block.Parser.Shelley
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
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as Text
import Lens.Micro ((^.))

import Cardano.Ledger.Keys (hashKey, unKeyHash)
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Protocol.Crypto (StandardCrypto, VRF)
import qualified Cardano.Protocol.TPraos.BHeader as TPraos
import qualified Cardano.Protocol.TPraos.OCert as TPraos
import Cardano.Slotting.Slot (SlotNo)

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
  , pHeaderSlot
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

import DbSync.Block.Parser.Types (EpochSlotInfo (..))
import DbSync.Block.Types
  ( BlockEra (..)
  , GenericBlock (..)
  )

-- ---------------------------------------------------------------------------
-- * Block converters: TPraos eras (Shelley, Allegra, Mary, Alonzo)
-- ---------------------------------------------------------------------------

fromShelleyBlock :: EpochSlotInfo -> ShelleyBlock (TPraos StandardCrypto) ShelleyEra -> GenericBlock
fromShelleyBlock = mkShelleyBlockTPraos Shelley

fromAllegraBlock :: EpochSlotInfo -> ShelleyBlock (TPraos StandardCrypto) AllegraEra -> GenericBlock
fromAllegraBlock = mkShelleyBlockTPraos Allegra

fromMaryBlock :: EpochSlotInfo -> ShelleyBlock (TPraos StandardCrypto) MaryEra -> GenericBlock
fromMaryBlock = mkShelleyBlockTPraos Mary

fromAlonzoBlock :: EpochSlotInfo -> ShelleyBlock (TPraos StandardCrypto) AlonzoEra -> GenericBlock
fromAlonzoBlock = mkShelleyBlockTPraos Alonzo

-- | Shared TPraos block converter — all pre-Babbage eras use the same pattern.
mkShelleyBlockTPraos
  :: BlockEra
  -> EpochSlotInfo
  -> ShelleyBlock (TPraos StandardCrypto) era
  -> GenericBlock
mkShelleyBlockTPraos era esi blk =
  let slotNo = slotNumber blk
      (protoMaj, protoMin) = splitProtoVer (blockProtoVersionTPraos blk)
  in GenericBlock
    { blkEra           = era
    , blkHash          = blockHash blk
    , blkPreviousHash  = blockPrevHash blk
    , blkSlotNo        = slotNo
    , blkBlockNo       = blockNumber blk
    , blkEpochNo       = esiSlotToEpochNo esi slotNo
    , blkEpochSlotNo   = esiSlotToEpochSlot esi slotNo
    , blkSize          = blockSize blk
    , blkTime          = esiSlotToUTCTime esi slotNo
    , blkSlotLeader    = blockIssuerRaw blk
    , blkProtoMajor    = protoMaj
    , blkProtoMinor    = protoMin
    , blkVrfKey        = Just (blockVrfKeyViewTPraos blk)
    , blkOpCert        = Just (blockOpCertRawTPraos blk)
    , blkOpCertCounter = Just (blockOpCertCounterTPraos blk)
    , blkTxs           = []  -- TODO: Wire tx converters (Step 6+7)
    }

-- ---------------------------------------------------------------------------
-- * Block converters: Praos eras (Babbage, Conway, Dijkstra)
-- ---------------------------------------------------------------------------

fromBabbageBlock :: EpochSlotInfo -> ShelleyBlock (Praos StandardCrypto) BabbageEra -> GenericBlock
fromBabbageBlock = mkShelleyBlockPraos Babbage

fromConwayBlock :: EpochSlotInfo -> ShelleyBlock (Praos StandardCrypto) ConwayEra -> GenericBlock
fromConwayBlock = mkShelleyBlockPraos Conway

fromDijkstraBlock :: EpochSlotInfo -> ShelleyBlock (Praos StandardCrypto) DijkstraEra -> GenericBlock
fromDijkstraBlock = mkShelleyBlockPraos Dijkstra

-- | Shared Praos block converter — Babbage+ eras use the same pattern.
mkShelleyBlockPraos
  :: BlockEra
  -> EpochSlotInfo
  -> ShelleyBlock (Praos StandardCrypto) era
  -> GenericBlock
mkShelleyBlockPraos era esi blk =
  let slotNo = slotNumber blk
      (protoMaj, protoMin) = splitProtoVer (blockProtoVersionPraos blk)
  in GenericBlock
    { blkEra           = era
    , blkHash          = blockHash blk
    , blkPreviousHash  = blockPrevHash blk
    , blkSlotNo        = slotNo
    , blkBlockNo       = blockNumber blk
    , blkEpochNo       = esiSlotToEpochNo esi slotNo
    , blkEpochSlotNo   = esiSlotToEpochSlot esi slotNo
    , blkSize          = blockSize blk
    , blkTime          = esiSlotToUTCTime esi slotNo
    , blkSlotLeader    = blockIssuerRaw blk
    , blkProtoMajor    = protoMaj
    , blkProtoMinor    = protoMin
    , blkVrfKey        = Just (blockVrfKeyViewPraos blk)
    , blkOpCert        = Just (blockOpCertRawPraos blk)
    , blkOpCertCounter = Just (blockOpCertCounterPraos blk)
    , blkTxs           = []  -- TODO: Wire tx converters (Step 6+7)
    }

-- ---------------------------------------------------------------------------
-- * Shared block helpers (protocol-agnostic)
-- ---------------------------------------------------------------------------

-- | Extract the block header from a 'ShelleyBlock'.
blockHeader :: ShelleyBlock p era -> ShelleyProtocolHeader p
blockHeader = Ledger.bheader . Consensus.shelleyBlockRaw

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

-- | Slot number (from header).
slotNumber :: ShelleyProtocol p => ShelleyBlock p era -> SlotNo
slotNumber = pHeaderSlot . blockHeader

-- | Block size in bytes.
blockSize :: ProtocolHeaderSupportsEnvelope p => ShelleyBlock p era -> Word64
blockSize = fromIntegral . pHeaderBlockSize . blockHeader

-- | Extract indexed transactions from the block body.
-- Returns @[(blockIndex, tx)]@ where @blockIndex@ is 0-based.
getTxs :: forall p era. Ledger.EraBlockBody era => ShelleyBlock p era -> [(Word64, Ledger.Tx era)]
getTxs blk = zip [0 ..] $ toList (Ledger.bbody (Consensus.shelleyBlockRaw blk) ^. Ledger.txSeqBlockBodyL)

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
  ( fromIntegral (Ledger.getVersion (Ledger.pvMajor pv))
  , fromIntegral (Ledger.pvMinor pv)
  )

-- | Serialize a VRF verification key to text.
-- Uses hex encoding for now. The original uses Bech32 with @vrf_vk@ prefix.
-- TODO: Add @bech32@ dependency and use proper Bech32 encoding.
vrfKeyToText :: VerKeyVRF (VRF StandardCrypto) -> Text
vrfKeyToText vk = Text.decodeUtf8 (Base16.encode (rawSerialiseVerKeyVRF vk))
