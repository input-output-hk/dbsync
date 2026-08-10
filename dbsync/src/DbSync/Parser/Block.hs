{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Post-Byron block converters and their shared helpers. Every era
-- from Shelley to Dijkstra uses the consensus 'ShelleyBlock' wrapper,
-- so each @from*Block@ stays thin and calls the shared header
-- helpers. They differ in three points only: the 'BlockEra' tag, the
-- TPraos or Praos helpers for VRF, OpCert and ProtVer, and which
-- @from*Tx@ they map over the transactions.
module DbSync.Parser.Block
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

import DbSync.Parser.Tx
  ( fromShelleyTx
  , fromAllegraTx
  , fromMaryTx
  , fromAlonzoTx
  , fromBabbageTx
  , fromConwayTx
  , fromDijkstraTx
  )
import DbSync.StateQuery (SlotDetails (..))
import DbSync.Parser.Types
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

blockHeader :: ShelleyBlock p era -> ShelleyProtocolHeader p
blockHeader = Ledger.blockHeader . Consensus.shelleyBlockRaw

-- | 32 raw bytes.
blockHash :: ShelleyBlock p era -> ByteString
blockHash =
  Crypto.hashToBytes
    . Consensus.unShelleyHash
    . Consensus.shelleyBlockHeaderHash

-- | 32 raw bytes, or an empty 'ByteString' for the first block
-- after genesis.
blockPrevHash :: ProtocolHeaderSupportsEnvelope p => ShelleyBlock p era -> ByteString
blockPrevHash blk =
  case pHeaderPrevHash (blockHeader blk) of
    TPraos.GenesisHash                      -> BS.empty
    TPraos.BlockHash (TPraos.HashHeader h)  -> Crypto.hashToBytes h

-- | The issuer's 28-byte key hash, serialised here rather than kept
-- as a 'KeyHash'.
blockIssuerRaw :: ShelleyProtocol p => ShelleyBlock p era -> ByteString
blockIssuerRaw = Crypto.hashToBytes . unKeyHash . hashKey . pHeaderIssuer . blockHeader

blockNumber :: ShelleyProtocol p => ShelleyBlock p era -> BlockNo
blockNumber = pHeaderBlock . blockHeader

-- | Size in bytes.
blockSize :: ProtocolHeaderSupportsEnvelope p => ShelleyBlock p era -> Word64
blockSize = fromIntegral . pHeaderBlockSize . blockHeader

-- | Returns @[(blockIndex, tx)]@ with a 0-based @blockIndex@.
getTxs :: forall p era. Ledger.EraBlockBody era => ShelleyBlock p era -> [(Word64, Ledger.Tx Ledger.TopTx era)]
getTxs blk = zip [0 ..] $ toList (Ledger.blockBody (Consensus.shelleyBlockRaw blk) ^. Ledger.txSeqBlockBodyL)

-- ---------------------------------------------------------------------------
-- * TPraos-specific helpers (Shelley, Allegra, Mary, Alonzo)
-- ---------------------------------------------------------------------------

blockVrfKeyViewTPraos :: ShelleyBlock (TPraos StandardCrypto) era -> Text
blockVrfKeyViewTPraos = vrfKeyToText . TPraos.bheaderVrfVk . TPraos.bhbody . blockHeader

blockOpCertRawTPraos :: ShelleyBlock (TPraos StandardCrypto) era -> ByteString
blockOpCertRawTPraos = KES.rawSerialiseVerKeyKES . TPraos.ocertVkHot . blockOpCertTPraos

blockOpCertCounterTPraos :: ShelleyBlock (TPraos StandardCrypto) era -> Word64
blockOpCertCounterTPraos = TPraos.ocertN . blockOpCertTPraos

blockOpCertTPraos :: ShelleyBlock (TPraos StandardCrypto) era -> TPraos.OCert StandardCrypto
blockOpCertTPraos = TPraos.bheaderOCert . TPraos.bhbody . blockHeader

blockProtoVersionTPraos :: ShelleyBlock (TPraos StandardCrypto) era -> Ledger.ProtVer
blockProtoVersionTPraos = TPraos.bprotver . TPraos.bhbody . blockHeader

-- ---------------------------------------------------------------------------
-- * Praos-specific helpers (Babbage, Conway, Dijkstra)
-- ---------------------------------------------------------------------------

blockVrfKeyViewPraos :: ShelleyBlock (Praos StandardCrypto) era -> Text
blockVrfKeyViewPraos = vrfKeyToText . Praos.hbVrfVk . getHeaderBodyPraos . blockHeader

blockOpCertRawPraos :: ShelleyBlock (Praos StandardCrypto) era -> ByteString
blockOpCertRawPraos = KES.rawSerialiseVerKeyKES . TPraos.ocertVkHot . blockOpCertPraos

blockOpCertCounterPraos :: ShelleyBlock (Praos StandardCrypto) era -> Word64
blockOpCertCounterPraos = TPraos.ocertN . blockOpCertPraos

blockOpCertPraos :: ShelleyBlock (Praos StandardCrypto) era -> TPraos.OCert StandardCrypto
blockOpCertPraos = Praos.hbOCert . getHeaderBodyPraos . blockHeader

blockProtoVersionPraos :: ShelleyBlock (Praos StandardCrypto) era -> Ledger.ProtVer
blockProtoVersionPraos = Praos.hbProtVer . getHeaderBodyPraos . blockHeader

getHeaderBodyPraos :: Praos.Header StandardCrypto -> Praos.HeaderBody StandardCrypto
getHeaderBodyPraos (Praos.Header hdrBody _) = hdrBody

-- ---------------------------------------------------------------------------
-- * Internal utilities
-- ---------------------------------------------------------------------------

splitProtoVer :: Ledger.ProtVer -> (Word16, Word16)
splitProtoVer pv =
  ( fromIntegral (Ledger.getVersion (Ledger.pvMajor pv) :: Word64)
  , fromIntegral (Ledger.pvMinor pv :: Natural)
  )

-- | Bech32 with the @vrf_vk@ HRP.
vrfKeyToText :: VerKeyVRF (VRF StandardCrypto) -> Text
vrfKeyToText = serialiseVrfVkToBech32 . rawSerialiseVerKeyVRF
