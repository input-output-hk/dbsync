{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the UTxO extractor — stake-credential extraction and
-- the FK plumbing that points @tx_out.stake_address_id@ /
-- @address.stake_address_id@ at the same row.
module DbSync.Extractor.UTxOSpec (spec) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.IORef (newIORef, readIORef)

import qualified Data.ByteString as BS

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.Block.Types
  ( BlockEra (..)
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxOut (..)
  )
import DbSync.Db.Schema.Address (Address (..))
import qualified DbSync.Db.Schema.UTxO as SU
import DbSync.Env (HasNetwork (..))
import DbSync.Extractor (ExtractState (..), ExtractorDef (..), HasExtractors (..))
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.StakeDelegation (stakeDelegationExtractor)
import DbSync.Extractor.UTxO
  ( extractStakeCred
  , utxoExtractor
  )
import DbSync.Id.Counter (IdCounters (..), mkIdCounter)
import DbSync.Id.DedupMap (newMaps)
import DbSync.Ingest.Pipeline (processBlock)
import DbSync.Resolver (HasResolver (..), IdResolver)
import DbSync.Resolver.Ingest (mkIngestResolver)
import DbSync.Writer (HasWriter (..), Writer)
import DbSync.Writer.Testing (TestWriterState (..), emptyTestWriterState, mkTestWriter)

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

spec :: Spec
spec = do
  describe "extractStakeCred / Shelley address parser" $ do
    it "extracts the stake credential from a base key-key address (0x00)" $
      extractStakeCred (mkBaseAddr 0x00) `shouldBe` Just stakeCred28

    it "extracts the stake credential from a base script-key address (0x10)" $
      extractStakeCred (mkBaseAddr 0x10) `shouldBe` Just stakeCred28

    it "extracts the stake credential from a base key-script address (0x20)" $
      extractStakeCred (mkBaseAddr 0x20) `shouldBe` Just stakeCred28

    it "extracts the stake credential from a base script-script address (0x30)" $
      extractStakeCred (mkBaseAddr 0x30) `shouldBe` Just stakeCred28

    it "ignores network-id bits (low nibble) when matching the type" $ do
      -- 0x01 = base key-key on mainnet, 0x10 already covered
      extractStakeCred (mkBaseAddr 0x01) `shouldBe` Just stakeCred28
      extractStakeCred (mkBaseAddr 0x21) `shouldBe` Just stakeCred28

    it "returns Nothing for pointer addresses (0x40, 0x50)" $ do
      extractStakeCred (mkBaseAddr 0x40) `shouldBe` Nothing
      extractStakeCred (mkBaseAddr 0x50) `shouldBe` Nothing

    it "returns Nothing for enterprise addresses (0x60, 0x70)" $ do
      extractStakeCred (mkBaseAddr 0x60) `shouldBe` Nothing
      extractStakeCred (mkBaseAddr 0x70) `shouldBe` Nothing

    it "returns Nothing for Byron bootstrap (0x80)" $
      extractStakeCred (mkBaseAddr 0x80) `shouldBe` Nothing

    it "returns Nothing for reward addresses (0xE0, 0xF0)" $ do
      extractStakeCred (mkBaseAddr 0xE0) `shouldBe` Nothing
      extractStakeCred (mkBaseAddr 0xF0) `shouldBe` Nothing

    it "returns Nothing when the address is shorter than 57 bytes" $ do
      extractStakeCred (BS.pack [0x00]) `shouldBe` Nothing
      extractStakeCred (BS.pack (0x00 : replicate 55 0xaa)) `shouldBe` Nothing

    it "returns Nothing on an empty input" $
      extractStakeCred BS.empty `shouldBe` Nothing

  describe "processBlock: tx_out.stake_address_id propagation" $ do
    it "populates tx_out.stake_address_id for a base address" $ do
      written <- runFullPipeline (blockWithOutput (mkBaseAddr 0x00))
      let outs = twTxOuts written
      length outs `shouldBe` 1
      SU.txOutStakeAddressId (snd (headDef (panic "no tx_out") outs))
        `shouldSatisfy` isJust

    it "leaves tx_out.stake_address_id NULL for an enterprise address" $ do
      written <- runFullPipeline (blockWithOutput (mkBaseAddr 0x60))
      let outs = twTxOuts written
      length outs `shouldBe` 1
      SU.txOutStakeAddressId (snd (headDef (panic "no tx_out") outs))
        `shouldBe` Nothing

    it "leaves tx_out.stake_address_id NULL for a pointer address" $ do
      written <- runFullPipeline (blockWithOutput (mkBaseAddr 0x40))
      let outs = twTxOuts written
      SU.txOutStakeAddressId (snd (headDef (panic "no tx_out") outs))
        `shouldBe` Nothing

    it "writes one stake_address row when the output has a base address" $ do
      written <- runFullPipeline (blockWithOutput (mkBaseAddr 0x00))
      length (twStakeAddresses written) `shouldBe` 1

    it "writes no stake_address row when the output has no inline stake cred" $ do
      written <- runFullPipeline (blockWithOutput (mkBaseAddr 0x60))
      length (twStakeAddresses written) `shouldBe` 0

  describe "processBlock: address.stake_address_id propagation" $ do
    it "populates address.stake_address_id for a base address" $ do
      written <- runFullPipeline (blockWithOutput (mkBaseAddr 0x00))
      let addrs = twAddresses written
      length addrs `shouldBe` 1
      addressStakeAddressId (snd (headDef (panic "no address") addrs))
        `shouldSatisfy` isJust

    it "the address.stake_address_id matches tx_out.stake_address_id" $ do
      written <- runFullPipeline (blockWithOutput (mkBaseAddr 0x00))
      let addrSaId = addressStakeAddressId (snd (headDef (panic "no address") (twAddresses written)))
          outSaId  = SU.txOutStakeAddressId (snd (headDef (panic "no tx_out") (twTxOuts written)))
      (addrSaId, outSaId) `shouldSatisfy` \(a, o) -> isJust a && a == o

    it "leaves address.stake_address_id NULL for an enterprise address" $ do
      written <- runFullPipeline (blockWithOutput (mkBaseAddr 0x60))
      let addrs = twAddresses written
      addressStakeAddressId (snd (headDef (panic "no address") addrs))
        `shouldBe` Nothing

  describe "processBlock: stake_address dedup across outputs" $ do
    it "two outputs with the same stake cred share one stake_address row" $ do
      written <- runFullPipeline (blockWithTwoOutputsSameStake (mkBaseAddr 0x00))
      length (twStakeAddresses written) `shouldBe` 1
      length (twTxOuts written) `shouldBe` 2
      let saIds = mapMaybe (SU.txOutStakeAddressId . snd) (twTxOuts written)
      length saIds `shouldBe` 2
      -- Both outputs reference the same StakeAddressId.
      headDef (panic "no sa") saIds `shouldBe` headDef (panic "no sa") (drop 1 saIds)

-- ---------------------------------------------------------------------------
-- Test helpers
-- ---------------------------------------------------------------------------

data TestPipelineEnv = TestPipelineEnv
  { tpeResolver   :: !(IdResolver IO)
  , tpeWriter     :: !(Writer IO)
  , tpeExtractors :: ![ExtractorDef]
  }

instance HasResolver TestPipelineEnv where
  getResolver = tpeResolver

instance HasWriter TestPipelineEnv where
  getWriter = tpeWriter

instance HasExtractors TestPipelineEnv where
  getExtractors = tpeExtractors

instance HasNetwork TestPipelineEnv where
  getNetwork _ = Mainnet

-- | Run @core@ + @stake_delegation@ + @utxo@ on a single block.
runFullPipeline :: GenericBlock -> IO TestWriterState
runFullPipeline block = do
  stRef <- newIORef mkInitState
  dedupMaps <- newMaps
  wrRef <- newIORef emptyTestWriterState
  let env = TestPipelineEnv
        { tpeResolver   = mkIngestResolver stRef dedupMaps
        , tpeWriter     = mkTestWriter wrRef
        , tpeExtractors = [coreExtractor, stakeDelegationExtractor, utxoExtractor]
        }
  runReaderT (processBlock block) env
  readIORef wrRef

mkInitState :: ExtractState
mkInitState = ExtractState
  { esIdCounters = IdCounters
      { icBlockId            = mkIdCounter 1
      , icTxId               = mkIdCounter 1
      , icTxOutId            = mkIdCounter 1
      , icTxInId             = mkIdCounter 1
      , icCollateralTxInId   = mkIdCounter 1
      , icReferenceTxInId    = mkIdCounter 1
      , icTxMetadataId       = mkIdCounter 1
      , icMaTxMintId         = mkIdCounter 1
      , icMaTxOutId          = mkIdCounter 1
      , icSlotLeaderId       = mkIdCounter 1
      , icAddressId          = mkIdCounter 1
      , icStakeAddressId     = mkIdCounter 1
      , icPoolHashId         = mkIdCounter 1
      , icMultiAssetId       = mkIdCounter 1
      , icScriptId              = mkIdCounter 1
      , icStakeRegistrationId   = mkIdCounter 1
      , icStakeDeregistrationId = mkIdCounter 1
      , icDelegationId          = mkIdCounter 1
      , icWithdrawalId          = mkIdCounter 1
      , icPoolUpdateId          = mkIdCounter 1
      , icPoolMetadataRefId     = mkIdCounter 1
      , icPoolOwnerId           = mkIdCounter 1
      , icPoolRetireId          = mkIdCounter 1
      , icPoolRelayId           = mkIdCounter 1
      , icTxCborId              = mkIdCounter 1
      , icEpochSyncStatsId      = mkIdCounter 1
      , icAdaPotsId             = mkIdCounter 1
      }
  , esLastBlockId = Nothing
  }

-- ---------------------------------------------------------------------------
-- Address fixtures
-- ---------------------------------------------------------------------------

-- | 28 bytes of 0xaa — used as the payment credential in fixture
-- addresses. Must be distinct from 'stakeCred28' so that mismatches
-- show up clearly in failing assertions.
paymentCred28 :: ByteString
paymentCred28 = BS.replicate 28 0xaa

-- | 28 bytes of 0xbb — used as the stake credential in fixture addresses.
stakeCred28 :: ByteString
stakeCred28 = BS.replicate 28 0xbb

-- | Build a 57-byte synthetic address with the given header byte.
-- Layout: header || 28-byte payment cred || 28-byte stake cred.
mkBaseAddr :: Word8 -> ByteString
mkBaseAddr header = BS.pack [header] <> paymentCred28 <> stakeCred28

-- ---------------------------------------------------------------------------
-- Block fixtures
-- ---------------------------------------------------------------------------

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)

-- | A Shelley block carrying a single tx with a single output whose
-- raw address bytes are the supplied value.
blockWithOutput :: ByteString -> GenericBlock
blockWithOutput rawAddr =
  shelleyEmptyBlock
    { blkTxs = [singleOutputTx rawAddr 5_000_000]
    }

-- | A block carrying two outputs that share the same stake credential
-- (same raw address, different output indexes).
blockWithTwoOutputsSameStake :: ByteString -> GenericBlock
blockWithTwoOutputsSameStake rawAddr =
  shelleyEmptyBlock
    { blkTxs =
        [ GenericTx
            { txHash             = BS.replicate 32 0xab
            , txBlockIndex       = 0
            , txSize             = 300
            , txFee              = 174_000
            , txOutSum           = 7_500_000
            , txValidContract    = True
            , txScriptSize       = 0
            , txTreasuryDonation = 0
            , txInvalidBefore    = Nothing
            , txInvalidHereafter = Nothing
            , txInputs           = []
            , txOutputs          =
                [ mkOutput 0 rawAddr 2_500_000
                , mkOutput 1 rawAddr 5_000_000
                ]
            , txCollateralInputs = []
            , txReferenceInputs  = []
            , txCollateralOutput = Nothing
            , txCertificates     = []
            , txWithdrawals      = []
            , txMetadata         = Nothing
            , txMint             = []
            , txCborRaw          = Nothing
            }
        ]
    }

shelleyEmptyBlock :: GenericBlock
shelleyEmptyBlock = GenericBlock
  { blkEra          = Shelley
  , blkHash         = BS.replicate 32 0
  , blkPreviousHash = ""
  , blkSlotNo       = SlotNo 100
  , blkBlockNo      = BlockNo 1
  , blkEpochNo      = EpochNo 5
  , blkEpochSlotNo  = 100
  , blkSize         = 512
  , blkTime         = sampleTime
  , blkSlotLeader   = BS.replicate 28 0xab
  , blkProtoMajor   = 9
  , blkProtoMinor   = 0
  , blkVrfKey       = Just "vrf_vk1test"
  , blkOpCert       = Just (BS.replicate 32 0)
  , blkOpCertCounter = Just 0
  , blkIsEBB        = False
  , blkTxs          = []
  }

singleOutputTx :: ByteString -> Word64 -> GenericTx
singleOutputTx rawAddr value = GenericTx
  { txHash             = BS.replicate 32 0xab
  , txBlockIndex       = 0
  , txSize             = 300
  , txFee              = 174_000
  , txOutSum           = value
  , txValidContract    = True
  , txScriptSize       = 0
  , txTreasuryDonation = 0
  , txInvalidBefore    = Nothing
  , txInvalidHereafter = Nothing
  , txInputs           = []
  , txOutputs          = [mkOutput 0 rawAddr value]
  , txCollateralInputs = []
  , txReferenceInputs  = []
  , txCollateralOutput = Nothing
  , txCertificates     = []
  , txWithdrawals      = []
  , txMetadata         = Nothing
  , txMint             = []
  , txCborRaw          = Nothing
  }

mkOutput :: Word16 -> ByteString -> Word64 -> GenericTxOut
mkOutput idx rawAddr value = GenericTxOut
  { txOutIndex       = idx
  , txOutAddress     = "addr_test1xyz"
  , txOutAddressRaw  = rawAddr
  , txOutValue       = value
  , txOutDataHash    = Nothing
  , txOutInlineDatum = Nothing
  , txOutRefScript   = Nothing
  , txOutMultiAssets = []
  }
