{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the off_chain_pools extractor's per-block pass:
-- 'enqueuePoolMetaFetch' is invoked once for every pool registration
-- that carries metadata, skipped otherwise.
module DbSync.Extractor.OffChainPoolsSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.IORef (atomicModifyIORef', newIORef, readIORef)

import qualified Data.ByteString as BS

import Test.Hspec (Spec, describe, it, shouldBe)

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import DbSync.Parser.Types
  ( BlockEra (..)
  , CertAction (..)
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  , PoolRegistrationData (..)
  )

import DbSync.Extractor (freshExtractState)
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.OffChainPools (offChainPoolsExtractor)
import DbSync.Extractor.Pipeline (processBlock)
import DbSync.Phase.Ingest.Resolver (mkIngestResolver)
import DbSync.Resolver (IdResolver (..))
import DbSync.Test.Lsm (withTestIngestStores)
import DbSync.Test.PipelineEnv (mkTestPipelineEnv)
import DbSync.Test.Writer (emptyTestWriterState, mkTestWriter)
import DbSync.Worker.OffChain.Types (PoolMetadataRef (..))
import DbSync.Worker.TxOut.AddressBuffer (newAddressBufferRef)

spec :: Spec
spec =
  describe "off_chain_pools extractor" $ do
    it "enqueues a fetch when a pool registration carries metadata" $ do
      refs <- runExtractor (blockWithPoolReg poolHashA (Just (sampleUrl, sampleHash)))
      length refs `shouldBe` 1
      let ref = headDef (panic "no enqueue") refs
      pmrUrl ref      `shouldBe` sampleUrl
      pmrMetaHash ref `shouldBe` sampleHash
      pmrPoolId ref   `shouldBe` poolHashA

    it "does not enqueue when metadata is absent" $ do
      refs <- runExtractor (blockWithPoolReg poolHashA Nothing)
      length refs `shouldBe` 0

    it "skips phase-2 failed txs" $ do
      refs <- runExtractor
        (blockWithFailedPoolReg poolHashA (Just (sampleUrl, sampleHash)))
      length refs `shouldBe` 0

    it "enqueues once per registration across multiple txs" $ do
      refs <- runExtractor (blockWithTwoRegs poolHashA poolHashB)
      length refs `shouldBe` 2
      map pmrPoolId refs `shouldBe` [poolHashA, poolHashB]

-- ---------------------------------------------------------------------------
-- Harness
-- ---------------------------------------------------------------------------

-- | Run the off_chain_pools extractor against a block, capturing every
-- 'enqueuePoolMetaFetch' call in the order they happen.
runExtractor :: GenericBlock -> IO [PoolMetadataRef]
runExtractor block = withTestIngestStores $ \utxoStore dedupStores -> do
  stRef     <- newIORef freshExtractState
  addrBuf   <- newAddressBufferRef
  wrRef     <- newIORef emptyTestWriterState
  capturedR <- newIORef []
  let baseResolver = mkIngestResolver stRef dedupStores addrBuf utxoStore Nothing
      resolver = baseResolver
        { enqueuePoolMetaFetch = \r ->
            atomicModifyIORef' capturedR (\rs -> (rs ++ [r], ()))
        }
      env = mkTestPipelineEnv resolver (mkTestWriter wrRef)
                              [coreExtractor, offChainPoolsExtractor]
  runReaderT (processBlock block) env
  readIORef capturedR

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

poolHashA :: ByteString
poolHashA = BS.replicate 28 0xa1

poolHashB :: ByteString
poolHashB = BS.replicate 28 0xb2

rewardAddr :: ByteString
rewardAddr = BS.cons 0xE1 (BS.replicate 28 0xcd)

sampleUrl :: Text
sampleUrl = "https://example.test/metadata.json"

sampleHash :: ByteString
sampleHash = BS.replicate 32 0x7e

samplePrd :: ByteString -> Maybe (Text, ByteString) -> PoolRegistrationData
samplePrd poolHash meta = PoolRegistrationData
  { prdPoolHash    = poolHash
  , prdVrfKeyHash  = BS.replicate 32 0xee
  , prdPledge      = 500_000_000_000
  , prdCost        = 340_000_000
  , prdMargin      = 0.05
  , prdRewardAddr  = rewardAddr
  , prdOwners      = []
  , prdRelays      = []
  , prdMetadata    = meta
  }

txWithPoolReg
  :: Bool                              -- ^ validContract
  -> ByteString                        -- ^ pool hash
  -> Maybe (Text, ByteString)          -- ^ metadata
  -> GenericTx
txWithPoolReg validContract poolHash meta = GenericTx
  { txHash             = BS.replicate 32 0x00
  , txBlockIndex       = 0
  , txSize             = 500
  , txFee              = 200_000
  , txOutSum           = 0
  , txValidContract    = validContract
  , txScriptSize       = 0
  , txTreasuryDonation = 0
  , txInvalidBefore    = Nothing
  , txInvalidHereafter = Nothing
  , txInputs           = []
  , txOutputs          = []
  , txCollateralInputs = []
  , txReferenceInputs  = []
  , txCollateralOutput = Nothing
  , txCertificates =
      [ GenericTxCertificate
          { txCertIndex  = 0
          , txCertAction = CertPoolRegistration (samplePrd poolHash meta)
          }
      ]
  , txWithdrawals      = []
  , txMetadata         = Nothing
  , txMint             = []
  , txCborRaw          = Nothing
  , txScripts          = []
  , txDatums           = []
  , txRedeemers        = []
  , txExtraKeyWitnesses = []
  , txParamProposal    = []
  , txProposals        = []
  , txVotingProcedures = []
  , txVotingAnchors    = []
  }

shelleyEmptyBlock :: Word64 -> GenericBlock
shelleyEmptyBlock epoch = GenericBlock
  { blkEra          = Shelley
  , blkHash         = BS.replicate 32 (fromIntegral (epoch `mod` 256))
  , blkPreviousHash = ""
  , blkSlotNo       = SlotNo (epoch * 432_000)
  , blkBlockNo      = BlockNo epoch
  , blkEpochNo      = EpochNo epoch
  , blkEpochSlotNo  = 0
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

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)

blockWithPoolReg :: ByteString -> Maybe (Text, ByteString) -> GenericBlock
blockWithPoolReg poolHash meta =
  (shelleyEmptyBlock 5) { blkTxs = [txWithPoolReg True poolHash meta] }

blockWithFailedPoolReg :: ByteString -> Maybe (Text, ByteString) -> GenericBlock
blockWithFailedPoolReg poolHash meta =
  (shelleyEmptyBlock 5) { blkTxs = [txWithPoolReg False poolHash meta] }

blockWithTwoRegs :: ByteString -> ByteString -> GenericBlock
blockWithTwoRegs hashA hashB = (shelleyEmptyBlock 5)
  { blkTxs =
      [ txWithPoolReg True hashA (Just (sampleUrl, sampleHash))
      , txWithPoolReg True hashB (Just (sampleUrl, sampleHash))
      ]
  }
