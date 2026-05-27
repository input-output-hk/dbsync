{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the pool extractor: pool-update epoch offsets and the
-- @txValidContract@ gate.
module DbSync.Extractor.PoolSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.IORef (newIORef, readIORef)

import Data.List ((!!))
import qualified Data.ByteString as BS

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Block.Types
  ( BlockEra (..)
  , CertAction (..)
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  , PoolRegistrationData (..)
  )
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin (Coin (..))

import DbSync.Db.Types (DbLovelace (..))
import qualified DbSync.Db.Schema.Pool as SP
import DbSync.Extractor
  ( BlockLedgerData (..)
  , emptyBlockLedgerData
  , freshExtractState
  )
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.Pool (poolExtractor)
import DbSync.Extractor.StakeDelegation (stakeDelegationExtractor)

import DbSync.Block.Pipeline (processBlock)
import DbSync.Phase.Type (SyncPhase (..))
import DbSync.Worker.TxOut.AddressBuffer (newAddressBufferRef)
import DbSync.Phase.Ingest.Resolver (mkIngestResolver)
import DbSync.Test.Lsm (withTestIngestStores)
import DbSync.Test.PipelineEnv (mkTestPipelineEnv, mkTestPipelineEnvWith)
import DbSync.Test.Writer (TestWriterState (..), emptyTestWriterState, mkTestWriter)

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

spec :: Spec
spec = do
  describe "pool_update.active_epoch_no" $ do
    it "is epoch + 2 on first registration" $ do
      written <- runPool (blockWithPoolReg poolHashA 5)
      let pu = snd (headDef (panic "no pool_update") (twPoolUpdates written))
      SP.poolUpdateActiveEpochNo pu `shouldBe` 7

    it "is epoch + 3 on a re-registration of the same pool" $ do
      written <- runPoolTwoBlocks
                   (blockWithPoolReg poolHashA 5)
                   (blockWithPoolReg poolHashA 6)
      let updates = twPoolUpdates written
      length updates `shouldBe` 2
      SP.poolUpdateActiveEpochNo (snd (updates !! 0)) `shouldBe` 7  -- 5 + 2
      SP.poolUpdateActiveEpochNo (snd (updates !! 1)) `shouldBe` 9  -- 6 + 3

    it "treats different pools as independent first registrations" $ do
      written <- runPoolTwoBlocks
                   (blockWithPoolReg poolHashA 5)
                   (blockWithPoolReg poolHashB 6)
      let updates = twPoolUpdates written
      length updates `shouldBe` 2
      -- Both are first sightings of their respective pools.
      SP.poolUpdateActiveEpochNo (snd (updates !! 0)) `shouldBe` 7
      SP.poolUpdateActiveEpochNo (snd (updates !! 1)) `shouldBe` 8

  describe "phase-2 failure (txValidContract = False)" $ do
    it "writes no pool_update rows" $ do
      written <- runPool (blockWithFailedPoolReg poolHashA 5)
      length (twPoolUpdates written) `shouldBe` 0

  describe "pool_update.deposit (worker-supplied protocol param)" $ do
    it "is NULL when the ledger feature is OFF" $ do
      -- Default 'mkTestPipelineEnv' supplies emptyBlockLedgerData, so
      -- 'bldLedgerEnabled' is False and the deposit stays Nothing.
      written <- runPool (blockWithPoolReg poolHashA 5)
      let pu = snd (headDef (panic "no pool_update") (twPoolUpdates written))
      SP.poolUpdateDeposit pu `shouldBe` Nothing

    it "is the protocol-param value on first registration when ledger ON" $ do
      let bld = (emptyBlockLedgerData :: BlockLedgerData)
            { bldLedgerEnabled = True
            , bldPoolDeposit   = Just (Coin 500_000_000)
            }
      written <- runPoolWith bld (blockWithPoolReg poolHashA 5)
      let pu = snd (headDef (panic "no pool_update") (twPoolUpdates written))
      SP.poolUpdateDeposit pu `shouldBe` Just (DbLovelace 500_000_000)

    it "is NULL on a re-registration even when ledger ON" $ do
      let bld = (emptyBlockLedgerData :: BlockLedgerData)
            { bldLedgerEnabled = True
            , bldPoolDeposit   = Just (Coin 500_000_000)
            }
      written <- runPoolWithBlocks bld
                   [blockWithPoolReg poolHashA 5, blockWithPoolReg poolHashA 6]
      let updates = twPoolUpdates written
      length updates `shouldBe` 2
      SP.poolUpdateDeposit (snd (updates !! 0)) `shouldBe` Just (DbLovelace 500_000_000)
      SP.poolUpdateDeposit (snd (updates !! 1)) `shouldBe` Nothing

-- ---------------------------------------------------------------------------
-- Test plumbing
-- ---------------------------------------------------------------------------

runPool :: GenericBlock -> IO TestWriterState
runPool = runPoolBlocks . pure

runPoolTwoBlocks :: GenericBlock -> GenericBlock -> IO TestWriterState
runPoolTwoBlocks b1 b2 = runPoolBlocks [b1, b2]

runPoolBlocks :: [GenericBlock] -> IO TestWriterState
runPoolBlocks blocks = withTestIngestStores $ \utxoStore dedupStores -> do
  stRef <- newIORef freshExtractState
  addrBuf <- newAddressBufferRef
  wrRef <- newIORef emptyTestWriterState
  let env = mkTestPipelineEnv (mkIngestResolver stRef dedupStores addrBuf utxoStore Nothing)
                              (mkTestWriter wrRef)
                              [coreExtractor, stakeDelegationExtractor, poolExtractor]
  for_ blocks $ \b -> runReaderT (processBlock b) env
  readIORef wrRef

-- | Run one block with a custom 'BlockLedgerData' fixture.
runPoolWith :: BlockLedgerData -> GenericBlock -> IO TestWriterState
runPoolWith bld block = runPoolWithBlocks bld [block]

-- | Run several blocks sharing one 'BlockLedgerData' value.
runPoolWithBlocks :: BlockLedgerData -> [GenericBlock] -> IO TestWriterState
runPoolWithBlocks bld blocks = withTestIngestStores $ \utxoStore dedupStores -> do
  stRef <- newIORef freshExtractState
  addrBuf <- newAddressBufferRef
  wrRef <- newIORef emptyTestWriterState
  let env = mkTestPipelineEnvWith Mainnet
              (mkIngestResolver stRef dedupStores addrBuf utxoStore Nothing) (mkTestWriter wrRef)
              [coreExtractor, stakeDelegationExtractor, poolExtractor]
              (\_ -> pure bld) IngestChainHistory
  for_ blocks $ \b -> runReaderT (processBlock b) env
  readIORef wrRef

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

poolHashA :: ByteString
poolHashA = BS.replicate 28 0xa1

poolHashB :: ByteString
poolHashB = BS.replicate 28 0xb2

rewardAddr :: ByteString
rewardAddr = BS.cons 0xE1 (BS.replicate 28 0xcd)

samplePrd :: ByteString -> PoolRegistrationData
samplePrd poolHash = PoolRegistrationData
  { prdPoolHash    = poolHash
  , prdVrfKeyHash  = BS.replicate 32 0xee
  , prdPledge      = 500_000_000_000
  , prdCost        = 340_000_000
  , prdMargin      = 0.05
  , prdRewardAddr  = rewardAddr
  , prdOwners      = []
  , prdRelays      = []
  , prdMetadata    = Nothing
  }

txWithPoolReg :: Bool -> ByteString -> GenericTx
txWithPoolReg validContract poolHash = GenericTx
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
          , txCertAction = CertPoolRegistration (samplePrd poolHash)
          }
      ]
  , txWithdrawals      = []
  , txMetadata         = Nothing
  , txMint             = []
  , txCborRaw          = Nothing
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

blockWithPoolReg :: ByteString -> Word64 -> GenericBlock
blockWithPoolReg poolHash epoch =
  (shelleyEmptyBlock epoch) { blkTxs = [txWithPoolReg True poolHash] }

blockWithFailedPoolReg :: ByteString -> Word64 -> GenericBlock
blockWithFailedPoolReg poolHash epoch =
  (shelleyEmptyBlock epoch) { blkTxs = [txWithPoolReg False poolHash] }
