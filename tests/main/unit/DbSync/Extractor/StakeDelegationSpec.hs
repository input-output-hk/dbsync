{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the stake-delegation extractor's @stake_registration.deposit@
-- dispatch.
--
-- Three-way precedence:
--
--   * Conway+ certs that carry an inline deposit win regardless of
--     ledger state.
--   * Shelley-Babbage certs (no inline deposit) take the worker's
--     'bldStakeKeyDeposit' protocol-param value when ledger is on.
--   * Ledger-off runs leave the column NULL — matches original behaviour.
module DbSync.Extractor.StakeDelegationSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.IORef (newIORef, readIORef)

import qualified Data.ByteString as BS

import Test.Hspec (Spec, describe, it, shouldBe)

import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin (Coin (..))

import DbSync.Block.Types
  ( BlockEra (..)
  , CertAction (..)
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  )
import DbSync.Db.Types (DbLovelace (..))
import qualified DbSync.Db.Schema.StakeDelegation as SSD
import DbSync.Extractor
  ( BlockLedgerData (..)
  , emptyBlockLedgerData
  , freshExtractState
  )
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.StakeDelegation (stakeDelegationExtractor)

import DbSync.Block.Pipeline (processBlock)
import DbSync.Phase.Type (SyncPhase (..))
import DbSync.Worker.TxOut.AddressBuffer (newAddressBufferRef)
import DbSync.Phase.Ingest.Resolver (mkIngestResolver)
import DbSync.Test.Lsm (withTestIngestStores)
import DbSync.Test.PipelineEnv (mkTestPipelineEnvWith)
import DbSync.Test.Writer (TestWriterState (..), emptyTestWriterState, mkTestWriter)

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

spec :: Spec
spec = describe "stake_registration.deposit" $ do

  it "Conway+ inline deposit wins over the worker value" $ do
    -- Cert carries 1_500_000; worker says 2_000_000. Inline wins.
    let bld = (emptyBlockLedgerData :: BlockLedgerData)
          { bldLedgerEnabled    = True
          , bldStakeKeyDeposit  = Just (Coin 2_000_000)
          }
    written <- runWith bld (blockWithStakeReg (Just 1_500_000))
    case twStakeRegistrations written of
      [(_, sr)] ->
        SSD.stakeRegistrationDeposit sr `shouldBe` Just (DbLovelace 1_500_000)
      _ -> panic "expected exactly one stake_registration"

  it "Shelley-Babbage cert (no inline deposit) takes the worker value" $ do
    let bld = (emptyBlockLedgerData :: BlockLedgerData)
          { bldLedgerEnabled    = True
          , bldStakeKeyDeposit  = Just (Coin 2_000_000)
          }
    written <- runWith bld (blockWithStakeReg Nothing)
    case twStakeRegistrations written of
      [(_, sr)] ->
        SSD.stakeRegistrationDeposit sr `shouldBe` Just (DbLovelace 2_000_000)
      _ -> panic "expected exactly one stake_registration"

  it "leaves the column NULL when ledger is OFF and cert has no inline" $ do
    written <- runWith emptyBlockLedgerData (blockWithStakeReg Nothing)
    case twStakeRegistrations written of
      [(_, sr)] -> SSD.stakeRegistrationDeposit sr `shouldBe` Nothing
      _ -> panic "expected exactly one stake_registration"

-- ---------------------------------------------------------------------------
-- Plumbing
-- ---------------------------------------------------------------------------

runWith :: BlockLedgerData -> GenericBlock -> IO TestWriterState
runWith bld block = withTestIngestStores $ \utxoStore dedupStores -> do
  stRef <- newIORef freshExtractState
  addrBuf <- newAddressBufferRef
  wrRef <- newIORef emptyTestWriterState
  let env = mkTestPipelineEnvWith Mainnet
              (mkIngestResolver stRef dedupStores addrBuf utxoStore Nothing) (mkTestWriter wrRef)
              [coreExtractor, stakeDelegationExtractor]
              (\_ -> pure bld) IngestChainHistory
  runReaderT (processBlock block) env
  readIORef wrRef

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

stakeCred :: ByteString
stakeCred = BS.replicate 28 0xab

txWithStakeReg :: Maybe Word64 -> GenericTx
txWithStakeReg mDeposit = GenericTx
  { txHash             = BS.replicate 32 0x01
  , txBlockIndex       = 0
  , txSize             = 200
  , txFee              = 170_000
  , txOutSum           = 0
  , txValidContract    = True
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
          , txCertAction = CertStakeRegistration stakeCred mDeposit
          }
      ]
  , txWithdrawals      = []
  , txMetadata         = Nothing
  , txMint             = []
  , txCborRaw          = Nothing
  }

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)

shelleyBlock :: GenericBlock
shelleyBlock = GenericBlock
  { blkEra           = Shelley
  , blkHash          = BS.replicate 32 0x42
  , blkPreviousHash  = ""
  , blkSlotNo        = SlotNo 100
  , blkBlockNo       = BlockNo 1
  , blkEpochNo       = EpochNo 5
  , blkEpochSlotNo   = 100
  , blkSize          = 512
  , blkTime          = sampleTime
  , blkSlotLeader    = BS.replicate 28 0xcc
  , blkProtoMajor    = 9
  , blkProtoMinor    = 0
  , blkVrfKey        = Just "vrf_vk1test"
  , blkOpCert        = Just (BS.replicate 32 0)
  , blkOpCertCounter = Just 0
  , blkIsEBB         = False
  , blkTxs           = []
  }

blockWithStakeReg :: Maybe Word64 -> GenericBlock
blockWithStakeReg mDeposit =
  shelleyBlock { blkTxs = [txWithStakeReg mDeposit] }
