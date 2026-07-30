{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the stake-delegation extractor.
--
-- Covers @stake_registration.deposit@ dispatch (three-way precedence
-- between inline Conway+ deposits, ledger protocol-param values, and
-- ledger-off) and the MIR cert handling that writes
-- @reserve@ / @treasury@ / @pot_transfer@ rows.
module DbSync.Extractor.StakeDelegationSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.IORef (newIORef, readIORef)

import qualified Data.ByteString as BS

import Test.Hspec (Spec, describe, it, shouldBe)

import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin (Coin (..))

import DbSync.Parser.Types
  ( BlockEra (..)
  , CertAction (..)
  , CredHash (..)
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  , MirAction (..)
  , MirPot (..)
  )
import qualified DbSync.Db.Schema.EpochBoundary as SEB
import DbSync.Db.Types (DbLovelace (..), fromDbInt65)
import qualified DbSync.Db.Schema.StakeDelegation as SSD
import DbSync.Extractor
  ( BlockLedgerData (..)
  , LedgerOutputs (..)
  , emptyBlockLedgerData
  , emptyLedgerOutputs
  , freshExtractState
  )
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.StakeDelegation (stakeDelegationExtractor)

import DbSync.Extractor.Pipeline (processBlock)
import DbSync.Phase.Type (SyncPhase (..))
import DbSync.Worker.TxOut.AddressBuffer (newAddressBufferRef)
import DbSync.Phase.Ingest.Resolver (mkIngestResolver)
import DbSync.Test.Lsm (withTestIngestStores)
import DbSync.Test.PipelineEnv (mkTestPipelineEnvWith)
import DbSync.Test.Writer (TestWriterState (..), emptyTestWriterState, mkTestWriter)

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

spec :: Spec
spec = do
  describe "stake_registration.deposit" $ do

    it "Conway+ inline deposit wins over the worker value" $ do
      -- Cert carries 1_500_000; worker says 2_000_000. Inline wins.
      let bld = LedgerDataOn $ emptyLedgerOutputs
            { loStakeKeyDeposit = Just (Coin 2_000_000)
            }
      written <- runWith bld (blockWithStakeReg (Just 1_500_000))
      case twStakeRegistrations written of
        [sr] ->
          SSD.stakeRegistrationDeposit sr `shouldBe` Just (DbLovelace 1_500_000)
        _ -> panic "expected exactly one stake_registration"

    it "Shelley-Babbage cert (no inline deposit) takes the worker value" $ do
      let bld = LedgerDataOn $ emptyLedgerOutputs
            { loStakeKeyDeposit = Just (Coin 2_000_000)
            }
      written <- runWith bld (blockWithStakeReg Nothing)
      case twStakeRegistrations written of
        [sr] ->
          SSD.stakeRegistrationDeposit sr `shouldBe` Just (DbLovelace 2_000_000)
        _ -> panic "expected exactly one stake_registration"

    it "leaves the column NULL when ledger is OFF and cert has no inline" $ do
      written <- runWith emptyBlockLedgerData (blockWithStakeReg Nothing)
      case twStakeRegistrations written of
        [sr] -> SSD.stakeRegistrationDeposit sr `shouldBe` Nothing
        _ -> panic "expected exactly one stake_registration"

  describe "MIR certs" $ do

    it "MirToStakeAddresses on MirReserves writes one reserve row" $ do
      let act = MirToStakeAddresses [(stakeCred, 7_500_000)]
      written <- runWith emptyBlockLedgerData (blockWithMir MirReserves act)
      length (twReserves written) `shouldBe` 1
      length (twTreasuries written) `shouldBe` 0
      case twReserves written of
        [r] -> do
          fromDbInt65 (SEB.reserveAmount r) `shouldBe` 7_500_000
        _ -> panic "expected one reserve row"

    it "MirToStakeAddresses on MirTreasury writes one treasury row" $ do
      let act = MirToStakeAddresses [(stakeCred, 4_000_000)]
      written <- runWith emptyBlockLedgerData (blockWithMir MirTreasury act)
      length (twTreasuries written) `shouldBe` 1
      length (twReserves written) `shouldBe` 0
      case twTreasuries written of
        [t] -> do
          fromDbInt65 (SEB.treasuryAmount t) `shouldBe` 4_000_000
        _ -> panic "expected one treasury row"

    it "multi-recipient MIR writes one row per recipient" $ do
      let act = MirToStakeAddresses
            [ (stakeCred,                          1_000_000)
            , (CredHash (BS.replicate 28 0xcd) False, 2_000_000)
            , (CredHash (BS.replicate 28 0xef) False, 3_000_000)
            ]
      written <- runWith emptyBlockLedgerData (blockWithMir MirReserves act)
      length (twReserves written) `shouldBe` 3
      map (fromDbInt65 . SEB.reserveAmount) (twReserves written)
        `shouldBe` [1_000_000, 2_000_000, 3_000_000]

    it "MirPotToPot on MirReserves: treasury +xfer, reserves -xfer" $ do
      written <- runWith emptyBlockLedgerData
                   (blockWithMir MirReserves (MirPotToPot 100))
      case twPotTransfers written of
        [pt] -> do
          fromDbInt65 (SEB.potTransferTreasury pt) `shouldBe`  100
          fromDbInt65 (SEB.potTransferReserves pt) `shouldBe` -100
        _ -> panic "expected one pot_transfer row"

    it "MirPotToPot on MirTreasury: treasury -xfer, reserves +xfer" $ do
      written <- runWith emptyBlockLedgerData
                   (blockWithMir MirTreasury (MirPotToPot 100))
      case twPotTransfers written of
        [pt] -> do
          fromDbInt65 (SEB.potTransferTreasury pt) `shouldBe` -100
          fromDbInt65 (SEB.potTransferReserves pt) `shouldBe`  100
        _ -> panic "expected one pot_transfer row"

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

stakeCred :: CredHash
stakeCred = CredHash (BS.replicate 28 0xab) False

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
          { txCertIndex      = 0
          , txCertAction     = CertStakeRegistration stakeCred mDeposit
          , txCertRedeemerIx = Nothing
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

txWithMir :: MirPot -> MirAction -> GenericTx
txWithMir pot act = GenericTx
  { txHash             = BS.replicate 32 0x02
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
          { txCertIndex      = 0
          , txCertAction     = CertMir pot act
          , txCertRedeemerIx = Nothing
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

blockWithMir :: MirPot -> MirAction -> GenericBlock
blockWithMir pot act =
  shelleyBlock { blkTxs = [txWithMir pot act] }
