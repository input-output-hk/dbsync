{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Tests for the MultiAsset extractor — which transactions contribute
-- @ma_tx_out@ and @ma_tx_mint@ rows. The subtle case is a phase-2
-- failure: its mint is never applied (no @ma_tx_mint@), but its
-- collateral-return output survives on-chain (Babbage onward) and its
-- assets still belong in @ma_tx_out@.
module DbSync.Extractor.MultiAssetSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.IORef (newIORef, readIORef)

import qualified Data.ByteString as BS

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Parser.Types
  ( BlockEra (..)
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxIn (..)
  , GenericTxOut (..)
  )
import DbSync.Db.Schema.MultiAsset (MaTxOut (..))
import DbSync.Db.Types (unDbWord64)
import DbSync.Extractor (freshExtractState)
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.MultiAsset (multiAssetExtractor)
import DbSync.Extractor.Pipeline (processBlock)
import DbSync.Phase.Ingest.Resolver (mkIngestResolver)
import DbSync.Test.Lsm (withTestIngestStores)
import DbSync.Test.PipelineEnv (mkTestPipelineEnv)
import DbSync.Test.Writer (TestWriterState (..), emptyTestWriterState, mkTestWriter)
import DbSync.Worker.TxOut.AddressBuffer (newAddressBufferRef)

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

spec :: Spec
spec = do
  describe "multiAssetExtractor: ma_tx_out" $ do
    it "writes a row for a valid tx's multi-asset output" $ do
      w <- runPipeline (blockWith [validTxWithAssetOutput])
      map (unDbWord64 . maTxOutQuantity) (twMaTxOuts w) `shouldBe` [1234]

    it "writes a row for a phase-2 failure's collateral-return output" $ do
      -- The parser folds a failed Babbage+ tx's collateral return into
      -- txOutputs, so it lands in tx_out and its assets belong in
      -- ma_tx_out like any other output.
      w <- runPipeline (blockWith [invalidTxWithAssetCollReturn])
      map (unDbWord64 . maTxOutQuantity) (twMaTxOuts w) `shouldBe` [4321]

    it "writes no row for a failed tx with no surviving output (Alonzo shape)" $ do
      w <- runPipeline (blockWith [invalidTxNoOutputs])
      length (twMaTxOuts w) `shouldBe` 0

  describe "multiAssetExtractor: ma_tx_mint" $ do
    it "writes a row for a valid tx's mint" $ do
      w <- runPipeline (blockWith [validTxWithMint])
      length (twMaTxMints w) `shouldBe` 1

    it "writes no row for a phase-2 failure's mint (never applied)" $ do
      w <- runPipeline (blockWith [invalidTxWithMint])
      length (twMaTxMints w) `shouldBe` 0

-- ---------------------------------------------------------------------------
-- Pipeline runner
-- ---------------------------------------------------------------------------

runPipeline :: GenericBlock -> IO TestWriterState
runPipeline block = withTestIngestStores $ \utxoStore dedupStores -> do
  stRef   <- newIORef freshExtractState
  addrBuf <- newAddressBufferRef
  wrRef   <- newIORef emptyTestWriterState
  let env = mkTestPipelineEnv
              (mkIngestResolver stRef dedupStores addrBuf utxoStore Nothing)
              (mkTestWriter wrRef)
              [coreExtractor, multiAssetExtractor]
  runReaderT (processBlock block) env
  readIORef wrRef

-- ---------------------------------------------------------------------------
-- Asset fixtures
-- ---------------------------------------------------------------------------

policyId :: ByteString
policyId = BS.replicate 28 0x11

assetName :: ByteString
assetName = "TOKEN"

-- ---------------------------------------------------------------------------
-- Block / tx fixtures
-- ---------------------------------------------------------------------------

blockWith :: [GenericTx] -> GenericBlock
blockWith txs = emptyBlock { blkTxs = txs }

-- | A valid tx with a single output carrying one multi-asset.
validTxWithAssetOutput :: GenericTx
validTxWithAssetOutput = baseTx
  { txOutputs = [ (mkOutput 0) { txOutMultiAssets = [(policyId, assetName, 1234)] } ]
  }

-- | A phase-2 failure whose collateral-return output — folded into
-- txOutputs by the parser — carries a multi-asset.
invalidTxWithAssetCollReturn :: GenericTx
invalidTxWithAssetCollReturn = baseTx
  { txValidContract    = False
  , txInputs           = [GenericTxIn (BS.replicate 32 0xcc) 0 Nothing]
  , txCollateralInputs = [GenericTxIn (BS.replicate 32 0xcc) 0 Nothing]
  , txOutputs          = [ (mkOutput 1) { txOutMultiAssets = [(policyId, assetName, 4321)] } ]
  }

-- | A phase-2 failure with no surviving output (Alonzo has no
-- collateral-return output).
invalidTxNoOutputs :: GenericTx
invalidTxNoOutputs = baseTx
  { txValidContract    = False
  , txInputs           = [GenericTxIn (BS.replicate 32 0xcc) 0 Nothing]
  , txCollateralInputs = [GenericTxIn (BS.replicate 32 0xcc) 0 Nothing]
  , txOutputs          = []
  }

-- | A valid tx that mints one multi-asset.
validTxWithMint :: GenericTx
validTxWithMint = baseTx { txMint = [(policyId, assetName, 1000)] }

-- | A phase-2 failure that declares a mint the chain never applies.
invalidTxWithMint :: GenericTx
invalidTxWithMint = baseTx
  { txValidContract    = False
  , txInputs           = [GenericTxIn (BS.replicate 32 0xcc) 0 Nothing]
  , txCollateralInputs = [GenericTxIn (BS.replicate 32 0xcc) 0 Nothing]
  , txMint             = [(policyId, assetName, 1000)]
  }

baseTx :: GenericTx
baseTx = GenericTx
  { txHash             = BS.replicate 32 0xab
  , txBlockIndex       = 0
  , txSize             = 300
  , txFee              = 174_000
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
  , txCertificates     = []
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

-- | A 29-byte enterprise key address (no stake credential).
mkOutput :: Word16 -> GenericTxOut
mkOutput idx = GenericTxOut
  { txOutIndex       = idx
  , txOutAddressRaw  = BS.pack [0x60] <> BS.replicate 28 0xaa
  , txOutValue       = 5_000_000
  , txOutDataHash    = Nothing
  , txOutInlineDatum = Nothing
  , txOutRefScript   = Nothing
  , txOutMultiAssets = []
  }

emptyBlock :: GenericBlock
emptyBlock = GenericBlock
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

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)
