{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the scripts_datums extractor.
--
-- Each test runs 'processBlock' through @coreExtractor@ plus
-- @scriptsDatumsExtractor@ against a hand-built 'GenericTx' that
-- exercises one witness kind in isolation.
module DbSync.Extractor.ScriptsDatumsSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.IORef (newIORef, readIORef)

import qualified Data.ByteString as BS

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Prices (..), txscriptfee)
import Cardano.Ledger.BaseTypes (Network (..), boundRational)

import DbSync.Parser.Types
  ( BlockEra (..)
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxDatum (..)
  , GenericTxRedeemer (..)
  , GenericTxScript (..)
  )
import qualified DbSync.Db.Schema.ScriptsDatums as SSD
import DbSync.Db.Types (DbLovelace (..), ScriptPurpose (..), ScriptType (..))
import DbSync.Extractor
  ( BlockLedgerData (..)
  , LedgerOutputs (..)
  , emptyLedgerOutputs
  , freshExtractState
  )
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.Pipeline (processBlock)
import DbSync.Extractor.ScriptsDatums (redeemerScriptFee, scriptsDatumsExtractor)
import DbSync.Phase.Ingest.Resolver (mkIngestResolver)
import DbSync.Phase.Type (SyncPhase (..))
import DbSync.Test.Lsm (withTestIngestStores)
import DbSync.Test.PipelineEnv (mkTestPipelineEnvOn, mkTestPipelineEnvWith)
import DbSync.Test.Writer (TestWriterState (..), emptyTestWriterState, mkTestWriter)
import DbSync.Util (coinToDbLovelace)
import DbSync.Worker.TxOut.AddressBuffer (newAddressBufferRef)

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

spec :: Spec
spec = do
  describe "scripts" $ do

    it "writes one script row per witness script" $ do
      written <- runOne (txWithScripts [sampleNativeScript])
      length (twScripts written) `shouldBe` 1
      case twScripts written of
        [(_, sc)] -> do
          SSD.scriptHash sc `shouldBe` gtsHash sampleNativeScript
          SSD.scriptType sc `shouldBe` Timelock
          SSD.scriptJson sc `shouldBe` Just sampleJsonText
        _ -> panic "expected exactly one script"

    it "deduplicates two scripts with the same hash" $ do
      written <- runOne (txWithScripts [sampleNativeScript, sampleNativeScript])
      length (twScripts written) `shouldBe` 1

    it "skips a phase-2 failed tx entirely" $ do
      written <- runOne ((txWithScripts [sampleNativeScript]) { txValidContract = False })
      length (twScripts written) `shouldBe` 0

  describe "datums" $ do

    it "writes one datum row and dedups by hash" $ do
      written <- runOne (txWithDatums [sampleDatum, sampleDatum])
      length (twDatums written) `shouldBe` 1
      case twDatums written of
        [(_, d)] -> SSD.datumValue d `shouldBe` gtdValue sampleDatum
        _ -> panic "expected exactly one datum"

  describe "extra_key_witness" $ do

    it "writes one row per hash, no deduplication" $ do
      let signers =
            [ BS.replicate 28 0x11
            , BS.replicate 28 0x22
            , BS.replicate 28 0x11
            ]
      written <- runOne (txWithExtraKeys signers)
      length (twExtraKeyWitnesses written) `shouldBe` 3
      let stored = map SSD.extraKeyWitnessHash (twExtraKeyWitnesses written)
      stored `shouldBe` signers

  describe "redeemers" $ do

    it "writes one redeemer plus one redeemer_data row" $ do
      written <- runOne (txWithRedeemers [sampleRedeemer])
      length (twRedeemers written) `shouldBe` 1
      length (twRedeemerData written) `shouldBe` 1
      case (twRedeemers written, twRedeemerData written) of
        ([(_, r)], [(rdId, _)]) -> do
          SSD.redeemerRedeemerDataId r `shouldBe` rdId
          SSD.redeemerPurpose r `shouldBe` gtrPurpose sampleRedeemer
          SSD.redeemerUnitMem r `shouldBe` gtrUnitMem sampleRedeemer
          SSD.redeemerUnitSteps r `shouldBe` gtrUnitSteps sampleRedeemer
          -- runOne is ledger-OFF: no prices, so the fee stays NULL.
          SSD.redeemerFee r `shouldBe` Nothing
        _ -> panic "expected one redeemer + one redeemer_data"

    it "two redeemers sharing a datum produce one redeemer_data row" $ do
      written <- runOne (txWithRedeemers [sampleRedeemer, sampleRedeemer { gtrIndex = 1 }])
      length (twRedeemers written) `shouldBe` 2
      length (twRedeemerData written) `shouldBe` 1
      let dataIds = map (SSD.redeemerRedeemerDataId . snd) (twRedeemers written)
      dataIds `shouldSatisfy` allEqual

  describe "redeemer fee" $ do

    it "takes the ceiling of the summed price products" $
      -- 1*(1/3) + 1*(1/3) = 2/3, so the fee is 1; a per-term
      -- ceiling would give 2.
      redeemerScriptFee (mkPrices (1 % 3) (1 % 3)) 1 1 `shouldBe` DbLovelace 1

    it "computes known fees from known prices and units" $ do
      -- 3*(1/2) + 2*(1/4) = 2 exactly.
      redeemerScriptFee (mkPrices (1 % 2) (1 % 4)) 3 2 `shouldBe` DbLovelace 2
      -- Mainnet prices: ceil(1700*0.0577 + 476468*0.0000721)
      --               = ceil(98.09 + 34.3533428) = 133.
      redeemerScriptFee mainnetPrices 1_700 476_468 `shouldBe` DbLovelace 133

    it "agrees with txscriptfee" $
      redeemerScriptFee mainnetPrices 1000 250_000
        `shouldBe` coinToDbLovelace (txscriptfee mainnetPrices (ExUnits 1000 250_000))

    it "populates redeemer.fee from the block's prices when ledger is ON" $ do
      let bld = LedgerDataOn $ emptyLedgerOutputs
            { loPrices = Just mainnetPrices }
      written <- runOneWith bld (txWithRedeemers [sampleRedeemer])
      case twRedeemers written of
        -- ceil(1000*0.0577 + 250000*0.0000721) = ceil(57.7 + 18.025) = 76.
        [(_, r)] -> SSD.redeemerFee r `shouldBe` Just (DbLovelace 76)
        _        -> panic "expected exactly one redeemer"

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

-- ---------------------------------------------------------------------------
-- Plumbing
-- ---------------------------------------------------------------------------

runOne :: GenericTx -> IO TestWriterState
runOne tx = withTestIngestStores $ \utxoStore dedupStores -> do
  stRef   <- newIORef freshExtractState
  addrBuf <- newAddressBufferRef
  wrRef   <- newIORef emptyTestWriterState
  let env = mkTestPipelineEnvOn Mainnet
              (mkIngestResolver stRef dedupStores addrBuf utxoStore Nothing)
              (mkTestWriter wrRef)
              [coreExtractor, scriptsDatumsExtractor]
  runReaderT (processBlock (blockWith tx)) env
  readIORef wrRef

-- | Like 'runOne' but with a caller-supplied 'BlockLedgerData'.
runOneWith :: BlockLedgerData -> GenericTx -> IO TestWriterState
runOneWith bld tx = withTestIngestStores $ \utxoStore dedupStores -> do
  stRef   <- newIORef freshExtractState
  addrBuf <- newAddressBufferRef
  wrRef   <- newIORef emptyTestWriterState
  let env = mkTestPipelineEnvWith Mainnet
              (mkIngestResolver stRef dedupStores addrBuf utxoStore Nothing)
              (mkTestWriter wrRef)
              [coreExtractor, scriptsDatumsExtractor]
              (\_ -> pure bld) IngestChainHistory
  runReaderT (processBlock (blockWith tx)) env
  readIORef wrRef

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

sampleHash32 :: ByteString
sampleHash32 = BS.replicate 32 0xab

mkPrices :: Rational -> Rational -> Prices
mkPrices m s = Prices
  { prMem   = fromMaybe (panic "mkPrices: bad mem price") (boundRational m)
  , prSteps = fromMaybe (panic "mkPrices: bad steps price") (boundRational s)
  }

mainnetPrices :: Prices
mainnetPrices = mkPrices (577 % 10_000) (721 % 10_000_000)

sampleNativeScript :: GenericTxScript
sampleNativeScript = GenericTxScript
  { gtsHash           = BS.replicate 28 0xcc
  , gtsType           = Timelock
  , gtsJson           = Just sampleJsonText
  , gtsBytes          = Nothing
  , gtsSerialisedSize = Nothing
  }

sampleJsonText :: Text
sampleJsonText = "{\"type\":\"sig\",\"keyHash\":\"abababab\"}"

sampleDatum :: GenericTxDatum
sampleDatum = GenericTxDatum
  { gtdHash  = sampleHash32
  , gtdBytes = BS.pack [0x01, 0x02, 0x03]
  , gtdValue = Just "{\"int\":42}"
  }

sampleRedeemer :: GenericTxRedeemer
sampleRedeemer = GenericTxRedeemer
  { gtrUnitMem    = 1000
  , gtrUnitSteps  = 250000
  , gtrPurpose    = Spend
  , gtrIndex      = 0
  , gtrScriptHash = Nothing
  , gtrDataHash   = sampleHash32
  , gtrDataBytes  = gtdBytes sampleDatum
  , gtrDataValue  = gtdValue sampleDatum
  }

baseTx :: GenericTx
baseTx = GenericTx
  { txHash             = BS.replicate 32 0x00
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

txWithScripts :: [GenericTxScript] -> GenericTx
txWithScripts ss = baseTx { txScripts = ss }

txWithDatums :: [GenericTxDatum] -> GenericTx
txWithDatums ds = baseTx { txDatums = ds }

txWithExtraKeys :: [ByteString] -> GenericTx
txWithExtraKeys ks = baseTx { txExtraKeyWitnesses = ks }

txWithRedeemers :: [GenericTxRedeemer] -> GenericTx
txWithRedeemers rs = baseTx { txRedeemers = rs }

blockWith :: GenericTx -> GenericBlock
blockWith tx = shelleyBlock { blkTxs = [tx] }

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)

shelleyBlock :: GenericBlock
shelleyBlock = GenericBlock
  { blkEra           = Alonzo
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
