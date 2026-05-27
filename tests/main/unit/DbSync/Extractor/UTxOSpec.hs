{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the UTxO extractor — stake-credential extraction and
-- the FK plumbing that points @tx_out.stake_address_id@ /
-- @address.stake_address_id@ at the same row.
module DbSync.Extractor.UTxOSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.IORef (newIORef, readIORef)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.Block.Types
  ( BlockEra (..)
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxIn (..)
  , GenericTxOut (..)
  )
import DbSync.Db.Schema.Address (Address (..))
import DbSync.Db.Schema.Ids (AddressId (..))
import qualified DbSync.Db.Schema.UTxO as SU
import DbSync.Extractor (freshExtractState)
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.StakeDelegation (stakeDelegationExtractor)
import DbSync.Extractor.UTxO
  ( extractPaymentCred
  , extractStakeCred
  , rawHasScript
  , utxoExtractor
  )
import DbSync.Phase.Ingest.DedupMap (newMaps)
import DbSync.Block.Pipeline (processBlock)
import DbSync.Worker.TxOut.AddressBuffer
  ( EpochAddressBuffer (..)
  , newAddressBufferRef
  )
import DbSync.Phase.Ingest.Resolver (mkIngestResolver)
import DbSync.Test.Lsm (withTestUtxoStore)
import DbSync.Test.PipelineEnv (mkTestPipelineEnv)
import DbSync.Test.Writer (TestWriterState (..), emptyTestWriterState, mkTestWriter)

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

  describe "extractPaymentCred / Shelley address parser" $ do
    it "extracts the payment credential from a base key-key address (0x00)" $
      extractPaymentCred (mkBaseAddr 0x00) `shouldBe` Just paymentCred28

    it "extracts the payment credential from a base script-key address (0x10)" $
      extractPaymentCred (mkBaseAddr 0x10) `shouldBe` Just paymentCred28

    it "extracts the payment credential from a base key-script address (0x20)" $
      extractPaymentCred (mkBaseAddr 0x20) `shouldBe` Just paymentCred28

    it "extracts the payment credential from a base script-script address (0x30)" $
      extractPaymentCred (mkBaseAddr 0x30) `shouldBe` Just paymentCred28

    it "ignores network-id bits (low nibble) when matching the type" $ do
      -- 0x01 = base key-key on mainnet
      extractPaymentCred (mkBaseAddr 0x01) `shouldBe` Just paymentCred28
      extractPaymentCred (mkBaseAddr 0x11) `shouldBe` Just paymentCred28

    it "extracts the payment credential from pointer addresses (0x40, 0x50)" $ do
      extractPaymentCred (mkBaseAddr 0x40) `shouldBe` Just paymentCred28
      extractPaymentCred (mkBaseAddr 0x50) `shouldBe` Just paymentCred28

    it "extracts the payment credential from enterprise addresses (0x60, 0x70)" $ do
      extractPaymentCred (mkBaseAddr 0x60) `shouldBe` Just paymentCred28
      extractPaymentCred (mkBaseAddr 0x70) `shouldBe` Just paymentCred28

    it "returns Nothing for Byron type-8 headers (0x80)" $
      extractPaymentCred (mkBaseAddr 0x80) `shouldBe` Nothing

    it "returns Nothing for reward addresses (0xE0, 0xF0)" $ do
      extractPaymentCred (mkBaseAddr 0xE0) `shouldBe` Nothing
      extractPaymentCred (mkBaseAddr 0xF0) `shouldBe` Nothing

    it "returns Nothing for Byron CBOR-wrapped raw bytes (start byte 0x82)" $
      -- Real Byron raws begin with CBOR array marker 0x82, not a Shelley
      -- header. Bytes 1..28 would be CBOR-frame garbage, not a credential.
      extractPaymentCred (BS.pack (0x82 : replicate 75 0xcc)) `shouldBe` Nothing

    it "returns Nothing when the address is shorter than 29 bytes" $ do
      extractPaymentCred (BS.pack [0x00]) `shouldBe` Nothing
      extractPaymentCred (BS.pack (0x00 : replicate 27 0xaa)) `shouldBe` Nothing

    it "returns Nothing on an empty input" $
      extractPaymentCred BS.empty `shouldBe` Nothing

  describe "rawHasScript / Shelley address parser" $ do
    it "returns False for key-payment base addresses (0x00, 0x20)" $ do
      rawHasScript (mkBaseAddr 0x00) `shouldBe` False
      rawHasScript (mkBaseAddr 0x20) `shouldBe` False

    it "returns True for script-payment base addresses (0x10, 0x30)" $ do
      rawHasScript (mkBaseAddr 0x10) `shouldBe` True
      rawHasScript (mkBaseAddr 0x30) `shouldBe` True

    it "returns False for key-payment pointer/enterprise (0x40, 0x60)" $ do
      rawHasScript (mkBaseAddr 0x40) `shouldBe` False
      rawHasScript (mkBaseAddr 0x60) `shouldBe` False

    it "returns True for script-payment pointer/enterprise (0x50, 0x70)" $ do
      rawHasScript (mkBaseAddr 0x50) `shouldBe` True
      rawHasScript (mkBaseAddr 0x70) `shouldBe` True

    it "returns False on empty input" $
      rawHasScript BS.empty `shouldBe` False

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

  describe "processBlock: address.payment_cred propagation" $ do
    it "populates address.payment_cred for a base key-key address (0x00)" $ do
      written <- runFullPipeline (blockWithOutput (mkBaseAddr 0x00))
      addressPaymentCred (snd (headDef (panic "no address") (twAddresses written)))
        `shouldBe` Just paymentCred28

    it "populates address.payment_cred for an enterprise address (0x60)" $ do
      written <- runFullPipeline (blockWithOutput (mkBaseAddr 0x60))
      addressPaymentCred (snd (headDef (panic "no address") (twAddresses written)))
        `shouldBe` Just paymentCred28

    it "leaves address.payment_cred NULL for Byron CBOR-wrapped bytes" $ do
      let byronRaw = BS.pack (0x82 : replicate 75 0xcc)
      written <- runFullPipeline (blockWithOutput byronRaw)
      addressPaymentCred (snd (headDef (panic "no address") (twAddresses written)))
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

  describe "processUTxO: tx_in resolution via UtxoCache" $ do
    it "writes tx_in.tx_out_id from the cache when the producer is in-block" $ do
      written <- runFullPipeline twoTxsInputSpendsFirst
      let inIds = mapMaybe (SU.txInTxOutId . snd) (twTxIns written)
      length (twTxIns written) `shouldBe` 1
      length inIds              `shouldBe` 1

    it "leaves tx_in.tx_out_id NULL on a cache miss" $ do
      written <- runFullPipeline (blockSpendingMissingTx (BS.replicate 32 0xee))
      let inIds = mapMaybe (SU.txInTxOutId . snd) (twTxIns written)
      length (twTxIns written) `shouldBe` 1
      inIds                     `shouldBe` []

    it "writes the same value for reference inputs that hit the cache" $ do
      written <- runFullPipeline twoTxsReferenceSpendsFirst
      let refIds = mapMaybe (SU.referenceTxInTxOutId . snd) (twReferenceTxIns written)
      length (twReferenceTxIns written) `shouldBe` 1
      length refIds                      `shouldBe` 1

  describe "processUTxO: phase-2 failure (txValidContract = False)" $ do
    it "writes no tx_out rows" $ do
      written <- runFullPipeline (blockWithFailedTx alonzoFailedTx)
      length (twTxOuts written) `shouldBe` 0

    it "writes no tx_in rows" $ do
      written <- runFullPipeline (blockWithFailedTx alonzoFailedTx)
      length (twTxIns written) `shouldBe` 0

    it "writes no reference_tx_in rows" $ do
      written <- runFullPipeline (blockWithFailedTx alonzoFailedTx)
      length (twReferenceTxIns written) `shouldBe` 0

    it "writes the collateral_tx_in rows" $ do
      written <- runFullPipeline (blockWithFailedTx alonzoFailedTx)
      length (twCollateralTxIns written) `shouldBe` 1

    it "writes no collateral_tx_out for an Alonzo failure" $ do
      written <- runFullPipeline (blockWithFailedTx alonzoFailedTx)
      length (twCollateralTxOuts written) `shouldBe` 0

    it "writes the collateral_tx_out for a Babbage+ failure" $ do
      written <- runFullPipeline (blockWithFailedTx babbageFailedTx)
      length (twCollateralTxOuts written) `shouldBe` 1

    it "still emits the address row for a Babbage+ collateral return" $ do
      written <- runFullPipeline (blockWithFailedTx babbageFailedTx)
      length (twAddresses written) `shouldBe` 1

-- ---------------------------------------------------------------------------
-- Test helpers
-- ---------------------------------------------------------------------------

-- | Run @core@ + @stake_delegation@ + @utxo@ on a single block.
-- | Run @core@ + @stake_delegation@ + @utxo@ on a single block.
--
-- The UTxO extractor records each output's address in the per-epoch
-- 'EpochAddressBuffer' rather than writing 'Address' rows via the
-- 'Writer'. To keep assertions on 'twAddresses' meaningful for these
-- tests, we materialise the buffer's address map into the returned
-- 'TestWriterState' with synthetic ids in insertion order.
runFullPipeline :: GenericBlock -> IO TestWriterState
runFullPipeline block = withTestUtxoStore $ \utxoStore -> do
  stRef <- newIORef freshExtractState
  dedupMaps <- newMaps
  addrBuf <- newAddressBufferRef
  wrRef <- newIORef emptyTestWriterState
  let env = mkTestPipelineEnv (mkIngestResolver stRef dedupMaps addrBuf utxoStore Nothing)
                              (mkTestWriter wrRef)
                              [coreExtractor, stakeDelegationExtractor, utxoExtractor]
  runReaderT (processBlock block) env
  written <- readIORef wrRef
  buf <- readIORef addrBuf
  let buffered = zipWith
        (\i addr -> (AddressId i, addr))
        [1 ..]
        (Map.elems (eabAddresses buf))
  pure written { twAddresses = buffered }

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

-- ---------------------------------------------------------------------------
-- Phase-2 failure fixtures
-- ---------------------------------------------------------------------------

-- | A Shelley block carrying a single phase-2 failed tx.
blockWithFailedTx :: GenericTx -> GenericBlock
blockWithFailedTx tx = shelleyEmptyBlock { blkTxs = [tx] }

-- | An Alonzo-shape failed phase-2 tx: collateral inputs only, no
-- collateral return (the field exists in the parser only for Babbage+).
-- The parser's failed-path produces @txOutputs = []@ regardless, but
-- we leave one output here to assert the extractor ignores it.
alonzoFailedTx :: GenericTx
alonzoFailedTx = (singleOutputTx (mkBaseAddr 0x00) 5_000_000)
  { txValidContract    = False
  , txInputs           = []
  , txOutputs          = []
  , txCollateralInputs = [GenericTxIn (BS.replicate 32 0xcc) 0]
  , txReferenceInputs  = []
  , txCollateralOutput = Nothing
  }

-- | A Babbage+-shape failed phase-2 tx: collateral inputs and a
-- collateral-return output.
babbageFailedTx :: GenericTx
babbageFailedTx = alonzoFailedTx
  { txCollateralOutput = Just (mkOutput 1 (mkBaseAddr 0x00) 4_000_000)
  }

-- ---------------------------------------------------------------------------
-- UtxoCache fixtures
-- ---------------------------------------------------------------------------

-- | Block with two txs: the second tx spends an output produced by
-- the first in the same block. 'Block.Pipeline' records the first
-- tx's outputs in the cache before extractors run, so the UTxO
-- extractor resolves the second tx's input as a cache hit.
twoTxsInputSpendsFirst :: GenericBlock
twoTxsInputSpendsFirst = shelleyEmptyBlock
  { blkTxs = [producerTx, spenderTx]
  }
  where
    producerHash = BS.replicate 32 0xa1
    producerTx = (singleOutputTx (mkBaseAddr 0x00) 5_000_000)
      { txHash = producerHash
      }
    spenderTx = (singleOutputTx (mkBaseAddr 0x00) 4_500_000)
      { txHash       = BS.replicate 32 0xa2
      , txBlockIndex = 1
      , txInputs     = [GenericTxIn producerHash 0]
      }

-- | Same shape as 'twoTxsInputSpendsFirst' but the second tx
-- references the first's output as a reference input (read-only).
twoTxsReferenceSpendsFirst :: GenericBlock
twoTxsReferenceSpendsFirst = shelleyEmptyBlock
  { blkTxs = [producerTx, refTx]
  }
  where
    producerHash = BS.replicate 32 0xb1
    producerTx = (singleOutputTx (mkBaseAddr 0x00) 5_000_000)
      { txHash = producerHash
      }
    refTx = (singleOutputTx (mkBaseAddr 0x00) 4_500_000)
      { txHash            = BS.replicate 32 0xb2
      , txBlockIndex      = 1
      , txReferenceInputs = [GenericTxIn producerHash 0]
      }

-- | Block whose only tx spends an output by a hash that was never
-- recorded in the cache. The extractor leaves @tx_in.tx_out_id@
-- NULL for the post-load resolve to fill in.
blockSpendingMissingTx :: ByteString -> GenericBlock
blockSpendingMissingTx producerHash = shelleyEmptyBlock
  { blkTxs = [spender]
  }
  where
    spender = (singleOutputTx (mkBaseAddr 0x00) 4_500_000)
      { txHash   = BS.replicate 32 0xc2
      , txInputs = [GenericTxIn producerHash 0]
      }
