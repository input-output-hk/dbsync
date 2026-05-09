{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Core extractor.
--
-- Verifies that 'coreExtractor' correctly transforms 'GenericBlock' values
-- into typed Block, Tx, and SlotLeader records via the 'IdResolver' + 'Writer'
-- pipeline. All tests use hand-crafted 'GenericBlock' fixtures and a test
-- writer — no node or DB needed.
module DbSync.Extractor.CoreSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.IORef (newIORef, readIORef)

import qualified Data.ByteString as BS

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Data.Map.Strict as Map

import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin (Coin (..))

import DbSync.Block.Types
  ( BlockEra (..)
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxIn (..)
  , GenericTxOut (..)
  )
import qualified DbSync.Block.Types as G
import DbSync.Db.Schema.Core (Block (..), SlotLeader (..))
import qualified DbSync.Db.Schema.Core as SC
import DbSync.Db.Schema.Ids (BlockId (..), TxId (..))
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Extractor
  ( BlockLedgerData (..)
  , ExtractorDef (..)
  , emptyBlockLedgerData
  , freshExtractState
  )
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Ingest.Pipeline (processBlock)
import DbSync.Id.DedupMap (newMaps)
import DbSync.Ledger.Types (DepositsMap (..))
import DbSync.Phase (SyncPhase (..))
import DbSync.Resolver (IdResolver (..))
import DbSync.Resolver.Ingest (mkIngestResolver)
import DbSync.Test.PipelineEnv (mkTestPipelineEnv, mkTestPipelineEnvWith)
import DbSync.Writer.Testing (TestWriterState (..), emptyTestWriterState, mkTestWriter)
import Test.Hspec (shouldSatisfy)

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

spec :: Spec
spec = do
  describe "coreExtractor definition" $ do
    it "has name 'core'" $
      pdName coreExtractor `shouldBe` "core"

    it "has version 1" $
      pdVersion coreExtractor `shouldBe` 1

    it "has no dependencies" $
      pdDependencies coreExtractor `shouldBe` []

    it "owns three tables: block, tx, slot_leader" $
      length (pdTables coreExtractor) `shouldBe` 3

  describe "extraction: empty block (0 txs)" $ do
    it "produces 1 block row and 0 tx rows" $ do
      written <- runCore emptyBlock
      length (twBlocks written) `shouldBe` 1
      length (twTxs written) `shouldBe` 0

    it "produces 1 slot_leader row on first encounter" $ do
      written <- runCore emptyBlock
      length (twSlotLeaders written) `shouldBe` 1

  describe "extraction: block with 3 transactions" $ do
    it "produces 1 block row and 3 tx rows" $ do
      written <- runCore blockWith3Txs
      length (twBlocks written) `shouldBe` 1
      length (twTxs written) `shouldBe` 3

  describe "extraction: two blocks with same slot leader" $ do
    it "only produces slot_leader row on first block" $ do
      (w1, w2) <- runCoreTwoBlocks emptyBlock emptyBlock2
      length (twSlotLeaders w1) `shouldBe` 1
      length (twSlotLeaders w2) `shouldBe` 0

  describe "extraction: two blocks with different slot leaders" $ do
    it "produces a slot_leader row for each" $ do
      let differentLeader = emptyBlock2 { blkSlotLeader = BS.replicate 28 0xff }
      (w1, w2) <- runCoreTwoBlocks emptyBlock differentLeader
      length (twSlotLeaders w1) `shouldBe` 1
      length (twSlotLeaders w2) `shouldBe` 1

  describe "extraction: ID monotonicity" $ do
    it "block IDs increase across sequential blocks" $ do
      (w1, w2) <- runCoreTwoBlocks emptyBlock emptyBlock2
      fst (headDef (panic "no block") (twBlocks w1)) `shouldBe` BlockId 1
      fst (headDef (panic "no block") (twBlocks w2)) `shouldBe` BlockId 2

    it "tx IDs increase across blocks" $ do
      let block2 = blockWith3Txs { blkHash = BS.replicate 32 2, blkBlockNo = BlockNo 2 }
      (w1, w2) <- runCoreTwoBlocks blockWith3Txs block2
      map fst (twTxs w1) `shouldBe` [TxId 1, TxId 2, TxId 3]
      map fst (twTxs w2) `shouldBe` [TxId 4, TxId 5, TxId 6]

  describe "extraction: block row correctness" $ do
    it "tx_count matches number of transactions" $ do
      written <- runCore blockWith3Txs
      let (_, blk) = headDef (panic "no block") (twBlocks written)
      blockTxCount blk `shouldBe` 3

    it "previous_id is Nothing for first block" $ do
      written <- runCore emptyBlock
      let (_, blk) = headDef (panic "no block") (twBlocks written)
      blockPreviousId blk `shouldBe` Nothing

    it "previous_id is set for second block" $ do
      (_, w2) <- runCoreTwoBlocks emptyBlock emptyBlock2
      let (_, blk) = headDef (panic "no block") (twBlocks w2)
      blockPreviousId blk `shouldBe` Just (BlockId 1)

  describe "extraction: tx row correctness" $ do
    it "block_id in tx rows matches the block's ID" $ do
      written <- runCore blockWith3Txs
      let blockIds = map (SC.txBlockId . snd) (twTxs written)
      blockIds `shouldBe` [BlockId 1, BlockId 1, BlockId 1]

    it "block_index is sequential within the block" $ do
      written <- runCore blockWith3Txs
      let idxs = map (SC.txBlockIndex . snd) (twTxs written)
      idxs `shouldBe` [0, 1, 2]

  describe "mkSlotLeader / pool hash propagation" $ do
    it "Shelley+ blocks populate slot_leader.pool_hash_id" $ do
      written <- runCore emptyBlock
      let (_, sl) = headDef (panic "no slot_leader") (twSlotLeaders written)
      slotLeaderPoolHashId sl `shouldSatisfy` isJust

    it "writes a pool_hash row for the Shelley slot leader" $ do
      written <- runCore emptyBlock
      length (twPoolHashes written) `shouldBe` 1

    it "Byron blocks leave slot_leader.pool_hash_id NULL" $ do
      written <- runCore byronBlock
      let (_, sl) = headDef (panic "no slot_leader") (twSlotLeaders written)
      slotLeaderPoolHashId sl `shouldBe` Nothing

    it "Byron blocks write no pool_hash row" $ do
      written <- runCore byronBlock
      length (twPoolHashes written) `shouldBe` 0

  describe "tx.fee / tx.deposit dispatch" $ do

    describe "phase-2 failure" $ do
      it "Ingest leaves fee at the parser sentinel and deposit at 0" $ do
        written <- runCoreWith emptyBlockLedgerData IngestChainHistory
                     [] (blockWithTx phase2Tx)
        case twTxs written of
          [(_, tx)] -> do
            SC.txFee tx     `shouldBe` DbLovelace 0
            SC.txDeposit tx `shouldBe` Just 0
          _ -> panic "expected exactly one tx"

      it "Follow computes fee from collateral inputs minus collateral return" $ do
        let collInValues = [Just (DbLovelace 5_000_000)]
        written <- runCoreWith emptyBlockLedgerData FollowingChainTip
                     collInValues (blockWithTx phase2Tx)
        case twTxs written of
          [(_, tx)] -> do
            -- 5_000_000 collateral in - 2_000_000 collateral out
            SC.txFee tx     `shouldBe` DbLovelace 3_000_000
            SC.txDeposit tx `shouldBe` Just 0
          _ -> panic "expected exactly one tx"

    describe "valid contract, ledger ON" $ do
      it "fills deposit from bcDepositsMap when the tx has a deposit event" $ do
        let bld = (emptyBlockLedgerData :: BlockLedgerData)
              { bldLedgerEnabled = True
              , bldDepositsMap   = DepositsMap
                  (Map.singleton (G.txHash validTx) (Coin 2_000_000))
              }
        written <- runCoreWith bld IngestChainHistory [] (blockWithTx validTx)
        case twTxs written of
          [(_, tx)] -> SC.txDeposit tx `shouldBe` Just 2_000_000
          _ -> panic "expected exactly one tx"

      it "leaves deposit NULL for plain txs (no deposit event)" $ do
        let bld = emptyBlockLedgerData { bldLedgerEnabled = True }
        written <- runCoreWith bld IngestChainHistory [] (blockWithTx validTx)
        case twTxs written of
          [(_, tx)] -> SC.txDeposit tx `shouldBe` Nothing
          _ -> panic "expected exactly one tx"

    describe "valid contract, ledger OFF" $ do
      it "Follow computes deposit via the inputs - outputs identity" $ do
        let inValues = [Just (DbLovelace 10_000_000)]
            tx = validTx
              { G.txInputs   = [GenericTxIn (BS.replicate 32 0xaa) 0]
              , G.txOutSum   = 9_000_000
              , G.txFee      = 200_000
              , G.txOutputs  = [outFor 9_000_000]
              }
        written <- runCoreWith emptyBlockLedgerData FollowingChainTip
                     inValues (blockWithTx tx)
        case twTxs written of
          [(_, t)] ->
            -- 10_000_000 - 9_000_000 - 200_000 - 0 (donation) = 800_000
            SC.txDeposit t `shouldBe` Just 800_000
          _ -> panic "expected exactly one tx"

      it "Ingest leaves deposit NULL for the SQL backfill to fill in" $ do
        written <- runCoreWith emptyBlockLedgerData IngestChainHistory
                     [] (blockWithTx validTx)
        case twTxs written of
          [(_, tx)] -> SC.txDeposit tx `shouldBe` Nothing
          _ -> panic "expected exactly one tx"

-- ---------------------------------------------------------------------------
-- Test helpers
-- ---------------------------------------------------------------------------

-- | Run the core extractor on a single block, return written records.
runCore :: GenericBlock -> IO TestWriterState
runCore block = do
  stRef <- newIORef freshExtractState
  dedupMaps <- newMaps
  wrRef <- newIORef emptyTestWriterState
  let env = mkTestPipelineEnv (mkIngestResolver stRef dedupMaps)
                              (mkTestWriter wrRef) [coreExtractor]
  runReaderT (processBlock block) env
  readIORef wrRef

-- | Run the core extractor on two blocks sequentially, return separate results.
runCoreTwoBlocks :: GenericBlock -> GenericBlock -> IO (TestWriterState, TestWriterState)
runCoreTwoBlocks block1 block2 = do
  stRef <- newIORef freshExtractState
  dedupMaps <- newMaps
  let resolver = mkIngestResolver stRef dedupMaps

  wrRef1 <- newIORef emptyTestWriterState
  let env1 = mkTestPipelineEnv resolver (mkTestWriter wrRef1) [coreExtractor]
  runReaderT (processBlock block1) env1
  w1 <- readIORef wrRef1

  wrRef2 <- newIORef emptyTestWriterState
  let env2 = mkTestPipelineEnv resolver (mkTestWriter wrRef2) [coreExtractor]
  runReaderT (processBlock block2) env2
  w2 <- readIORef wrRef2

  pure (w1, w2)

-- ---------------------------------------------------------------------------
-- Test fixtures
-- ---------------------------------------------------------------------------

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)

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

emptyBlock2 :: GenericBlock
emptyBlock2 = emptyBlock
  { blkHash    = BS.replicate 32 1
  , blkBlockNo = BlockNo 2
  , blkSlotNo  = SlotNo 120
  }

-- | A Byron-era block. The slot-leader hash is a genesis-key delegate,
-- not a stake-pool key, so the pool-hash FK must stay NULL.
byronBlock :: GenericBlock
byronBlock = emptyBlock
  { blkEra           = Byron
  , blkHash          = BS.replicate 32 0xc0
  , blkVrfKey        = Nothing
  , blkOpCert        = Nothing
  , blkOpCertCounter = Nothing
  }

blockWith3Txs :: GenericBlock
blockWith3Txs = emptyBlock
  { blkTxs = [mkTx 0 "tx0hash", mkTx 1 "tx1hash", mkTx 2 "tx2hash"]
  }

mkTx :: Word64 -> ByteString -> GenericTx
mkTx idx txH = GenericTx
  { txHash              = txH <> BS.replicate (32 - BS.length txH) 0
  , txBlockIndex        = idx
  , txSize              = 300
  , txFee               = 174000
  , txOutSum            = 5000000
  , txValidContract     = True
  , txScriptSize        = 0
  , txTreasuryDonation  = 0
  , txInvalidBefore     = Nothing
  , txInvalidHereafter  = Nothing
  , txInputs            = []
  , txOutputs           = []
  , txCollateralInputs  = []
  , txReferenceInputs   = []
  , txCollateralOutput  = Nothing
  , txCertificates      = []
  , txWithdrawals       = []
  , txMetadata          = Nothing
  , txMint              = []
  , txCborRaw           = Nothing
  }

-- ---------------------------------------------------------------------------
-- Fee / deposit dispatch helpers
-- ---------------------------------------------------------------------------

-- | Run 'coreExtractor' with custom 'BlockLedgerData', 'SyncPhase',
-- and a stubbed 'resolveInputValues' return value.
runCoreWith
  :: BlockLedgerData
  -> SyncPhase
  -> [Maybe DbLovelace]   -- ^ what 'resolveInputValues' should return
  -> GenericBlock
  -> IO TestWriterState
runCoreWith ledgerData phase inValues block = do
  stRef     <- newIORef freshExtractState
  dedupMaps <- newMaps
  wrRef     <- newIORef emptyTestWriterState
  let baseResolver = mkIngestResolver stRef dedupMaps
      resolver = baseResolver { resolveInputValues = \_ -> pure inValues }
      env = mkTestPipelineEnvWith Mainnet resolver (mkTestWriter wrRef)
              [coreExtractor] (\_ -> pure ledgerData) phase
  runReaderT (processBlock block) env
  readIORef wrRef

-- | A valid (phase-2 success) tx with no inputs / outputs / withdrawals
-- by default. Field overrides supply the dispatch-relevant data.
validTx :: GenericTx
validTx = mkTx 0 "validtx"

-- | A phase-2 failed tx with one collateral input and a 2_000_000
-- collateral return.
phase2Tx :: GenericTx
phase2Tx = (mkTx 0 "phase2tx")
  { G.txValidContract    = False
  , G.txFee              = 0
  , G.txOutSum           = 0
  , G.txOutputs          = []
  , G.txCollateralInputs = [GenericTxIn (BS.replicate 32 0xbb) 0]
  , G.txCollateralOutput = Just (outFor 2_000_000)
  }

-- | A 'GenericTxOut' carrying the supplied lovelace value. Address
-- bytes are the same Shelley-shape pad used by every other test
-- fixture in the suite.
outFor :: Word64 -> GenericTxOut
outFor v = GenericTxOut
  { G.txOutIndex       = 0
  , G.txOutAddress     = "addr_test1xyz"
  , G.txOutAddressRaw  = BS.pack (0x00 : replicate 56 0x11)
  , G.txOutValue       = v
  , G.txOutDataHash    = Nothing
  , G.txOutInlineDatum = Nothing
  , G.txOutRefScript   = Nothing
  , G.txOutMultiAssets = []
  }

-- | Wrap a tx in 'emptyBlock'.
blockWithTx :: GenericTx -> GenericBlock
blockWithTx tx = emptyBlock { blkTxs = [tx] }
