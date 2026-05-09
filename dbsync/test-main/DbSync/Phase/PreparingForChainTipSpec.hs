{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end tests for 'DbSync.Phase.PreparingForChainTip.run'.
--
-- The whole pipeline is driven from a small set of 'GenericBlock'
-- fixtures: two blocks, three transactions, a phase-2 failure with
-- collateral. The fixtures flow through the real extractors and
-- COPY writer so the rows the post-load pass operates on are
-- shaped exactly the way 'IngestChainHistory' would have shaped
-- them — no hand-rolled INSERTs.
--
-- After the pipeline drains, 'Phase.PreparingForChainTip.run'
-- runs against the same database and the spec asserts:
--
--   * @tx_in@ / @collateral_tx_in@ FK columns are populated.
--   * @tx_out.consumed_by_tx_id@ tracks the spending tx_in only
--     (collateral consumption is intentionally not tracked).
--   * @tx.fee@ on the phase-2 failure is collateral-in minus
--     collateral-out.
--   * @tx.deposit@ for valid-contract tx is the inputs-minus-
--     outputs fallback; for the phase-2 failure it's @0@.
--   * Tables flipped from UNLOGGED to LOGGED.
--   * Declared unique indexes exist.
--   * @<table>_id_seq@ allocates @MAX(id) + 1@.
--
-- Requires a running PostgreSQL instance with a @dbsync_test@
-- database; runs in the "Database integration" test group.
module DbSync.Phase.PreparingForChainTipSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.IORef (newIORef)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import qualified Data.ByteString as BS
import qualified Data.Text as T

import Test.Hspec
  ( Spec
  , afterAll_
  , beforeAll_
  , describe
  , it
  , shouldBe
  )

import DbSync.Block.Types
  ( BlockEra (..)
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxIn (..)
  , GenericTxOut (..)
  )
import DbSync.Copy.Writer (CopyWriter (..), closeCopyWriter, mkCopyWriter)
import DbSync.Db.Schema.Address (addressTableDef)
import DbSync.Db.Schema.CBOR (txCborTableDef)
import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef, txTableDef)
import DbSync.Db.Schema.Init (dropSchema, initSchema)
import DbSync.Db.Schema.Metadata (txMetadataTableDef)
import DbSync.Db.Schema.MultiAsset
  ( maTxMintTableDef
  , maTxOutTableDef
  , multiAssetTableDef
  )
import DbSync.Db.Schema.Pool
  ( poolHashTableDef
  , poolMetadataRefTableDef
  , poolOwnerTableDef
  , poolRelayTableDef
  , poolRetireTableDef
  , poolUpdateTableDef
  )
import DbSync.Db.Schema.StakeDelegation
  ( delegationTableDef
  , stakeAddressTableDef
  , stakeDeregistrationTableDef
  , stakeRegistrationTableDef
  , withdrawalTableDef
  )
import DbSync.Db.Schema.Types (TableDef)
import DbSync.Db.Schema.UTxO
  ( collateralTxInTableDef
  , collateralTxOutTableDef
  , referenceTxInTableDef
  , txInTableDef
  , txOutTableDef
  )
import DbSync.Extractor (ExtractorDef, freshExtractState)
import DbSync.Extractor.Cbor (cborExtractor)
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.Metadata (metadataExtractor)
import DbSync.Extractor.MultiAsset (multiAssetExtractor)
import DbSync.Extractor.Pool (poolExtractor)
import DbSync.Extractor.StakeDelegation (stakeDelegationExtractor)
import DbSync.Extractor.UTxO (utxoExtractor)
import DbSync.Id.DedupMap (newMaps)
import DbSync.Ingest.Pipeline (processBlock)
import qualified DbSync.Phase.PreparingForChainTip as Prep
import DbSync.Resolver.Ingest (mkIngestResolver)
import DbSync.Test.Database
  ( queryTestDb
  , testConnBs
  , testConnStr
  )
import DbSync.Test.Hasql (withTestConnection)
import DbSync.Test.PipelineEnv (mkTestPipelineEnv)
import DbSync.Writer.CopyAdapter (mkCopyWriterAdapter)

-- ---------------------------------------------------------------------------
-- * Schema setup
-- ---------------------------------------------------------------------------

-- | Every table the active extractors write into. Must include
-- @collateral_tx_out@ (which 'utxoExtractor' writes via the COPY
-- adapter even though its 'pdTables' list omits it) and every
-- table the post-load UPDATEs read from.
tables :: [TableDef]
tables =
  [ blockTableDef
  , txTableDef
  , slotLeaderTableDef
  , addressTableDef
  , txOutTableDef
  , txInTableDef
  , collateralTxInTableDef
  , collateralTxOutTableDef
  , referenceTxInTableDef
  , txMetadataTableDef
  , multiAssetTableDef
  , maTxMintTableDef
  , maTxOutTableDef
  , stakeAddressTableDef
  , stakeRegistrationTableDef
  , stakeDeregistrationTableDef
  , delegationTableDef
  , withdrawalTableDef
  , poolHashTableDef
  , poolUpdateTableDef
  , poolMetadataRefTableDef
  , poolOwnerTableDef
  , poolRetireTableDef
  , poolRelayTableDef
  , txCborTableDef
  ]

versions :: [(Text, Int)]
versions =
  [ ("core", 1)
  , ("utxo", 1)
  , ("metadata", 1)
  , ("multi_asset", 1)
  , ("stake_delegation", 1)
  , ("pool", 1)
  , ("cbor", 1)
  ]

extractors :: [ExtractorDef]
extractors =
  [ coreExtractor
  , utxoExtractor
  , metadataExtractor
  , multiAssetExtractor
  , stakeDelegationExtractor
  , poolExtractor
  , cborExtractor
  ]

-- ---------------------------------------------------------------------------
-- * Pipeline runner
-- ---------------------------------------------------------------------------

-- | Drive the supplied blocks through extractor → resolver → COPY
-- writer → PostgreSQL, then run the post-load pass on the same DB.
runPipelineThenPrepare :: [GenericBlock] -> IO ()
runPipelineThenPrepare blocks = do
  stRef <- newIORef freshExtractState
  dedupMaps <- newMaps
  cw <- mkCopyWriter testConnBs tables
  let env = mkTestPipelineEnv (mkIngestResolver stRef dedupMaps)
                              (mkCopyWriterAdapter cw) extractors
  for_ blocks $ \blk -> runReaderT (processBlock blk) env
  cwCommit cw
  closeCopyWriter cw
  withTestConnection $ \conn -> Prep.run conn tables

-- ---------------------------------------------------------------------------
-- * Setup / teardown
-- ---------------------------------------------------------------------------

setUp :: IO ()
setUp = do
  dropSchema tables versions testConnStr
  initSchema tables versions testConnStr
  runPipelineThenPrepare [producerBlock, spendingBlock]

tearDown :: IO ()
tearDown = dropSchema tables [] testConnStr

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "DbSync.Phase.PreparingForChainTip" $
  beforeAll_ setUp $
  afterAll_  tearDown $ do

    describe "FK resolution" $ do
      it "tx_in.tx_out_id is populated from the producing tx's id" $ do
        result <- T.strip <$> queryTestDb
          "SELECT tx_out_id FROM tx_in WHERE id = 1"
        result `shouldBe` "1"

      it "collateral_tx_in.tx_out_id is populated the same way" $ do
        result <- T.strip <$> queryTestDb
          "SELECT tx_out_id FROM collateral_tx_in WHERE id = 1"
        result `shouldBe` "1"

      it "tx_out.consumed_by_tx_id is set on the spent output only" $ do
        result <- T.strip <$> queryTestDb
          "SELECT id, consumed_by_tx_id FROM tx_out ORDER BY id"
        let rows = T.lines result
        rows `shouldBe`
          [ "1|2"  -- producer.0: spent by the consuming tx_in
          , "2|"   -- producer.1: only collateral consumed it; not tracked
          , "3|"   -- producer.2: never spent
          , "4|"   -- consumer's own output: never spent
          ]

    describe "tx column backfill" $ do
      it "phase-2 failed tx.fee is collateral-in minus collateral-out" $ do
        result <- T.strip <$> queryTestDb
          "SELECT fee FROM tx WHERE valid_contract = FALSE"
        result `shouldBe` "3000000"

      it "phase-2 failed tx.deposit is set to 0" $ do
        result <- T.strip <$> queryTestDb
          "SELECT deposit FROM tx WHERE valid_contract = FALSE"
        result `shouldBe` "0"

      it "valid-contract tx.deposit is inputs - outputs - fee - donation" $ do
        -- 5_000_000 (input) - 4_500_000 (out_sum) - 200_000 (fee)
        --   - 0 (treasury_donation) = 300_000. The producer has no
        --   inputs so its deposit is left NULL by the fallback.
        result <- T.strip <$> queryTestDb
          "SELECT deposit FROM tx WHERE block_id = 2 AND block_index = 0"
        result `shouldBe` "300000"

      it "valid-contract tx.fee is left untouched" $ do
        -- The post-load pass replaces fee only when valid_contract is
        -- FALSE. The consumer's declared fee should be exactly what
        -- the parser wrote.
        result <- T.strip <$> queryTestDb
          "SELECT fee FROM tx WHERE block_id = 2 AND block_index = 0"
        result `shouldBe` "200000"

    describe "schema-mode flip" $
      it "every extractor table is now LOGGED" $ do
        -- pg_class.relpersistence: 'p' = permanent (LOGGED), 'u' = UNLOGGED
        result <- T.strip <$> queryTestDb
          (T.unwords
            [ "SELECT count(*) FROM pg_class"
            , "WHERE relkind = 'r'"
            , "AND relname IN ('block', 'tx', 'tx_out', 'tx_in',"
            , "  'collateral_tx_in', 'collateral_tx_out',"
            , "  'reference_tx_in', 'address', 'slot_leader',"
            , "  'withdrawal')"
            , "AND relpersistence = 'p'"
            ])
        result `shouldBe` "10"

    describe "indexes" $
      it "the address.raw unique index is created" $ do
        result <- T.strip <$> queryTestDb
          (T.unwords
            [ "SELECT count(*) FROM pg_indexes"
            , "WHERE tablename = 'address'"
            , "AND indexname = 'address_unique_1_idx'"
            ])
        result `shouldBe` "1"

    describe "sequence reset" $ do
      it "tx_id_seq's next value is MAX(id) + 1" $ do
        -- Three txs landed (producer + valid-contract spender + phase-2)
        result <- T.strip <$> queryTestDb "SELECT nextval('tx_id_seq')"
        result `shouldBe` "4"

      it "tx_out_id_seq's next value is MAX(id) + 1" $ do
        -- Producer wrote three outputs; consumer wrote one; phase-2
        -- wrote none. MAX(id) = 4, next allocation = 5.
        result <- T.strip <$> queryTestDb "SELECT nextval('tx_out_id_seq')"
        result `shouldBe` "5"

      it "tx_in_id_seq's next value is MAX(id) + 1" $ do
        result <- T.strip <$> queryTestDb "SELECT nextval('tx_in_id_seq')"
        result `shouldBe` "2"

      it "an empty table's sequence still starts at 1" $ do
        -- No reference_tx_in rows were produced; setval(seq, 0+1, false)
        -- leaves nextval at 1.
        result <- T.strip <$>
          queryTestDb "SELECT nextval('reference_tx_in_id_seq')"
        result `shouldBe` "1"

-- ---------------------------------------------------------------------------
-- * GenericBlock fixtures
-- ---------------------------------------------------------------------------

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)

-- | Shelley-shaped raw address. Header byte 0x00 (BasePaymentKey
-- StakeKey) + 56 padding bytes — well-formed enough for the UTxO
-- extractor's stake-credential parser.
sampleAddrRaw :: ByteString
sampleAddrRaw = BS.pack (0x00 : replicate 56 0x11)

-- | Common output shape used in every fixture.
mkOut :: Word16 -> Word64 -> GenericTxOut
mkOut idx value = GenericTxOut
  { txOutIndex       = idx
  , txOutAddress     = "addr_test1xyz"
  , txOutAddressRaw  = sampleAddrRaw
  , txOutValue       = value
  , txOutDataHash    = Nothing
  , txOutInlineDatum = Nothing
  , txOutRefScript   = Nothing
  , txOutMultiAssets = []
  }

-- | An empty Shelley GenericTx. Field shapes are the same as every
-- other test in the suite.
emptyTx :: ByteString -> GenericTx
emptyTx hash = GenericTx
  { txHash             = hash
  , txBlockIndex       = 0
  , txSize             = 200
  , txFee              = 0
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
  }

-- | Pad a short ByteString to 32 bytes — the canonical hash length
-- the parser hands the rest of the pipeline.
padHash32 :: ByteString -> ByteString
padHash32 bs = bs <> BS.replicate (max 0 (32 - BS.length bs)) 0

-- | The producing tx. Three outputs at known values; the consumer
-- and the phase-2 failure both reference its hash.
producerHash :: ByteString
producerHash = padHash32 "PROD"

producerTx :: GenericTx
producerTx = (emptyTx producerHash)
  { txBlockIndex = 0
  , txSize       = 100
  , txFee        = 170000
  , txOutSum     = 12000000
  , txOutputs    = [mkOut 0 5000000, mkOut 1 5000000, mkOut 2 2000000]
  }

-- | Block carrying just the producer.
producerBlock :: GenericBlock
producerBlock = GenericBlock
  { blkEra           = Shelley
  , blkHash          = padHash32 "BLK1"
  , blkPreviousHash  = ""
  , blkSlotNo        = SlotNo 100
  , blkBlockNo       = BlockNo 1
  , blkEpochNo       = EpochNo 0
  , blkEpochSlotNo   = 100
  , blkSize          = 512
  , blkTime          = sampleTime
  , blkSlotLeader    = BS.replicate 28 0xab
  , blkProtoMajor    = 9
  , blkProtoMinor    = 0
  , blkVrfKey        = Just "vrf_vk1test"
  , blkOpCert        = Just (BS.replicate 32 0)
  , blkOpCertCounter = Just 0
  , blkIsEBB         = False
  , blkTxs           = [producerTx]
  }

-- | The valid-contract consumer. Spends @(producerHash, 0)@ for a
-- 5 000 000 input; one output for 4 500 000; fee 200 000. The
-- ledger-disabled deposit fallback should compute @300_000@.
consumerTx :: GenericTx
consumerTx = (emptyTx (padHash32 "VALID"))
  { txBlockIndex = 0
  , txSize       = 200
  , txFee        = 200000
  , txOutSum     = 4500000
  , txInputs     = [GenericTxIn producerHash 0]
  , txOutputs    = [mkOut 0 4500000]
  }

-- | The phase-2 failure. Mirrors what the parser writes after
-- Slice 6: @txFee = 0@ sentinel, no inputs/outputs/withdrawals,
-- just the collateral input and return.
phase2Tx :: GenericTx
phase2Tx = (emptyTx (padHash32 "FAIL"))
  { txBlockIndex       = 1
  , txSize             = 300
  , txValidContract    = False
  , txCollateralInputs = [GenericTxIn producerHash 1]
  , txCollateralOutput = Just (mkOut 0 2000000)
  }

-- | Block carrying the consumer and the phase-2 failure.
spendingBlock :: GenericBlock
spendingBlock = producerBlock
  { blkHash         = padHash32 "BLK2"
  , blkPreviousHash = blkHash producerBlock
  , blkSlotNo       = SlotNo 120
  , blkBlockNo      = BlockNo 2
  , blkEpochSlotNo  = 120
  , blkTxs          = [consumerTx, phase2Tx]
  }
