{-# LANGUAGE OverloadedStrings #-}

-- | Scenario tests for 'DbSync.Phase.Following.Run'.
--
-- Hand-crafted 'GenericBlock' fixtures are pushed through the real
-- resolver + writer + extractors against PG. Mirrors the shape of
-- 'DbSync.Db.LoaderSpec' so the same fixtures exercise both phases.
module DbSync.Phase.Following.RunSpec (spec) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import qualified DbSync.Parser.Metadata as Metadata

import Test.Hspec (Spec, afterAll_, beforeAll_, before_, describe, it, shouldBe)

import DbSync.Parser.Types
  ( BlockEra (..)
  , CertAction (..)
  , CredHash (..)
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  , GenericTxIn (..)
  , GenericTxOut (..)
  , GenericTxWithdrawal (..)
  , PoolRegistrationData (..)
  )
import DbSync.Db.Schema.Address (addressTableDef)
import DbSync.Util.Bech32 (serialiseShelleyAddrToBech32)
import DbSync.Db.Schema.CBOR (txCborTableDef)
import DbSync.Db.Schema.Core (blockTableDef, poolHashTableDef, slotLeaderTableDef, stakeAddressTableDef, txTableDef)
import DbSync.Db.Schema.Metadata (txMetadataTableDef)
import DbSync.Db.Schema.MultiAsset
  ( maTxMintTableDef
  , maTxOutTableDef
  , multiAssetTableDef
  )
import DbSync.Db.Schema.Pool
  ( poolMetadataRefTableDef
  , poolOwnerTableDef
  , poolRelayTableDef
  , poolRetireTableDef
  , poolUpdateTableDef
  )
import DbSync.Db.Schema.StakeDelegation
  ( delegationTableDef
  , stakeDeregistrationTableDef
  , stakeRegistrationTableDef
  , withdrawalTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO
  ( collateralTxInTableDef
  , referenceTxInTableDef
  , txInTableDef
  , txOutTableDef
  )
import DbSync.Extractor (ExtractorDef)
import DbSync.Extractor.Cbor (cborExtractor)
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.Metadata (metadataExtractor)
import DbSync.Extractor.MultiAsset (multiAssetExtractor)
import DbSync.Extractor.Pool (poolExtractor)
import DbSync.Extractor.StakeDelegation (stakeDelegationExtractor)
import DbSync.Extractor.UTxO (utxoExtractor)
import DbSync.Extractor.Pipeline (processBlock)
import DbSync.Phase.Type (SyncPhase (..))
import DbSync.Phase.Following.Resolver (ConsumedTracking (..), mkFollowResolver)
import DbSync.Test.Database
  ( queryTestDb
  , setupFollowTipSchema
  , teardownSchema
  , truncateAllTables
  )
import DbSync.Test.Hasql (withTestConnection)
import DbSync.Test.PipelineEnv (mkTestPipelineEnvWith)
import DbSync.Extractor (emptyBlockLedgerData)
import qualified DbSync.Phase.Following.Writer as FollowingWriter

tables :: [TableDef]
tables =
  [ blockTableDef
  , txTableDef
  , slotLeaderTableDef
  , addressTableDef
  , txOutTableDef
  , txInTableDef
  , collateralTxInTableDef
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

-- | Truncate order: dependent rows first, parent rows last. Hand-ordered
-- so 'truncateAllTables' (which uses RESTART IDENTITY CASCADE) is safe
-- against FK violations even without the CASCADE clause.
tableNames :: [Text]
tableNames = map tdName
  [ txOutTableDef
  , addressTableDef
  , txInTableDef
  , collateralTxInTableDef
  , referenceTxInTableDef
  , txMetadataTableDef
  , maTxMintTableDef
  , maTxOutTableDef
  , multiAssetTableDef
  , stakeRegistrationTableDef
  , stakeDeregistrationTableDef
  , delegationTableDef
  , withdrawalTableDef
  , poolOwnerTableDef
  , poolRelayTableDef
  , poolRetireTableDef
  , poolMetadataRefTableDef
  , poolUpdateTableDef
  , stakeAddressTableDef
  , poolHashTableDef
  , txCborTableDef
  , txTableDef
  , blockTableDef
  , slotLeaderTableDef
  ]

spec :: Spec
spec = describe "DbSync.Phase.Following.Run" $
  beforeAll_ (setupFollowTipSchema tables) $
  afterAll_  (teardownSchema tables) $
  before_    (truncateAllTables tableNames) $ do

    describe "single empty block" $
      it "writes 1 block + 1 slot_leader" $ do
        runFollow [emptyBlock]
        blockCount <- countOf blockTableDef
        slCount    <- countOf slotLeaderTableDef
        blockCount `shouldBe` "1"
        slCount    `shouldBe` "1"

    describe "two empty blocks, same leader" $ do
      it "produces 2 blocks and 1 deduped slot_leader" $ do
        runFollow [emptyBlock, emptyBlock2]
        blockCount <- countOf blockTableDef
        slCount    <- countOf slotLeaderTableDef
        blockCount `shouldBe` "2"
        slCount    `shouldBe` "1"

      it "links previous_id correctly" $ do
        runFollow [emptyBlock, emptyBlock2]
        result <- T.strip <$> queryTestDb
          ("SELECT id, previous_id FROM " <> tdName blockTableDef <> " ORDER BY id;")
        let rows = T.lines result
        rows `shouldBe` ["1|", "2|1"]

    describe "block with one tx" $ do
      it "writes 1 block and 1 tx" $ do
        runFollow [blockWith1Tx]
        blockCount <- countOf blockTableDef
        txCount    <- countOf txTableDef
        blockCount `shouldBe` "1"
        txCount    `shouldBe` "1"

      it "tx.block_id references the inserted block" $ do
        runFollow [blockWith1Tx]
        result <- T.strip <$> queryTestDb
          ("SELECT block_id FROM " <> tdName txTableDef <> ";")
        result `shouldBe` "1"

    describe "block with one tx and one output" $ do
      it "writes 1 tx_out row" $ do
        runFollow [blockWith1Out]
        n <- countOf txOutTableDef
        n `shouldBe` "1"

      it "tx_out.tx_id references the new tx" $ do
        runFollow [blockWith1Out]
        result <- T.strip <$> queryTestDb
          ("SELECT tx_id FROM " <> tdName txOutTableDef <> ";")
        result `shouldBe` "1"

      it "tx_out.index, value, address round-trip" $ do
        runFollow [blockWith1Out]
        result <- T.strip <$> queryTestDb
          ( "SELECT " <> tdName txOutTableDef <> ".index, "
              <> tdName txOutTableDef <> ".value, "
              <> tdName addressTableDef <> ".address"
              <> " FROM " <> tdName txOutTableDef
              <> " JOIN " <> tdName addressTableDef
              <> " ON " <> tdName addressTableDef <> ".id = "
              <> tdName txOutTableDef <> ".address_id;"
          )
        result `shouldBe` ("0|5000000|" <> serialiseShelleyAddrToBech32 sampleAddrRaw)

    describe "block with one tx and two outputs" $ do
      it "writes both tx_outs in order" $ do
        runFollow [blockWith2Outs]
        result <- T.strip <$> queryTestDb
          ("SELECT id, tx_id, index FROM " <> tdName txOutTableDef <> " ORDER BY id;")
        let rows = T.lines result
        rows `shouldBe` ["1|1|0", "2|1|1"]

    describe "block with one tx and all input kinds" $ do
      it "writes 1 tx_in, 1 collateral_tx_in, 1 reference_tx_in" $ do
        runFollow [blockWithAllInputs]
        txInN <- countOf txInTableDef
        colN  <- countOf collateralTxInTableDef
        refN  <- countOf referenceTxInTableDef
        txInN `shouldBe` "1"
        colN  `shouldBe` "1"
        refN  `shouldBe` "1"

      it "tx_in.tx_in_id references the spending tx, tx_out_id is NULL" $ do
        runFollow [blockWithAllInputs]
        result <- T.strip <$> queryTestDb
          ("SELECT tx_in_id, tx_out_id, tx_out_index FROM " <> tdName txInTableDef <> ";")
        -- '|' between empty fields renders as "1||0" for NULL tx_out_id
        result `shouldBe` "1||0"

      it "tx_in.tx_out_hash carries the referenced tx hash" $ do
        runFollow [blockWithAllInputs]
        result <- T.strip <$> queryTestDb
          ("SELECT encode(tx_out_hash, 'hex') FROM " <> tdName txInTableDef <> ";")
        -- "spent_tx_hash_in" (16 ASCII bytes) padded to 32 bytes with NULs.
        result `shouldBe`
          "7370656e745f74785f686173685f696e00000000000000000000000000000000"

    describe "block with one tx and metadata" $ do
      it "writes 1 tx_metadata row per metadata key" $ do
        runFollow [blockWithMetadata]
        n <- countOf txMetadataTableDef
        n `shouldBe` "1"

      it "stores key, no-schema JSON, single-key CBOR, and tx_id" $ do
        runFollow [blockWithMetadata]
        result <- T.strip <$> queryTestDb
          ( "SELECT key, json, encode(bytes, 'hex'), tx_id FROM "
              <> tdName txMetadataTableDef <> ";"
          )
        -- key=42, json="\"hello\"", bytes=cbor({42: "hello"}), tx_id=1.
        -- CBOR breakdown: 0xa1 = 1-entry map, 0x18 0x2a = uint 42,
        -- 0x65 + "hello" bytes = 5-byte text string.
        result `shouldBe` "42|\"hello\"|a1182a6568656c6c6f|1"

    describe "block minting one multi-asset" $ do
      it "writes 1 multi_asset and 1 ma_tx_mint row" $ do
        runFollow [blockWithMint]
        maN  <- countOf multiAssetTableDef
        mtmN <- countOf maTxMintTableDef
        maN  `shouldBe` "1"
        mtmN `shouldBe` "1"

      it "multi_asset.policy and name round-trip via hex" $ do
        runFollow [blockWithMint]
        result <- T.strip <$> queryTestDb
          ( "SELECT encode(policy, 'hex'), encode(name, 'hex') FROM "
              <> tdName multiAssetTableDef <> ";"
          )
        -- "policy01" + 20 nulls = 28 bytes; "tokenA" raw = 6 bytes
        result `shouldBe` "706f6c69637930310000000000000000000000000000000000000000|746f6b656e41"

      it "ma_tx_mint.quantity carries a signed Integer (positive case)" $ do
        runFollow [blockWithMint]
        result <- T.strip <$> queryTestDb
          ("SELECT quantity, tx_id, ident FROM " <> tdName maTxMintTableDef <> ";")
        result `shouldBe` "1000|1|1"

    describe "two transactions minting the same asset (dedup)" $
      it "produces 1 multi_asset and 2 ma_tx_mint rows" $ do
        runFollow [blockWithTwoMintsOfSameAsset]
        maN  <- countOf multiAssetTableDef
        mtmN <- countOf maTxMintTableDef
        maN  `shouldBe` "1"
        mtmN `shouldBe` "2"

    describe "block with a multi-asset tx output" $
      it "writes 1 multi_asset and 1 ma_tx_out row referencing the tx_out" $ do
        runFollow [blockWithMaOut]
        maN <- countOf multiAssetTableDef
        mao <- T.strip <$> queryTestDb
          ("SELECT quantity, tx_out_id, ident FROM " <> tdName maTxOutTableDef <> ";")
        maN `shouldBe` "1"
        mao `shouldBe` "500|1|1"

    describe "block with a stake registration cert" $ do
      it "writes 1 stake_address and 1 stake_registration row" $ do
        runFollow [blockWithStakeReg]
        saN <- countOf stakeAddressTableDef
        srN <- countOf stakeRegistrationTableDef
        saN `shouldBe` "1"
        srN `shouldBe` "1"

      it "stake_registration links addr_id, tx_id, epoch_no" $ do
        runFollow [blockWithStakeReg]
        result <- T.strip <$> queryTestDb
          ( "SELECT addr_id, cert_index, epoch_no, tx_id, deposit FROM "
              <> tdName stakeRegistrationTableDef <> ";"
          )
        -- addr_id=1, cert_index=0, epoch_no=5 (from emptyBlock), tx_id=1, deposit NULL
        result `shouldBe` "1|0|5|1|"

    describe "stake registration + deregistration of same address" $
      it "deduplicates the stake_address row" $ do
        runFollow [blockWithRegThenDereg]
        saN  <- countOf stakeAddressTableDef
        srN  <- countOf stakeRegistrationTableDef
        sdN  <- countOf stakeDeregistrationTableDef
        saN `shouldBe` "1"
        srN `shouldBe` "1"
        sdN `shouldBe` "1"

    describe "block with a withdrawal" $
      it "writes 1 stake_address and 1 withdrawal" $ do
        runFollow [blockWithWithdrawal]
        saN <- countOf stakeAddressTableDef
        wd  <- T.strip <$> queryTestDb
          ("SELECT addr_id, tx_id, amount FROM " <> tdName withdrawalTableDef <> ";")
        saN `shouldBe` "1"
        wd  `shouldBe` "1|1|7000000"

    describe "block with a minimal pool registration" $ do
      it "writes 1 pool_update, 1 stake_address, and dedupes pool_hash" $ do
        runFollow [blockWithPoolReg]
        phN <- countOf poolHashTableDef
        puN <- countOf poolUpdateTableDef
        saN <- countOf stakeAddressTableDef
        -- 1 pool_hash row: the registered pool. The unregistered slot
        -- leader is queried only, so it adds no pool_hash row.
        phN `shouldBe` "1"
        puN `shouldBe` "1"
        saN `shouldBe` "1"  -- the reward addr

      it "no pool_metadata_ref / pool_owner / pool_relay rows are written" $ do
        runFollow [blockWithPoolReg]
        pmrN <- countOf poolMetadataRefTableDef
        poN  <- countOf poolOwnerTableDef
        prN  <- countOf poolRelayTableDef
        pmrN `shouldBe` "0"
        poN  `shouldBe` "0"
        prN  `shouldBe` "0"

    describe "block with a pool registration carrying metadata" $
      it "writes 1 pool_metadata_ref linked to the pool" $ do
        runFollow [blockWithPoolRegMeta]
        pmr <- T.strip <$> queryTestDb
          ( "SELECT pool_id, url, encode(hash, 'hex') FROM "
              <> tdName poolMetadataRefTableDef <> ";"
          )
        -- pool_id = 1: the registered pool is the only pool_hash row;
        -- the unregistered slot leader adds none.
        pmr `shouldBe` "1|https://pool.example.com/meta.json|6d657461686173685f33325f62797465735f70616464645f5f5f5f5f5f5f5f5f"

    describe "block with a pool retirement cert" $
      it "writes 1 pool_retire row and dedupes pool_hash" $ do
        runFollow [blockWithPoolRetire]
        phN <- countOf poolHashTableDef
        prN <- countOf poolRetireTableDef
        -- 1 pool_hash row: the retired pool. The unregistered slot
        -- leader adds none.
        phN `shouldBe` "1"
        prN `shouldBe` "1"

    describe "block with a delegation cert (cross-extractor flow)" $
      it "writes 1 stake_address and 1 delegation row, dedupes pool_hash" $ do
        runFollow [blockWithDelegation]
        saN <- countOf stakeAddressTableDef
        phN <- countOf poolHashTableDef
        d   <- T.strip <$> queryTestDb
          ( "SELECT addr_id, pool_hash_id, active_epoch_no, tx_id FROM "
              <> tdName delegationTableDef <> ";"
          )
        saN `shouldBe` "1"
        -- 1 pool_hash row: the delegation target. The unregistered slot
        -- leader adds none.
        phN `shouldBe` "1"
        -- active_epoch_no = blkEpochNo (5) + 2 = 7; pool_hash_id = 1.
        d   `shouldBe` "1|1|7|1"

    describe "block with a tx carrying CBOR bytes" $ do
      it "writes 1 tx_cbor row when txCborRaw is set" $ do
        runFollow [blockWithCbor]
        n <- countOf txCborTableDef
        n `shouldBe` "1"

      it "tx_cbor.tx_id and bytes round-trip" $ do
        runFollow [blockWithCbor]
        result <- T.strip <$> queryTestDb
          ( "SELECT tx_id, encode(bytes, 'hex') FROM "
              <> tdName txCborTableDef <> ";"
          )
        -- "tx-cbor-payload" = 15 ASCII bytes
        result `shouldBe` "1|74782d63626f722d7061796c6f6164"

      it "no tx_cbor row when txCborRaw is Nothing (Byron-shape txs)" $ do
        runFollow [blockWith1Tx]
        n <- countOf txCborTableDef
        n `shouldBe` "0"

    describe "block with a treasury donation" $
      it "tx.treasury_donation round-trips" $ do
        runFollow [blockWithDonation]
        result <- T.strip <$> queryTestDb
          ("SELECT treasury_donation FROM " <> tdName txTableDef <> ";")
        result `shouldBe` "250000000"

    describe "four stake reg/dereg txs in one block" $
      it "writes 2 stake_registration, 2 stake_deregistration, 2 stake_address rows" $ do
        runFollow [blockWithFourRegistrations]
        srN <- countOf stakeRegistrationTableDef
        sdN <- countOf stakeDeregistrationTableDef
        saN <- countOf stakeAddressTableDef
        srN `shouldBe` "2"
        sdN `shouldBe` "2"
        saN `shouldBe` "2"

    describe "multiple reg/dereg certs in one tx" $
      it "writes 2 stake_registration, 1 stake_deregistration, 2 stake_address rows" $ do
        runFollow [blockWithMultiRegCerts]
        srN <- countOf stakeRegistrationTableDef
        sdN <- countOf stakeDeregistrationTableDef
        saN <- countOf stakeAddressTableDef
        srN `shouldBe` "2"
        sdN `shouldBe` "1"
        saN `shouldBe` "2"

    describe "tx_out with a pointer-style address" $
      it "writes a tx_out with NULL stake_address_id and no stake_address row" $ do
        runFollow [blockWithPointerOut]
        n  <- countOf stakeAddressTableDef
        sid <- T.strip <$> queryTestDb
          ( "SELECT coalesce(stake_address_id::text, 'NULL') FROM "
              <> tdName txOutTableDef <> ";"
          )
        n   `shouldBe` "0"
        sid `shouldBe` "NULL"

    describe "metadata extractor disabled" $
      it "writes no tx_metadata rows for a block carrying aux data" $ do
        runFollowWith extractorsNoMetadata [blockWithMetadata]
        n <- countOf txMetadataTableDef
        n `shouldBe` "0"

    describe "two payment txs chained in one block" $ do
      it "writes 2 txs, 1 tx_in, and 2 tx_outs" $ do
        runFollow [blockWithChainedTxs]
        txN  <- countOf txTableDef
        inN  <- countOf txInTableDef
        outN <- countOf txOutTableDef
        txN  `shouldBe` "2"
        inN  `shouldBe` "1"
        outN `shouldBe` "2"

      it "tx_in.tx_out_id resolves to tx1's output within the same block" $ do
        runFollow [blockWithChainedTxs]
        result <- T.strip <$> queryTestDb
          ( "SELECT tx_in_id, tx_out_id, tx_out_index FROM "
              <> tdName txInTableDef <> ";"
          )
        -- tx_in_id = tx2 (id=2); tx_out_id = tx1's first output (id=1); index = 0
        result `shouldBe` "2|1|0"

    describe "per-era block ingestion" $ do
      it "Byron block lands with proto_major 1" $ do
        runFollow [byronEmptyBlock]
        result <- T.strip <$> queryTestDb
          ("SELECT proto_major FROM " <> tdName blockTableDef <> ";")
        result `shouldBe` "1"

      it "Shelley block lands with proto_major 2" $ do
        runFollow [shelleyEmptyBlock]
        result <- T.strip <$> queryTestDb
          ("SELECT proto_major FROM " <> tdName blockTableDef <> ";")
        result `shouldBe` "2"

      it "Allegra block lands with proto_major 3" $ do
        runFollow [allegraEmptyBlock]
        result <- T.strip <$> queryTestDb
          ("SELECT proto_major FROM " <> tdName blockTableDef <> ";")
        result `shouldBe` "3"

      it "Mary block lands with proto_major 4" $ do
        runFollow [maryEmptyBlock]
        result <- T.strip <$> queryTestDb
          ("SELECT proto_major FROM " <> tdName blockTableDef <> ";")
        result `shouldBe` "4"

      it "Alonzo block lands with proto_major 6" $ do
        runFollow [alonzoEmptyBlock]
        result <- T.strip <$> queryTestDb
          ("SELECT proto_major FROM " <> tdName blockTableDef <> ";")
        result `shouldBe` "6"

      it "Babbage block lands with proto_major 8" $ do
        runFollow [babbageEmptyBlock]
        result <- T.strip <$> queryTestDb
          ("SELECT proto_major FROM " <> tdName blockTableDef <> ";")
        result `shouldBe` "8"

      it "Alonzo phase-2 failure marks tx valid_contract=false" $ do
        runFollow [alonzoPhase2FailBlock]
        result <- T.strip <$> queryTestDb
          ("SELECT valid_contract FROM " <> tdName txTableDef <> ";")
        result `shouldBe` "f"

      it "Allegra tx_invalid_before/hereafter columns round-trip" $ do
        runFollow [allegraTxWithBounds]
        result <- T.strip <$> queryTestDb
          ( "SELECT invalid_before, invalid_hereafter FROM "
              <> tdName txTableDef <> ";"
          )
        result `shouldBe` "100|200"

-- | Bare row-count via @psql@. Returns the count as 'Text' so callers
-- compare directly against the numeric literals they already use.
countOf :: TableDef -> IO Text
countOf td = T.strip <$>
  queryTestDb ("SELECT count(*) FROM " <> tdName td <> ";")

-- ---------------------------------------------------------------------------
-- Runner
-- ---------------------------------------------------------------------------

runFollowWith :: [ExtractorDef] -> [GenericBlock] -> IO ()
runFollowWith ex blocks =
  withTestConnection $ \conn -> do
    resolver <- mkFollowResolver conn TrackConsumedBy
    let writer = FollowingWriter.mkWriter conn
        env    =
          mkTestPipelineEnvWith
            Mainnet
            resolver
            writer
            ex
            (\_ -> pure emptyBlockLedgerData)
            FollowingChainTip
    for_ blocks $ \blk -> runReaderT (processBlock blk) env

runFollow :: [GenericBlock] -> IO ()
runFollow = runFollowWith extractors

-- | Extractor list without metadata, for the metadata-disabled spec.
extractorsNoMetadata :: [ExtractorDef]
extractorsNoMetadata =
  [ coreExtractor
  , utxoExtractor
  , multiAssetExtractor
  , stakeDelegationExtractor
  , poolExtractor
  , cborExtractor
  ]

-- ---------------------------------------------------------------------------
-- Fixtures (same shape as Db.LoaderSpec)
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

byronEmptyBlock :: GenericBlock
byronEmptyBlock = emptyBlock { blkEra = Byron, blkProtoMajor = 1 }

shelleyEmptyBlock :: GenericBlock
shelleyEmptyBlock = emptyBlock { blkEra = Shelley, blkProtoMajor = 2 }

allegraEmptyBlock :: GenericBlock
allegraEmptyBlock = emptyBlock { blkEra = Allegra, blkProtoMajor = 3 }

maryEmptyBlock :: GenericBlock
maryEmptyBlock = emptyBlock { blkEra = Mary, blkProtoMajor = 4 }

alonzoEmptyBlock :: GenericBlock
alonzoEmptyBlock = emptyBlock { blkEra = Alonzo, blkProtoMajor = 6 }

babbageEmptyBlock :: GenericBlock
babbageEmptyBlock = emptyBlock { blkEra = Babbage, blkProtoMajor = 8 }

alonzoPhase2FailBlock :: GenericBlock
alonzoPhase2FailBlock = alonzoEmptyBlock
  { blkTxs = [sampleTx { txValidContract = False }]
  }

allegraTxWithBounds :: GenericBlock
allegraTxWithBounds = allegraEmptyBlock
  { blkTxs =
      [ sampleTx
          { txInvalidBefore    = Just 100
          , txInvalidHereafter = Just 200
          }
      ]
  }

blockWith1Tx :: GenericBlock
blockWith1Tx = emptyBlock { blkTxs = [sampleTx] }

sampleTx :: GenericTx
sampleTx = GenericTx
  { txHash             = BS.replicate 32 0xaa
  , txBlockIndex       = 0
  , txSize             = 300
  , txFee              = 174000
  , txOutSum           = 5000000
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

-- | A non-Byron Shelley-shaped raw address. Header byte 0x00 (BasePaymentKey
-- StakeKey) — payment cred is the next 28 bytes. Total 57 bytes.
sampleAddrRaw :: ByteString
sampleAddrRaw = BS.pack (0x00 : replicate 56 0x11)

sampleOut :: Word16 -> Word64 -> GenericTxOut
sampleOut idx value = GenericTxOut
  { txOutIndex       = idx
  , txOutAddressRaw  = sampleAddrRaw
  , txOutValue       = value
  , txOutDataHash    = Nothing
  , txOutInlineDatum = Nothing
  , txOutRefScript   = Nothing
  , txOutMultiAssets = []
  }

blockWith1Out :: GenericBlock
blockWith1Out = emptyBlock
  { blkTxs = [sampleTx { txOutputs = [sampleOut 0 5000000] }]
  }

blockWith2Outs :: GenericBlock
blockWith2Outs = emptyBlock
  { blkTxs = [sampleTx { txOutputs = [sampleOut 0 1000000, sampleOut 1 2000000] }]
  }

-- | Pad a short ByteString to 32 bytes with trailing 0x00 so the hash
-- is the right size for the @tx_out_hash@ column.
padHash32 :: ByteString -> ByteString
padHash32 bs = bs <> BS.replicate (max 0 (32 - BS.length bs)) 0

blockWithAllInputs :: GenericBlock
blockWithAllInputs = emptyBlock
  { blkTxs =
      [ sampleTx
          { txInputs           = [GenericTxIn (padHash32 "spent_tx_hash_in")  0]
          , txCollateralInputs = [GenericTxIn (padHash32 "spent_tx_hash_col") 1]
          , txReferenceInputs  = [GenericTxIn (padHash32 "spent_tx_hash_ref") 2]
          }
      ]
  }

blockWithMetadata :: GenericBlock
blockWithMetadata = emptyBlock
  { blkTxs = [sampleTx { txMetadata = Just (Map.singleton 42 (Metadata.S "hello")) }]
  }

-- | A 28-byte policy ID (raw bytes; padded with NULs).
samplePolicy :: ByteString
samplePolicy = "policy01" <> BS.replicate (28 - 8) 0

sampleAssetName :: ByteString
sampleAssetName = "tokenA"

blockWithMint :: GenericBlock
blockWithMint = emptyBlock
  { blkTxs = [sampleTx { txMint = [(samplePolicy, sampleAssetName, 1000)] }]
  }

-- | Two transactions in one block both mint the same asset. The second
-- mint must reuse the @multi_asset.id@ created by the first.
blockWithTwoMintsOfSameAsset :: GenericBlock
blockWithTwoMintsOfSameAsset = emptyBlock
  { blkTxs =
      [ sampleTx { txMint = [(samplePolicy, sampleAssetName, 1000)] }
      , sampleTx
          { txHash = BS.replicate 32 0xbb
          , txMint = [(samplePolicy, sampleAssetName, 500)]
          }
      ]
  }

-- | A tx whose single output carries a multi-asset value.
blockWithMaOut :: GenericBlock
blockWithMaOut = emptyBlock
  { blkTxs =
      [ sampleTx
          { txOutputs =
              [ (sampleOut 0 5000000)
                  { txOutMultiAssets = [(samplePolicy, sampleAssetName, 500)]
                  }
              ]
          }
      ]
  }

-- | A 28-byte stake credential hash.
sampleStakeCred :: ByteString
sampleStakeCred = "stake_cred_28b" <> BS.replicate (28 - 14) 0

stakeRegCert :: GenericTxCertificate
stakeRegCert = GenericTxCertificate
  { txCertIndex  = 0
  , txCertAction = CertStakeRegistration (CredHash sampleStakeCred False) Nothing
  }

stakeDeregCert :: GenericTxCertificate
stakeDeregCert = GenericTxCertificate
  { txCertIndex  = 1
  , txCertAction = CertStakeDeregistration (CredHash sampleStakeCred False)
  }

blockWithStakeReg :: GenericBlock
blockWithStakeReg = emptyBlock
  { blkTxs = [sampleTx { txCertificates = [stakeRegCert] }]
  }

blockWithRegThenDereg :: GenericBlock
blockWithRegThenDereg = emptyBlock
  { blkTxs = [sampleTx { txCertificates = [stakeRegCert, stakeDeregCert] }]
  }

-- | A tx with a single withdrawal. The reward address is 29 bytes:
-- 1-byte header + 28-byte credential hash. The extractor strips the
-- header and stores the 28-byte credential as the dedup key.
blockWithWithdrawal :: GenericBlock
blockWithWithdrawal = emptyBlock
  { blkTxs =
      [ sampleTx
          { txWithdrawals =
              [ GenericTxWithdrawal
                  { txwRewardAddress = BS.cons 0xe0 sampleStakeCred
                  , txwAmount        = 7000000
                  }
              ]
          }
      ]
  }

-- | A 28-byte pool key hash.
samplePoolKey :: ByteString
samplePoolKey = "pool_key_28b" <> BS.replicate (28 - 12) 0

-- | The reward address: 1-byte header + 28-byte credential hash.
sampleRewardAddr :: ByteString
sampleRewardAddr = BS.cons 0xe1 sampleStakeCred

-- | A minimal pool registration with no metadata, no owners, no relays.
prdMinimal :: PoolRegistrationData
prdMinimal = PoolRegistrationData
  { prdPoolHash   = samplePoolKey
  , prdVrfKeyHash = BS.replicate 32 0xcc
  , prdPledge     = 1000000
  , prdCost       = 340000000
  , prdMargin     = 0.05
  , prdRewardAddr = sampleRewardAddr
  , prdOwners     = []
  , prdRelays     = []
  , prdMetadata   = Nothing
  }

poolRegCert :: GenericTxCertificate
poolRegCert = GenericTxCertificate
  { txCertIndex  = 0
  , txCertAction = CertPoolRegistration prdMinimal
  }

blockWithPoolReg :: GenericBlock
blockWithPoolReg = emptyBlock
  { blkTxs = [sampleTx { txCertificates = [poolRegCert] }]
  }

-- | Pool registration with metadata. Hash is exactly 32 bytes so its
-- hex form is deterministic.
poolMetaHash :: ByteString
poolMetaHash = "metahash_32_bytes_paddd_________"  -- 32 chars

prdWithMeta :: PoolRegistrationData
prdWithMeta = prdMinimal
  { prdMetadata = Just ("https://pool.example.com/meta.json", poolMetaHash)
  }

blockWithPoolRegMeta :: GenericBlock
blockWithPoolRegMeta = emptyBlock
  { blkTxs =
      [ sampleTx
          { txCertificates =
              [ poolRegCert { txCertAction = CertPoolRegistration prdWithMeta }
              ]
          }
      ]
  }

poolRetireCert :: GenericTxCertificate
poolRetireCert = GenericTxCertificate
  { txCertIndex  = 0
  , txCertAction = CertPoolRetirement samplePoolKey 99
  }

blockWithPoolRetire :: GenericBlock
blockWithPoolRetire = emptyBlock
  { blkTxs = [sampleTx { txCertificates = [poolRetireCert] }]
  }

-- | Delegation cert: stake credential delegates to a pool key hash.
-- Triggers the cross-extractor 'pool_hash' write because the pool hasn't
-- been registered separately.
delegationCert :: GenericTxCertificate
delegationCert = GenericTxCertificate
  { txCertIndex  = 0
  , txCertAction = CertDelegation (CredHash sampleStakeCred False) samplePoolKey
  }

blockWithDelegation :: GenericBlock
blockWithDelegation = emptyBlock
  { blkTxs = [sampleTx { txCertificates = [delegationCert] }]
  }

-- | A tx that carries raw CBOR bytes (Shelley+ in real life). The
-- extractor only writes a row when 'txCborRaw' is 'Just', so this
-- exercises the positive path.
blockWithCbor :: GenericBlock
blockWithCbor = emptyBlock
  { blkTxs = [sampleTx { txCborRaw = Just "tx-cbor-payload" }]
  }

-- | A tx with a non-zero treasury donation.
blockWithDonation :: GenericBlock
blockWithDonation = emptyBlock
  { blkTxs = [sampleTx { txTreasuryDonation = 250000000 }]
  }

-- | A second 28-byte stake credential, distinct from 'sampleStakeCred'.
sampleStakeCredB :: ByteString
sampleStakeCredB = "stake_cred_28b_B" <> BS.replicate (28 - 16) 0

-- | Four reg/dereg txs in one block: reg A, dereg A, reg B, dereg B.
blockWithFourRegistrations :: GenericBlock
blockWithFourRegistrations = emptyBlock
  { blkTxs =
      [ sampleTx
          { txHash = BS.replicate 32 0xaa
          , txCertificates =
              [ GenericTxCertificate 0 (CertStakeRegistration (CredHash sampleStakeCred False) Nothing) ]
          }
      , sampleTx
          { txHash = BS.replicate 32 0xab
          , txCertificates =
              [ GenericTxCertificate 0 (CertStakeDeregistration (CredHash sampleStakeCred False)) ]
          }
      , sampleTx
          { txHash = BS.replicate 32 0xac
          , txCertificates =
              [ GenericTxCertificate 0 (CertStakeRegistration (CredHash sampleStakeCredB False) Nothing) ]
          }
      , sampleTx
          { txHash = BS.replicate 32 0xad
          , txCertificates =
              [ GenericTxCertificate 0 (CertStakeDeregistration (CredHash sampleStakeCredB False)) ]
          }
      ]
  }

-- | One tx carrying three reg/dereg certs: reg A, dereg A, reg B.
blockWithMultiRegCerts :: GenericBlock
blockWithMultiRegCerts = emptyBlock
  { blkTxs =
      [ sampleTx
          { txCertificates =
              [ GenericTxCertificate 0 (CertStakeRegistration (CredHash sampleStakeCred False)  Nothing)
              , GenericTxCertificate 1 (CertStakeDeregistration (CredHash sampleStakeCred False))
              , GenericTxCertificate 2 (CertStakeRegistration (CredHash sampleStakeCredB False) Nothing)
              ]
          }
      ]
  }

-- | Pointer-style raw address. Header 0x40 + 28-byte payment cred + a
-- minimal 3-byte pointer triple. 'extractStakeCred' returns 'Nothing'
-- for any non-base header, so the stake_address pipeline writes no row.
samplePointerAddrRaw :: ByteString
samplePointerAddrRaw =
  BS.pack [0x40]
    <> BS.replicate 28 0x22
    <> BS.pack [0x00, 0x00, 0x00]

blockWithPointerOut :: GenericBlock
blockWithPointerOut = emptyBlock
  { blkTxs =
      [ sampleTx
          { txOutputs =
              [ (sampleOut 0 5000000) { txOutAddressRaw = samplePointerAddrRaw } ]
          }
      ]
  }

-- | Two payment txs chained in one block: tx1 produces an output,
-- tx2 spends it. Exercises in-block tx_out resolution.
blockWithChainedTxs :: GenericBlock
blockWithChainedTxs = emptyBlock
  { blkTxs =
      [ sampleTx
          { txHash    = BS.replicate 32 0xaa
          , txOutputs = [sampleOut 0 4000000]
          }
      , sampleTx
          { txHash       = BS.replicate 32 0xbb
          , txBlockIndex = 1
          , txInputs     = [GenericTxIn (BS.replicate 32 0xaa) 0]
          , txOutputs    = [sampleOut 0 3500000]
          }
      ]
  }
