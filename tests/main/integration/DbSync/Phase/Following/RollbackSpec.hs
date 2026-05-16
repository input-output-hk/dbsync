{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the 'FollowingChainTip' rollback cascade.
--
-- Two kinds of tests:
--
--   * /Schema-walk/ — pure assertions that 'tdForeignKeys' declares
--     the right children for the three rollback parents (tx, tx_out,
--     pool_update). Catches FK-metadata drift compile-time-fast.
--   * /Cascade integration/ — populate a real DB, call
--     'Rollback.rollbackToPoint', assert per-table row counts.
module DbSync.Phase.Following.RollbackSpec
  ( spec
  , schemaWalkSpec
  , cascadeSpec
  ) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import qualified DbSync.Block.Metadata as Metadata

import Test.Hspec (Spec, afterAll_, beforeAll_, before_, describe, it, shouldBe, shouldMatchList)

import DbSync.Block.Types
  ( BlockEra (..)
  , CardanoPoint
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxOut (..)
  )
import qualified DbSync.Db.Schema.CBOR as CBOR
import qualified DbSync.Db.Schema.Core as Core
import qualified DbSync.Db.Schema.Metadata as MetadataSchema
import qualified DbSync.Db.Schema.MultiAsset as MultiAsset
import qualified DbSync.Db.Schema.Pool as Pool
import qualified DbSync.Db.Schema.StakeDelegation as StakeDel
import DbSync.Db.Schema.Address (addressTableDef)
import DbSync.Db.Schema.CBOR (txCborTableDef)
import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef, txTableDef)
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
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO
  ( collateralTxInTableDef
  , collateralTxOutTableDef
  , referenceTxInTableDef
  , txInTableDef
  , txOutTableDef
  )
import DbSync.Extractor (ExtractorDef, emptyBlockLedgerData)
import DbSync.Extractor.Cbor (cborExtractor)
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.Metadata (metadataExtractor)
import DbSync.Extractor.MultiAsset (multiAssetExtractor)
import DbSync.Extractor.Pool (poolExtractor)
import DbSync.Extractor.StakeDelegation (stakeDelegationExtractor)
import DbSync.Extractor.UTxO (utxoExtractor)
import DbSync.Ingest.Pipeline (processBlock)
import DbSync.Db.Phase (SyncPhase (..))
import qualified DbSync.Phase.Following.Rollback as Rollback
import DbSync.App.Boot (mkCardanoPoint)
import DbSync.Resolver.Follow (mkFollowResolver)
import DbSync.Test.Database
  ( queryTestDb
  , setupFollowTipSchema
  , teardownSchema
  , truncateAllTables
  )
import DbSync.Test.Hasql (withTestConnection)
import DbSync.Test.PipelineEnv (mkTestPipelineEnvWith)
import DbSync.Writer.InsertAdapter (mkInsertWriter)

-- ---------------------------------------------------------------------------
-- Tables and extractors mirror the FollowingSpec setup so the
-- schema-init is identical to the one the production code uses.
-- ---------------------------------------------------------------------------

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
  , collateralTxOutTableDef
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

extractorVersions :: [(Text, Int)]
extractorVersions =
  [ ("core", 1)
  , ("utxo", 1)
  , ("metadata", 1)
  , ("multi_asset", 1)
  , ("stake_delegation", 1)
  , ("pool", 1)
  , ("cbor", 1)
  ]

tableNames :: [Text]
tableNames = map tdName tables

-- ---------------------------------------------------------------------------
-- Spec entry point
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  schemaWalkSpec
  cascadeSpec

-- ---------------------------------------------------------------------------
-- Schema-walk: assert tdForeignKeys declarations
-- ---------------------------------------------------------------------------

-- | Pure tests. Verify that 'childrenOf' picks up exactly the tables
-- that legitimately FK to each rollback parent. New tables that
-- introduce a FK to one of these parents need to be added here too,
-- which is the point — the test refuses to drift silently.
schemaWalkSpec :: Spec
schemaWalkSpec = describe "Rollback.childrenOf" $ do
  it "lists every tx-keyed child in the schema" $
    Rollback.childrenOf tables (tdName Core.txTableDef)
      `shouldMatchList`
        [ (tdName txInTableDef,                        "tx_in_id")
        , (tdName collateralTxInTableDef,              "tx_in_id")
        , (tdName referenceTxInTableDef,               "tx_in_id")
        , (tdName txOutTableDef,                       "tx_id")
        , (tdName collateralTxOutTableDef,             "tx_id")
        , (tdName MetadataSchema.txMetadataTableDef,   "tx_id")
        , (tdName MultiAsset.maTxMintTableDef,         "tx_id")
        , (tdName StakeDel.withdrawalTableDef,         "tx_id")
        , (tdName StakeDel.stakeRegistrationTableDef,  "tx_id")
        , (tdName StakeDel.stakeDeregistrationTableDef,"tx_id")
        , (tdName StakeDel.delegationTableDef,         "tx_id")
        , (tdName Pool.poolUpdateTableDef,             "registered_tx_id")
        , (tdName Pool.poolRetireTableDef,             "announced_tx_id")
        , (tdName Pool.poolMetadataRefTableDef,        "registered_tx_id")
        , (tdName CBOR.txCborTableDef,                 "tx_id")
        ]

  it "lists ma_tx_out as the only tx_out-keyed child" $
    Rollback.childrenOf tables (tdName txOutTableDef)
      `shouldMatchList`
        [ (tdName MultiAsset.maTxOutTableDef, "tx_out_id") ]

  it "lists pool_owner and pool_relay as pool_update-keyed children" $
    Rollback.childrenOf tables (tdName Pool.poolUpdateTableDef)
      `shouldMatchList`
        [ (tdName Pool.poolOwnerTableDef, "pool_update_id")
        , (tdName Pool.poolRelayTableDef, "update_id")
        ]

  it "returns empty for tables that nothing references in this schema" $ do
    -- 'multi_asset' is a dedup table — referenced by 'ma_tx_mint' /
    -- 'ma_tx_out' via the 'ident' column, but those are not declared
    -- as FKs in our schema and are deliberately left out of the
    -- rollback cascade.
    Rollback.childrenOf tables (tdName MultiAsset.multiAssetTableDef)
      `shouldBe` []

-- ---------------------------------------------------------------------------
-- Cascade integration: populate DB, roll back, assert row counts
-- ---------------------------------------------------------------------------

cascadeSpec :: Spec
cascadeSpec = describe "Rollback.rollbackToPoint" $
  beforeAll_ (setupFollowTipSchema tables extractorVersions) $
  afterAll_  (teardownSchema tables) $
  before_    (truncateAllTables tableNames) $ do

    it "deletes blocks strictly above the target and their dependent rows" $ do
      runFollow [block1WithTx, block2WithTxMeta, block3WithTx]

      -- Sanity: all three blocks were ingested.
      blockN <- T.strip <$> queryTestDb "SELECT count(*) FROM block;"
      blockN `shouldBe` "3"

      -- Roll back to the first block. block2 and block3 should be
      -- deleted, along with their txs, tx_outs, and metadata. The
      -- first block stays.
      withTestConnection $ \conn ->
        Rollback.rollbackToPoint tables conn target
      blockN' <- T.strip <$> queryTestDb "SELECT count(*) FROM block;"
      blockN' `shouldBe` "1"

      txN <- T.strip <$> queryTestDb "SELECT count(*) FROM tx;"
      txN `shouldBe` "1"

      txOutN <- T.strip <$> queryTestDb "SELECT count(*) FROM tx_out;"
      txOutN `shouldBe` "1"

      metaN <- T.strip <$> queryTestDb "SELECT count(*) FROM tx_metadata;"
      -- block2's metadata is gone.
      metaN `shouldBe` "0"

    it "leaves dedup tables (slot_leader, multi_asset, address) untouched" $ do
      runFollow [block1WithTx, block2WithTxMeta, block3WithTx]

      -- One unique address shared across all blocks ⇒ one address row.
      addrBefore <- T.strip <$> queryTestDb "SELECT count(*) FROM address;"
      slBefore   <- T.strip <$> queryTestDb "SELECT count(*) FROM slot_leader;"

      withTestConnection $ \conn ->
        Rollback.rollbackToPoint tables conn target

      addrAfter <- T.strip <$> queryTestDb "SELECT count(*) FROM address;"
      slAfter   <- T.strip <$> queryTestDb "SELECT count(*) FROM slot_leader;"
      addrAfter `shouldBe` addrBefore
      slAfter   `shouldBe` slBefore

    it "advances dbsync_sync_state.last_committed_slot to the target" $ do
      runFollow [block1WithTx, block2WithTxMeta, block3WithTx]
      -- Seed the sync-state row so the UPDATE has somewhere to land.
      _ <- queryTestDb
        "INSERT INTO dbsync_sync_state (schema_version_applied, ledger_enabled) \
        \VALUES (1, false) ON CONFLICT (id) DO NOTHING;"

      withTestConnection $ \conn ->
        Rollback.rollbackToPoint tables conn target

      result <- T.strip <$>
        queryTestDb
          "SELECT last_committed_slot, last_committed_block_no, \
          \       encode(last_committed_block_hash, 'hex') \
          \FROM dbsync_sync_state;"
      -- block1: slot 100, blockNo 1, hash 0x00..00 (32 bytes).
      result `shouldBe`
        "100|1|0000000000000000000000000000000000000000000000000000000000000000"

-- ---------------------------------------------------------------------------
-- Runner
-- ---------------------------------------------------------------------------

runFollow :: [GenericBlock] -> IO ()
runFollow blocks =
  withTestConnection $ \conn -> do
    resolver <- mkFollowResolver conn
    let writer = mkInsertWriter conn
        env    =
          mkTestPipelineEnvWith
            Mainnet
            resolver
            writer
            extractors
            (\_ -> pure emptyBlockLedgerData)
            FollowingChainTip
    for_ blocks $ \blk -> runReaderT (processBlock blk) env

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)

-- | A non-Byron Shelley-shaped raw address (header byte 0x00).
sampleAddrRaw :: ByteString
sampleAddrRaw = BS.pack (0x00 : replicate 56 0x11)

mkBlock
  :: ByteString    -- ^ this block's hash (32 bytes)
  -> ByteString    -- ^ previous block's hash
  -> Word64        -- ^ block_no
  -> Word64        -- ^ slot_no
  -> [GenericTx]
  -> GenericBlock
mkBlock h prev bn sn txs = GenericBlock
  { blkEra          = Shelley
  , blkHash         = h
  , blkPreviousHash = prev
  , blkSlotNo       = SlotNo sn
  , blkBlockNo      = BlockNo bn
  , blkEpochNo      = EpochNo 5
  , blkEpochSlotNo  = sn
  , blkSize         = 512
  , blkTime         = sampleTime
  , blkSlotLeader   = BS.replicate 28 0xab
  , blkProtoMajor   = 9
  , blkProtoMinor   = 0
  , blkVrfKey       = Just "vrf_vk1test"
  , blkOpCert       = Just (BS.replicate 32 0)
  , blkOpCertCounter = Just 0
  , blkIsEBB        = False
  , blkTxs          = txs
  }

sampleTx :: Word8 -> Word64 -> GenericTx
sampleTx tag value = GenericTx
  { txHash             = BS.replicate 32 tag
  , txBlockIndex       = 0
  , txSize             = 300
  , txFee              = 174000
  , txOutSum           = value
  , txValidContract    = True
  , txScriptSize       = 0
  , txTreasuryDonation = 0
  , txInvalidBefore    = Nothing
  , txInvalidHereafter = Nothing
  , txInputs           = []
  , txOutputs          = [sampleOut value]
  , txCollateralInputs = []
  , txReferenceInputs  = []
  , txCollateralOutput = Nothing
  , txCertificates     = []
  , txWithdrawals      = []
  , txMetadata         = Nothing
  , txMint             = []
  , txCborRaw          = Nothing
  }

sampleOut :: Word64 -> GenericTxOut
sampleOut value = GenericTxOut
  { txOutIndex       = 0
  , txOutAddress     = "addr_test1xyz"
  , txOutAddressRaw  = sampleAddrRaw
  , txOutValue       = value
  , txOutDataHash    = Nothing
  , txOutInlineDatum = Nothing
  , txOutRefScript   = Nothing
  , txOutMultiAssets = []
  }

-- | Block 1 — the one we'll roll back to. Its hash is the rollback
-- target.
block1WithTx :: GenericBlock
block1WithTx =
  mkBlock (BS.replicate 32 0) "" 1 100 [sampleTx 0xaa 1_000_000]

-- | Rollback target — block1's slot + hash.
target :: CardanoPoint
target = mkCardanoPoint 100 (BS.replicate 32 0)

block2WithTxMeta :: GenericBlock
block2WithTxMeta =
  mkBlock
    (BS.replicate 32 1)
    (BS.replicate 32 0)
    2 120
    [ (sampleTx 0xbb 2_000_000)
        { txMetadata = Just (Map.singleton 42 (Metadata.S "hello"))
        }
    ]

block3WithTx :: GenericBlock
block3WithTx =
  mkBlock (BS.replicate 32 2) (BS.replicate 32 1) 3 140 [sampleTx 0xcc 3_000_000]
