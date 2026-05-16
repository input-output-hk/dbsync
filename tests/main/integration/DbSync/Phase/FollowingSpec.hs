{-# LANGUAGE OverloadedStrings #-}

-- | Scenario tests for 'DbSync.Phase.Following'.
--
-- Hand-crafted 'GenericBlock' fixtures are pushed through the real
-- resolver + writer + extractors against PG. Mirrors the shape of
-- 'DbSync.Copy.WriterSpec' so the same fixtures exercise both phases.
module DbSync.Phase.FollowingSpec (spec) where

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

import Test.Hspec (Spec, afterAll_, beforeAll_, before_, describe, it, shouldBe)

import DbSync.Block.Types
  ( BlockEra (..)
  , CertAction (..)
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  , GenericTxIn (..)
  , GenericTxOut (..)
  , GenericTxWithdrawal (..)
  , PoolRegistrationData (..)
  )
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
import DbSync.Db.Schema.Types (TableDef)
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
import DbSync.Ingest.Pipeline (processBlock)
import DbSync.Db.Phase (SyncPhase (..))
import DbSync.Resolver.Follow (mkFollowResolver)
import DbSync.Test.Database
  ( queryTestDb
  , setupFollowTipSchema
  , teardownSchema
  , truncateAllTables
  )
import DbSync.Test.Hasql (withTestConnection)
import DbSync.Test.PipelineEnv (mkTestPipelineEnvWith)
import DbSync.Extractor (emptyBlockLedgerData)
import DbSync.Writer.InsertAdapter (mkInsertWriter)

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
tableNames =
  [ "tx_out"
  , "address"
  , "tx_in"
  , "collateral_tx_in"
  , "reference_tx_in"
  , "tx_metadata"
  , "ma_tx_mint"
  , "ma_tx_out"
  , "multi_asset"
  , "stake_registration"
  , "stake_deregistration"
  , "delegation"
  , "withdrawal"
  , "pool_owner"
  , "pool_relay"
  , "pool_retire"
  , "pool_metadata_ref"
  , "pool_update"
  , "stake_address"
  , "pool_hash"
  , "tx_cbor"
  , "tx"
  , "block"
  , "slot_leader"
  ]

spec :: Spec
spec = describe "DbSync.Phase.Following" $
  beforeAll_ (setupFollowTipSchema tables extractorVersions) $
  afterAll_  (teardownSchema tables) $
  before_    (truncateAllTables tableNames) $ do

    describe "single empty block" $
      it "writes 1 block + 1 slot_leader" $ do
        runFollow [emptyBlock]
        blockCount <- T.strip <$> queryTestDb "SELECT count(*) FROM block;"
        slCount    <- T.strip <$> queryTestDb "SELECT count(*) FROM slot_leader;"
        blockCount `shouldBe` "1"
        slCount    `shouldBe` "1"

    describe "two empty blocks, same leader" $ do
      it "produces 2 blocks and 1 deduped slot_leader" $ do
        runFollow [emptyBlock, emptyBlock2]
        blockCount <- T.strip <$> queryTestDb "SELECT count(*) FROM block;"
        slCount    <- T.strip <$> queryTestDb "SELECT count(*) FROM slot_leader;"
        blockCount `shouldBe` "2"
        slCount    `shouldBe` "1"

      it "links previous_id correctly" $ do
        runFollow [emptyBlock, emptyBlock2]
        result <- T.strip <$>
          queryTestDb "SELECT id, previous_id FROM block ORDER BY id;"
        let rows = T.lines result
        rows `shouldBe` ["1|", "2|1"]

    describe "block with one tx" $ do
      it "writes 1 block and 1 tx" $ do
        runFollow [blockWith1Tx]
        blockCount <- T.strip <$> queryTestDb "SELECT count(*) FROM block;"
        txCount    <- T.strip <$> queryTestDb "SELECT count(*) FROM tx;"
        blockCount `shouldBe` "1"
        txCount    `shouldBe` "1"

      it "tx.block_id references the inserted block" $ do
        runFollow [blockWith1Tx]
        result <- T.strip <$> queryTestDb "SELECT block_id FROM tx;"
        result `shouldBe` "1"

    describe "block with one tx and one output" $ do
      it "writes 1 tx_out row" $ do
        runFollow [blockWith1Out]
        n <- T.strip <$> queryTestDb "SELECT count(*) FROM tx_out;"
        n `shouldBe` "1"

      it "tx_out.tx_id references the new tx" $ do
        runFollow [blockWith1Out]
        result <- T.strip <$> queryTestDb "SELECT tx_id FROM tx_out;"
        result `shouldBe` "1"

      it "tx_out.index, value, address round-trip" $ do
        runFollow [blockWith1Out]
        result <- T.strip <$>
          queryTestDb
            "SELECT tx_out.index, tx_out.value, address.address \
            \FROM tx_out JOIN address ON address.id = tx_out.address_id;"
        result `shouldBe` "0|5000000|addr_test1xyz"

    describe "block with one tx and two outputs" $ do
      it "writes both tx_outs in order" $ do
        runFollow [blockWith2Outs]
        result <- T.strip <$>
          queryTestDb "SELECT id, tx_id, index FROM tx_out ORDER BY id;"
        let rows = T.lines result
        rows `shouldBe` ["1|1|0", "2|1|1"]

    describe "block with one tx and all input kinds" $ do
      it "writes 1 tx_in, 1 collateral_tx_in, 1 reference_tx_in" $ do
        runFollow [blockWithAllInputs]
        txInN  <- T.strip <$> queryTestDb "SELECT count(*) FROM tx_in;"
        colN   <- T.strip <$> queryTestDb "SELECT count(*) FROM collateral_tx_in;"
        refN   <- T.strip <$> queryTestDb "SELECT count(*) FROM reference_tx_in;"
        txInN  `shouldBe` "1"
        colN   `shouldBe` "1"
        refN   `shouldBe` "1"

      it "tx_in.tx_in_id references the spending tx, tx_out_id is NULL" $ do
        runFollow [blockWithAllInputs]
        result <- T.strip <$>
          queryTestDb "SELECT tx_in_id, tx_out_id, tx_out_index FROM tx_in;"
        -- '|' between empty fields renders as "1||0" for NULL tx_out_id
        result `shouldBe` "1||0"

      it "tx_in.tx_out_hash carries the referenced tx hash" $ do
        runFollow [blockWithAllInputs]
        result <- T.strip <$>
          queryTestDb "SELECT encode(tx_out_hash, 'hex') FROM tx_in;"
        -- "spent_tx_hash_in" (16 ASCII bytes) padded to 32 bytes with NULs.
        result `shouldBe`
          "7370656e745f74785f686173685f696e00000000000000000000000000000000"

    describe "block with one tx and metadata" $ do
      it "writes 1 tx_metadata row per metadata key" $ do
        runFollow [blockWithMetadata]
        n <- T.strip <$> queryTestDb "SELECT count(*) FROM tx_metadata;"
        n `shouldBe` "1"

      it "stores key, no-schema JSON, single-key CBOR, and tx_id" $ do
        runFollow [blockWithMetadata]
        result <- T.strip <$>
          queryTestDb "SELECT key, json, encode(bytes, 'hex'), tx_id FROM tx_metadata;"
        -- key=42, json="\"hello\"", bytes=cbor({42: "hello"}), tx_id=1.
        -- CBOR breakdown: 0xa1 = 1-entry map, 0x18 0x2a = uint 42,
        -- 0x65 + "hello" bytes = 5-byte text string.
        result `shouldBe` "42|\"hello\"|a1182a6568656c6c6f|1"

    describe "block minting one multi-asset" $ do
      it "writes 1 multi_asset and 1 ma_tx_mint row" $ do
        runFollow [blockWithMint]
        maN  <- T.strip <$> queryTestDb "SELECT count(*) FROM multi_asset;"
        mtmN <- T.strip <$> queryTestDb "SELECT count(*) FROM ma_tx_mint;"
        maN  `shouldBe` "1"
        mtmN `shouldBe` "1"

      it "multi_asset.policy and name round-trip via hex" $ do
        runFollow [blockWithMint]
        result <- T.strip <$>
          queryTestDb
            "SELECT encode(policy, 'hex'), encode(name, 'hex') FROM multi_asset;"
        -- "policy01" + 20 nulls = 28 bytes; "tokenA" raw = 6 bytes
        result `shouldBe` "706f6c69637930310000000000000000000000000000000000000000|746f6b656e41"

      it "ma_tx_mint.quantity carries a signed Integer (positive case)" $ do
        runFollow [blockWithMint]
        result <- T.strip <$>
          queryTestDb "SELECT quantity, tx_id, ident FROM ma_tx_mint;"
        result `shouldBe` "1000|1|1"

    describe "two transactions minting the same asset (dedup)" $
      it "produces 1 multi_asset and 2 ma_tx_mint rows" $ do
        runFollow [blockWithTwoMintsOfSameAsset]
        maN  <- T.strip <$> queryTestDb "SELECT count(*) FROM multi_asset;"
        mtmN <- T.strip <$> queryTestDb "SELECT count(*) FROM ma_tx_mint;"
        maN  `shouldBe` "1"
        mtmN `shouldBe` "2"

    describe "block with a multi-asset tx output" $
      it "writes 1 multi_asset and 1 ma_tx_out row referencing the tx_out" $ do
        runFollow [blockWithMaOut]
        maN <- T.strip <$> queryTestDb "SELECT count(*) FROM multi_asset;"
        mao <- T.strip <$>
          queryTestDb "SELECT quantity, tx_out_id, ident FROM ma_tx_out;"
        maN `shouldBe` "1"
        mao `shouldBe` "500|1|1"

    describe "block with a stake registration cert" $ do
      it "writes 1 stake_address and 1 stake_registration row" $ do
        runFollow [blockWithStakeReg]
        saN <- T.strip <$> queryTestDb "SELECT count(*) FROM stake_address;"
        srN <- T.strip <$> queryTestDb "SELECT count(*) FROM stake_registration;"
        saN `shouldBe` "1"
        srN `shouldBe` "1"

      it "stake_registration links addr_id, tx_id, epoch_no" $ do
        runFollow [blockWithStakeReg]
        result <- T.strip <$> queryTestDb
          "SELECT addr_id, cert_index, epoch_no, tx_id, deposit FROM stake_registration;"
        -- addr_id=1, cert_index=0, epoch_no=5 (from emptyBlock), tx_id=1, deposit NULL
        result `shouldBe` "1|0|5|1|"

    describe "stake registration + deregistration of same address" $
      it "deduplicates the stake_address row" $ do
        runFollow [blockWithRegThenDereg]
        saN  <- T.strip <$> queryTestDb "SELECT count(*) FROM stake_address;"
        srN  <- T.strip <$> queryTestDb "SELECT count(*) FROM stake_registration;"
        sdN  <- T.strip <$> queryTestDb "SELECT count(*) FROM stake_deregistration;"
        saN `shouldBe` "1"
        srN `shouldBe` "1"
        sdN `shouldBe` "1"

    describe "block with a withdrawal" $
      it "writes 1 stake_address and 1 withdrawal" $ do
        runFollow [blockWithWithdrawal]
        saN <- T.strip <$> queryTestDb "SELECT count(*) FROM stake_address;"
        wd  <- T.strip <$>
          queryTestDb "SELECT addr_id, tx_id, amount FROM withdrawal;"
        saN `shouldBe` "1"
        wd  `shouldBe` "1|1|7000000"

    describe "block with a minimal pool registration" $ do
      it "writes 1 pool_update, 1 stake_address, and dedupes pool_hash" $ do
        runFollow [blockWithPoolReg]
        phN <- T.strip <$> queryTestDb "SELECT count(*) FROM pool_hash;"
        puN <- T.strip <$> queryTestDb "SELECT count(*) FROM pool_update;"
        saN <- T.strip <$> queryTestDb "SELECT count(*) FROM stake_address;"
        -- 2 pool_hash rows: one for the slot leader (Shelley+ pipeline
        -- always writes one), one for the registered pool itself.
        phN `shouldBe` "2"
        puN `shouldBe` "1"
        saN `shouldBe` "1"  -- the reward addr

      it "no pool_metadata_ref / pool_owner / pool_relay rows are written" $ do
        runFollow [blockWithPoolReg]
        pmrN <- T.strip <$> queryTestDb "SELECT count(*) FROM pool_metadata_ref;"
        poN  <- T.strip <$> queryTestDb "SELECT count(*) FROM pool_owner;"
        prN  <- T.strip <$> queryTestDb "SELECT count(*) FROM pool_relay;"
        pmrN `shouldBe` "0"
        poN  `shouldBe` "0"
        prN  `shouldBe` "0"

    describe "block with a pool registration carrying metadata" $
      it "writes 1 pool_metadata_ref linked to the pool" $ do
        runFollow [blockWithPoolRegMeta]
        pmr <- T.strip <$>
          queryTestDb "SELECT pool_id, url, encode(hash, 'hex') FROM pool_metadata_ref;"
        -- pool_id = 2: the slot leader allocates id=1, the registered
        -- pool gets id=2.
        pmr `shouldBe` "2|https://pool.example.com/meta.json|6d657461686173685f33325f62797465735f70616464645f5f5f5f5f5f5f5f5f"

    describe "block with a pool retirement cert" $
      it "writes 1 pool_retire row and dedupes pool_hash" $ do
        runFollow [blockWithPoolRetire]
        phN <- T.strip <$> queryTestDb "SELECT count(*) FROM pool_hash;"
        prN <- T.strip <$> queryTestDb "SELECT count(*) FROM pool_retire;"
        -- 2 pool_hash rows: slot leader + retired pool.
        phN `shouldBe` "2"
        prN `shouldBe` "1"

    describe "block with a delegation cert (cross-extractor flow)" $
      it "writes 1 stake_address and 1 delegation row, dedupes pool_hash" $ do
        runFollow [blockWithDelegation]
        saN <- T.strip <$> queryTestDb "SELECT count(*) FROM stake_address;"
        phN <- T.strip <$> queryTestDb "SELECT count(*) FROM pool_hash;"
        d   <- T.strip <$>
          queryTestDb
            "SELECT addr_id, pool_hash_id, active_epoch_no, tx_id FROM delegation;"
        saN `shouldBe` "1"
        -- 2 pool_hash rows: slot leader (id=1) + delegation target (id=2).
        phN `shouldBe` "2"
        -- active_epoch_no = blkEpochNo (5) + 2 = 7; pool_hash_id = 2
        -- because the slot leader took id=1.
        d   `shouldBe` "1|2|7|1"

    describe "block with a tx carrying CBOR bytes" $ do
      it "writes 1 tx_cbor row when txCborRaw is set" $ do
        runFollow [blockWithCbor]
        n <- T.strip <$> queryTestDb "SELECT count(*) FROM tx_cbor;"
        n `shouldBe` "1"

      it "tx_cbor.tx_id and bytes round-trip" $ do
        runFollow [blockWithCbor]
        result <- T.strip <$>
          queryTestDb "SELECT tx_id, encode(bytes, 'hex') FROM tx_cbor;"
        -- "tx-cbor-payload" = 15 ASCII bytes
        result `shouldBe` "1|74782d63626f722d7061796c6f6164"

      it "no tx_cbor row when txCborRaw is Nothing (Byron-shape txs)" $ do
        runFollow [blockWith1Tx]
        n <- T.strip <$> queryTestDb "SELECT count(*) FROM tx_cbor;"
        n `shouldBe` "0"

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
-- Fixtures (same shape as Copy.WriterSpec)
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
  }

-- | A non-Byron Shelley-shaped raw address. Header byte 0x00 (BasePaymentKey
-- StakeKey) — payment cred is the next 28 bytes. Total 57 bytes.
sampleAddrRaw :: ByteString
sampleAddrRaw = BS.pack (0x00 : replicate 56 0x11)

sampleOut :: Word16 -> Word64 -> GenericTxOut
sampleOut idx value = GenericTxOut
  { txOutIndex       = idx
  , txOutAddress     = "addr_test1xyz"
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
  , txCertAction = CertStakeRegistration sampleStakeCred Nothing
  }

stakeDeregCert :: GenericTxCertificate
stakeDeregCert = GenericTxCertificate
  { txCertIndex  = 1
  , txCertAction = CertStakeDeregistration sampleStakeCred
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
  , txCertAction = CertDelegation sampleStakeCred samplePoolKey
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
