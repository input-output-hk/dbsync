{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Diff test: the buffered Follow path and the immediate Follow
-- path must produce identical rows for identical input.
--
-- The buffered path pre-allocates IDs, accumulates INSERTs in a
-- per-block 'WriteBuffer', and flushes them as one libpq pipeline
-- at end of block. The immediate path issues each INSERT as its own
-- 'Conn.use' round-trip. Both should reach the same end-state in
-- PG; this spec forces the issue by running the same fixture
-- through each runner and diffing row counts plus key contents.
--
-- Catches regressions where:
--
--   * The buffered path skips a write (e.g. forgot to wire a
--     'writeXxx' field to the buffer).
--   * The pre-allocator under- or over-allocates IDs (counter
--     drift between 'IdCounts.countAssignableIds' and the
--     extractor pass).
--   * The per-block dedup cache shadows a row that should hit PG,
--     or fails to shadow one that should be cache-hit.
module DbSync.Phase.Following.BufferedDiffSpec (spec) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (Network (..))

import qualified Data.Text as T
import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import Test.Hspec (Spec, afterAll_, beforeAll_, before_, describe, it, shouldBe)

import DbSync.Block.Types (GenericBlock)
import DbSync.Db.Phase (SyncPhase (..))
import DbSync.Db.Transaction (withTransactionOn)
import DbSync.Extractor (ExtractorDef, emptyBlockLedgerData)
import DbSync.Extractor.Cbor (cborExtractor)
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.Metadata (metadataExtractor)
import DbSync.Extractor.MultiAsset (multiAssetExtractor)
import DbSync.Extractor.Pool (poolExtractor)
import DbSync.Extractor.StakeDelegation (stakeDelegationExtractor)
import DbSync.Extractor.UTxO (utxoExtractor)
import DbSync.Block.Pipeline (processBlock)
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
  , collateralTxOutTableDef
  , referenceTxInTableDef
  , txInTableDef
  , txOutTableDef
  )
import DbSync.Phase.Following.IdAllocator (allocateAllIds)
import DbSync.Phase.Following.IdCounts (countAssignableIds)
import DbSync.Phase.Following.Resolver (mkBufferedFollowResolver, mkFollowResolver)
import DbSync.Phase.Following.WriteBuffer (drain, newWriteBuffer)
import DbSync.Phase.Following.Writer (mkBufferedWriter, mkWriter)
import DbSync.Test.Database
  ( queryTestDb
  , setupFollowTipSchema
  , teardownSchema
  , truncateAllTables
  )
import DbSync.Test.Fixtures (producerBlock, spendingBlock)
import DbSync.Test.Hasql (withTestConnection)
import DbSync.Test.PipelineEnv (mkTestPipelineEnvWith)

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec =
  beforeAll_ (setupFollowTipSchema tables extractorVersions) $
    afterAll_ (teardownSchema tables) $
      before_ (truncateAllTables tableNames) $
        describe "buffered vs immediate Follow path" $ do

          it "produces identical row counts for the producer/spending fixtures" $ do
            immediate <- diffSnapshot runImmediate rowCounts
            truncateAllTables tableNames
            buffered  <- diffSnapshot runBuffered  rowCounts
            buffered `shouldBe` immediate

          it "produces identical column contents for the same fixtures" $ do
            immediate <- diffSnapshot runImmediate contentSamples
            truncateAllTables tableNames
            buffered  <- diffSnapshot runBuffered  contentSamples
            buffered `shouldBe` immediate

-- | Run the chosen runner over the canonical fixture pair, then
-- snapshot the database state via the chosen sampler.
diffSnapshot
  :: ([GenericBlock] -> IO ())   -- ^ runner under test
  -> IO snap                     -- ^ snapshot taker
  -> IO snap
diffSnapshot run takeSnap = do
  run [producerBlock, spendingBlock]
  takeSnap

-- ---------------------------------------------------------------------------
-- Runners
-- ---------------------------------------------------------------------------

-- | Process @blocks@ through the immediate-INSERT Follow path.
runImmediate :: [GenericBlock] -> IO ()
runImmediate blocks =
  withTestConnection $ \conn -> do
    resolver <- mkFollowResolver conn
    let writer = mkWriter conn
        env    =
          mkTestPipelineEnvWith
            Mainnet
            resolver
            writer
            extractors
            (\_ -> pure emptyBlockLedgerData)
            FollowingChainTip
    for_ blocks $ \blk -> runReaderT (processBlock blk) env

-- | Process @blocks@ through the buffered Follow path.
--
-- Mirrors 'Phase.Following.Run.processForward': pre-allocate IDs,
-- run the extractor pass with the buffered resolver/writer, flush
-- the pipeline in one round-trip per block.
runBuffered :: [GenericBlock] -> IO ()
runBuffered blocks =
  withTestConnection $ \conn ->
    for_ blocks $ \blk -> do
      let counts = countAssignableIds blk
      preAllocated <- allocateAllIds conn counts
      buf          <- newWriteBuffer
      resolver     <- mkBufferedFollowResolver conn preAllocated buf
      let writer = mkBufferedWriter buf
          env    =
            mkTestPipelineEnvWith
              Mainnet
              resolver
              writer
              extractors
              (\_ -> pure emptyBlockLedgerData)
              FollowingChainTip
      withTransactionOn conn $ do
        runReaderT (processBlock blk) env
        writes <- drain buf
        result <- Conn.use conn (Sess.pipeline writes)
        case result of
          Right () -> pure ()
          Left  e  -> panic $ "buffered runner: pipeline flush: " <> show e

-- ---------------------------------------------------------------------------
-- Snapshots
-- ---------------------------------------------------------------------------

-- | Row count per table the fixtures touch. Sorted alphabetically
-- so the comparison is stable in failure messages.
rowCounts :: IO [(Text, Text)]
rowCounts =
  traverse countOne sampleTables
  where
    countOne tbl = do
      n <- T.strip <$> queryTestDb ("SELECT count(*) FROM \"" <> tbl <> "\";")
      pure (tbl, n)

-- | Row counts plus a sample of key columns. Detects divergences
-- the count-only snapshot can't see (e.g. same row count but a
-- different address mapped, or an ID gap that shifted FKs).
contentSamples :: IO [(Text, Text)]
contentSamples = do
  counts <- rowCounts
  details <-
    traverse
      (\(label, sql) -> (label,) . T.strip <$> queryTestDb sql)
      [ ("block hashes",       "SELECT string_agg(encode(hash, 'hex'),    ',' ORDER BY id) FROM block")
      , ("tx hashes",          "SELECT string_agg(encode(hash, 'hex'),    ',' ORDER BY id) FROM tx")
      , ("tx fees",            "SELECT string_agg(fee::text,              ',' ORDER BY id) FROM tx")
      , ("tx_out values",      "SELECT string_agg(value::text,            ',' ORDER BY id) FROM tx_out")
      , ("tx_out address_ids", "SELECT string_agg(coalesce(address_id::text, 'NULL'), ',' ORDER BY id) FROM tx_out")
      , ("address raws",       "SELECT string_agg(encode(raw, 'hex'),     ',' ORDER BY id) FROM address")
      , ("slot leader hashes", "SELECT string_agg(encode(hash, 'hex'),    ',' ORDER BY id) FROM slot_leader")
      ]
  pure (counts ++ details)

sampleTables :: [Text]
sampleTables =
  [ "block"
  , "tx"
  , "slot_leader"
  , "tx_out"
  , "tx_in"
  , "collateral_tx_in"
  , "address"
  , "stake_address"
  , "pool_hash"
  , "tx_metadata"
  , "multi_asset"
  , "ma_tx_out"
  , "ma_tx_mint"
  ]

-- ---------------------------------------------------------------------------
-- Extractor set + schema setup
-- ---------------------------------------------------------------------------

-- Mirrors 'tests/main/integration/DbSync/Phase/Following/RunSpec.hs'
-- so this spec exercises the same surface.
extractors :: [ExtractorDef]
extractors =
  [ coreExtractor
  , utxoExtractor
  , multiAssetExtractor
  , metadataExtractor
  , stakeDelegationExtractor
  , poolExtractor
  , cborExtractor
  ]

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
  , "collateral_tx_out"
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
