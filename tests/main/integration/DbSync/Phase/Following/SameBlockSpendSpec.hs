{-# LANGUAGE OverloadedStrings #-}

-- | Absolute assertions for Follow-phase spend resolution:
-- @tx_in.tx_out_id@ and @tx_out.consumed_by_tx_id@ after same-block
-- and cross-block spends, on both the buffered and the immediate
-- path. 'DbSync.Phase.Following.BufferedDiffSpec' only proves the
-- two paths agree; this spec pins what the rows must contain, so a
-- bug shared by both paths still fails.
module DbSync.Phase.Following.SameBlockSpendSpec (spec) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Text.Printf (printf)

import Cardano.Ledger.BaseTypes (Network (..))
import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import Test.Hspec
  ( Spec
  , afterAll_
  , beforeAll_
  , before_
  , describe
  , it
  , shouldReturn
  , shouldSatisfy
  )

import DbSync.Db.Schema.Address (addressTableDef)
import DbSync.Db.Schema.Core
  ( TxCols (..)
  , blockTableDef
  , poolHashTableDef
  , slotLeaderTableDef
  , stakeAddressTableDef
  , txCols
  , txTableDef
  )
import DbSync.Db.Schema.Types (TableColumn (..), TableDef (..))
import DbSync.Db.Schema.UTxO
  ( TxInCols (..)
  , TxOutCols (..)
  , collateralTxInTableDef
  , collateralTxOutTableDef
  , referenceTxInTableDef
  , txInCols
  , txInTableDef
  , txOutCols
  , txOutTableDef
  )
import DbSync.Db.Transaction (withTransactionOn)
import DbSync.Extractor (ExtractorDef, emptyBlockLedgerData)
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.Pipeline (processBlock)
import DbSync.Extractor.UTxO (utxoExtractor)
import DbSync.Parser.Types (GenericBlock)
import DbSync.Phase.Following.IdAllocator (allocateAllIds)
import DbSync.Phase.Following.IdCounts (countAssignableIds)
import DbSync.Phase.Following.Resolver
  ( ConsumedTracking (..)
  , mkBufferedFollowResolver
  , mkFollowResolver
  )
import DbSync.Phase.Following.WriteBuffer (drain, newWriteBuffer)
import DbSync.Phase.Following.Writer (mkBufferedWriter, mkWriter)
import DbSync.Phase.Type (SyncPhase (..))
import DbSync.Test.Database
  ( queryTestDb
  , setupFollowTipSchema
  , teardownSchema
  , truncateAllTables
  )
import DbSync.Test.Fixtures
  ( chainedBlock
  , chainedProducerHash
  , chainedSpenderHash
  , consumerHash
  , phase2Hash
  , producerBlock
  , producerHash
  , spendingBlock
  )
import DbSync.Test.Hasql (withTestConnection)
import DbSync.Test.PipelineEnv (mkTestPipelineEnvWith)

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec =
  beforeAll_ (setupFollowTipSchema tables) $
    afterAll_ (teardownSchema tables) $
      before_ (truncateAllTables tableNames) $
        describe "Follow-phase spend resolution" $ do
          describe "buffered path" $ do
            it "same-block chained spend resolves the producer and marks it consumed" $ do
              runBuffered TrackConsumedBy [chainedBlock]
              assertChainedSpendMarked

            it "cross-block spends mark valid and failed-tx consumption" $ do
              runBuffered TrackConsumedBy [producerBlock, spendingBlock]
              prodId  <- requireTxId producerHash
              validId <- requireTxId consumerHash
              failId  <- requireTxId phase2Hash
              spentProducerOf consumerHash `shouldReturn` prodId
              consumedByOf producerHash 0 `shouldReturn` validId
              -- The parser folds a failed phase-2 tx's collateral
              -- input into its regular inputs, so the collateral it
              -- consumed must carry a mark too.
              consumedByOf producerHash 1 `shouldReturn` failId
              consumedByOf producerHash 2 `shouldReturn` "null"
              consumedByOf producerHash 3 `shouldReturn` "null"

            it "tracking off leaves consumed_by NULL but still resolves the producer" $ do
              runBuffered SkipConsumedBy [chainedBlock]
              producerId <- requireTxId chainedProducerHash
              spentProducerOf chainedSpenderHash `shouldReturn` producerId
              consumedByOf chainedProducerHash 0 `shouldReturn` "null"

          describe "immediate path" $
            it "same-block chained spend resolves the producer and marks it consumed" $ do
              runImmediate TrackConsumedBy [chainedBlock]
              assertChainedSpendMarked

-- Shared expectation for the chained fixture: the spender's tx_in
-- row names the producing tx, exactly the spent output is marked,
-- and nothing else is.
assertChainedSpendMarked :: IO ()
assertChainedSpendMarked = do
  producerId <- requireTxId chainedProducerHash
  spenderId  <- requireTxId chainedSpenderHash
  spentProducerOf chainedSpenderHash `shouldReturn` producerId
  consumedByOf chainedProducerHash 0 `shouldReturn` spenderId
  consumedByOf chainedProducerHash 1 `shouldReturn` "null"
  consumedByOf chainedSpenderHash 0 `shouldReturn` "null"

-- ---------------------------------------------------------------------------
-- Runners (mirror BufferedDiffSpec, parameterised over tracking)
-- ---------------------------------------------------------------------------

runImmediate :: ConsumedTracking -> [GenericBlock] -> IO ()
runImmediate tracking blocks =
  withTestConnection $ \conn -> do
    resolver <- mkFollowResolver conn tracking
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

runBuffered :: ConsumedTracking -> [GenericBlock] -> IO ()
runBuffered tracking blocks =
  withTestConnection $ \conn ->
    for_ blocks $ \blk -> do
      let counts = countAssignableIds extractors blk
      preAllocated <- allocateAllIds conn counts
      buf          <- newWriteBuffer
      resolver     <- mkBufferedFollowResolver conn preAllocated buf tracking
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
-- Assertion helpers
-- ---------------------------------------------------------------------------

-- | The tx id for a fixture hash; fails the test if the row is
-- absent so the comparisons above can never pass vacuously.
requireTxId :: ByteString -> IO Text
requireTxId h = do
  i <- T.strip <$> queryTestDb
    (T.unwords
      [ "SELECT", name txCols.tcId
      , "FROM", tdName txTableDef
      , "WHERE", name txCols.tcHash, "=", hexLit h
      ])
  i <$ (i `shouldSatisfy` (not . T.null))

-- | @tx_in.tx_out_id@ (as text, @null@ when NULL) of the given
-- spender's single input.
spentProducerOf :: ByteString -> IO Text
spentProducerOf spender =
  T.strip <$> queryTestDb
    (T.unwords
      [ "SELECT COALESCE(ti." <> name txInCols.ticTxOutId <> "::text, 'null')"
      , "FROM", tdName txInTableDef, "ti"
      , "JOIN", tdName txTableDef, "s"
      , "  ON s." <> name txCols.tcId, "= ti." <> name txInCols.ticTxInId
      , "WHERE s." <> name txCols.tcHash, "=", hexLit spender
      ])

-- | @tx_out.consumed_by_tx_id@ (as text, @null@ when NULL) of the
-- given producer's output at @idx@.
consumedByOf :: ByteString -> Word16 -> IO Text
consumedByOf producer idx =
  T.strip <$> queryTestDb
    (T.unwords
      [ "SELECT COALESCE(o." <> name txOutCols.tocConsumedByTxId <> "::text, 'null')"
      , "FROM", tdName txOutTableDef, "o"
      , "JOIN", tdName txTableDef, "t"
      , "  ON t." <> name txCols.tcId, "= o." <> name txOutCols.tocTxId
      , "WHERE t." <> name txCols.tcHash, "=", hexLit producer
      , "  AND o." <> name txOutCols.tocIndex, "=", show idx
      ])

name :: TableColumn -> Text
name = tcName

hexLit :: ByteString -> Text
hexLit bs =
  "decode('" <> T.pack (concatMap (printf "%02x") (BS.unpack bs)) <> "', 'hex')"

-- ---------------------------------------------------------------------------
-- Extractor set + schema setup
-- ---------------------------------------------------------------------------

extractors :: [ExtractorDef]
extractors = [coreExtractor, utxoExtractor]

tables :: [TableDef]
tables =
  [ blockTableDef
  , txTableDef
  , slotLeaderTableDef
  , poolHashTableDef
  , stakeAddressTableDef
  , addressTableDef
  , txOutTableDef
  , txInTableDef
  , collateralTxInTableDef
  , collateralTxOutTableDef
  , referenceTxInTableDef
  ]

-- Dependent rows first so truncation never trips FK order.
tableNames :: [Text]
tableNames = map tdName
  [ txOutTableDef
  , addressTableDef
  , txInTableDef
  , collateralTxInTableDef
  , collateralTxOutTableDef
  , referenceTxInTableDef
  , stakeAddressTableDef
  , txTableDef
  , blockTableDef
  , poolHashTableDef
  , slotLeaderTableDef
  ]
