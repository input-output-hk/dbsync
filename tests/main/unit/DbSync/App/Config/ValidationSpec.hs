-- | Tests for config validation.
--
-- Validates option dependencies and ledger requirements.
module DbSync.App.Config.ValidationSpec
  ( spec
  ) where

import Cardano.Prelude

import qualified Data.Text as Text
import DbSync.App.Config.Types
  ( ConfigError (..)
  , Extractors (..)
  , LedgerConfig (..)
  , SyncConfig (..)
  , UtxoOption (..)
  , UtxoStrategy (..)
  , defaultExtractors
  , defaultLedgerConfig
  , defaultLoggingConfig
  , defaultMetricsConfig
  , defaultSyncSettings
  , defaultUtxoOption
  , parseConfig
  )
import DbSync.App.Config.Validation (validateConfig)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

-- | A defaults config with the utxo extractor enabled and the given
-- strategy / consumed_by_tx_id / ledger.enabled combination.
configWith :: UtxoStrategy -> Bool -> Bool -> SyncConfig
configWith strategy consumedByTxId ledgerEnabled = SyncConfig
  { scSync       = defaultSyncSettings
  , scLedger     = defaultLedgerConfig { lcEnabled = ledgerEnabled }
  , scExtractors = defaultExtractors
      { exUtxo = defaultUtxoOption
          { uoEnabled        = True
          , uoConsumedByTxId = consumedByTxId
          , uoStrategy       = strategy
          }
      }
  , scMetrics    = defaultMetricsConfig
  , scLogging    = defaultLoggingConfig
  }

-- | Helper: parse then validate, returning all errors.
parseAndValidate :: FilePath -> IO (Either [ConfigError] SyncConfig)
parseAndValidate fp = do
  result <- parseConfig fp
  case result of
    Left err  -> pure $ Left [err]
    Right cfg -> pure $ validateConfig cfg

spec :: Spec
spec = describe "DbSync.App.Config.Validation" $ do
  describe "validateConfig" $ do
    it "accepts a full valid config" $ do
      result <- parseAndValidate "fixtures/full-config.json"
      result `shouldSatisfy` isRight

    it "accepts minimal config (defaults are valid)" $ do
      result <- parseAndValidate "fixtures/minimal-config.json"
      result `shouldSatisfy` isRight

    it "accepts ledger disabled when epoch_boundary also disabled" $ do
      result <- parseAndValidate "fixtures/valid-ledger-disabled.json"
      result `shouldSatisfy` isRight

    it "rejects epoch_boundary enabled without ledger" $ do
      result <- parseAndValidate "fixtures/invalid-epoch-no-ledger.json"
      result `shouldSatisfy` isLeft
      case result of
        Left errs -> do
          length errs `shouldSatisfy` (> 0)
          -- Should mention epoch_boundary and ledger
          let msgs = [t | ConfigValidationError t <- errs]
          msgs `shouldSatisfy` any (Text.isInfixOf "epoch_boundary")
        Right _ -> panic "Expected validation error"

    it "rejects multi_asset enabled without utxo" $ do
      result <- parseAndValidate "fixtures/invalid-multi-asset-no-utxo.json"
      result `shouldSatisfy` isLeft
      case result of
        Left errs -> do
          length errs `shouldSatisfy` (> 0)
          let msgs = [t | ConfigValidationError t <- errs]
          msgs `shouldSatisfy` any (Text.isInfixOf "multi_asset")
        Right _ -> panic "Expected validation error"

    -- "prune" and "from_ledger" are still rejected at parse time, so
    -- these rules are exercised on directly constructed configs.
    it "rejects strategy prune without consumed_by_tx_id" $ do
      let cfg = configWith StrategyPrune False False
      case validateConfig cfg of
        Left errs -> do
          let msgs = [t | ConfigValidationError t <- errs]
          msgs `shouldSatisfy` any (Text.isInfixOf "consumed_by_tx_id")
        Right _ -> panic "Expected validation error"

    it "accepts strategy prune with consumed_by_tx_id" $
      validateConfig (configWith StrategyPrune True False)
        `shouldSatisfy` isRight

    it "rejects strategy from_ledger without ledger" $ do
      let cfg = configWith StrategyFromLedger True False
      case validateConfig cfg of
        Left errs -> do
          let msgs = [t | ConfigValidationError t <- errs]
          msgs `shouldSatisfy` any (Text.isInfixOf "from_ledger")
        Right _ -> panic "Expected validation error"

    it "accepts strategy from_ledger with ledger" $
      validateConfig (configWith StrategyFromLedger True True)
        `shouldSatisfy` isRight

    it "collects multiple errors at once" $ do
      -- The fixture violates two independent rules (epoch_boundary
      -- without ledger, multi_asset without utxo); accumulation must
      -- surface both rather than stopping at the first.
      result <- parseAndValidate "fixtures/invalid-two-errors.json"
      case result of
        Left errs -> do
          length errs `shouldSatisfy` (>= 2)
          let msgs = [t | ConfigValidationError t <- errs]
          msgs `shouldSatisfy` any (Text.isInfixOf "epoch_boundary")
          msgs `shouldSatisfy` any (Text.isInfixOf "multi_asset")
        Right _ -> panic "Expected validation error"
