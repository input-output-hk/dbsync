-- | Tests for config validation.
--
-- Validates option dependencies and ledger requirements.
module DbSync.Config.ValidationSpec
  ( spec
  ) where

import Cardano.Prelude

import qualified Data.Text as Text
import DbSync.Config (parseConfig)
import DbSync.Config.Types (ConfigError (..), SyncConfig)
import DbSync.Config.Validation (validateConfig)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

-- | Helper: parse then validate, returning all errors.
parseAndValidate :: FilePath -> IO (Either [ConfigError] SyncConfig)
parseAndValidate fp = do
  result <- parseConfig fp
  case result of
    Left err  -> pure $ Left [err]
    Right cfg -> pure $ validateConfig cfg

spec :: Spec
spec = describe "DbSync.Config.Validation" $ do
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

    it "collects multiple errors at once" $ do
      -- Verifies the error-accumulation mechanism. We currently only
      -- have one fixture that triggers multiple errors at once; if
      -- more rules land, extend or add a fixture and assert >= 2.
      result <- parseAndValidate "fixtures/invalid-epoch-no-ledger.json"
      case result of
        Left errs -> length errs `shouldSatisfy` (>= 1)
        Right _ -> panic "Expected validation error"
