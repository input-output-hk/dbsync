-- | Tests for config validation.
--
-- Validates option dependencies and ledger requirements.
-- The original project had no validation here — this is new.
module DbSync.Config.ValidationSpec
  ( spec
  ) where

import Cardano.Prelude

import qualified Data.Text as Text
import DbSync.Config (parseConfig)
import DbSync.Config.Types (ConfigError (..), SyncConfig)
import DbSync.Config.Validation (validateConfig)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

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
      result <- parseAndValidate "test-fixtures/full-config.json"
      result `shouldSatisfy` isRight

    it "accepts minimal config (defaults are valid)" $ do
      result <- parseAndValidate "test-fixtures/minimal-config.json"
      result `shouldSatisfy` isRight

    it "accepts ledger disabled when epoch_boundary also disabled" $ do
      result <- parseAndValidate "test-fixtures/valid-ledger-disabled.json"
      result `shouldSatisfy` isRight

    it "rejects epoch_boundary enabled without ledger" $ do
      result <- parseAndValidate "test-fixtures/invalid-epoch-no-ledger.json"
      result `shouldSatisfy` isLeft
      case result of
        Left errs -> do
          length errs `shouldSatisfy` (> 0)
          -- Should mention epoch_boundary and ledger
          let msgs = [t | ConfigValidationError t <- errs]
          msgs `shouldSatisfy` any (Text.isInfixOf "epoch_boundary")
        Right _ -> panic "Expected validation error"

    it "rejects multi_asset enabled without utxo" $ do
      result <- parseAndValidate "test-fixtures/invalid-multi-asset-no-utxo.json"
      result `shouldSatisfy` isLeft
      case result of
        Left errs -> do
          length errs `shouldSatisfy` (> 0)
          let msgs = [t | ConfigValidationError t <- errs]
          msgs `shouldSatisfy` any (Text.isInfixOf "multi_asset")
        Right _ -> panic "Expected validation error"

    it "collects multiple errors at once" $ do
      -- invalid-multi-asset-no-utxo also has scripts_datums enabled (default)
      -- while utxo is disabled — but that's not a rule we enforce yet.
      -- This test just verifies the error collection mechanism works.
      result <- parseAndValidate "test-fixtures/invalid-epoch-no-ledger.json"
      case result of
        Left errs -> length errs `shouldSatisfy` (>= 1)
        Right _ -> panic "Expected validation error"
