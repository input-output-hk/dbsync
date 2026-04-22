-- | Tests for YAML config parsing.
--
-- Tests read from actual YAML files in @test-fixtures/@, so the fixtures
-- double as documentation and examples of valid configs.
module DbSync.Config.TypesSpec
  ( spec
  ) where

import Cardano.Prelude

import DbSync.Config (parseConfig)
import DbSync.Config.Types
  ( DatabaseConfig (..)
  , LedgerConfig (..)
  , LogFormat (..)
  , LoggingConfig (..)
  , MetricsConfig (..)
  , SyncOption (..)
  , SyncOptions (..)
  , SyncConfig (..)
  , SyncMode (..)
  , SyncSettings (..)
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "DbSync.Config" $ do
  describe "parseConfig (full-config.json)" $ do
    it "parses all fields correctly" $ do
      result <- parseConfig "test-fixtures/full-config.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right cfg -> do
          -- Database
          dcHost (scDatabase cfg) `shouldBe` "localhost"
          dcPort (scDatabase cfg) `shouldBe` 5432
          dcName (scDatabase cfg) `shouldBe` "dbsync_test"
          dcUser (scDatabase cfg) `shouldBe` ""
          dcPassword (scDatabase cfg) `shouldBe` ""

          -- Sync settings
          ssMode (scSync cfg) `shouldBe` SyncModeAuto
          ssCheckpointDir (scSync cfg) `shouldBe` "/data/checkpoints"
          ssCopyConnections (scSync cfg) `shouldBe` 12

          -- Ledger
          lcEnabled (scLedger cfg) `shouldBe` True
          lcStateDir (scLedger cfg) `shouldBe` "/data/ledger"
          lcSnapshotInterval (scLedger cfg) `shouldBe` 10

          -- Options
          prEnabled (pcCore (scOptions cfg)) `shouldBe` True
          prEnabled (pcUtxo (scOptions cfg)) `shouldBe` True
          prEnabled (pcCbor (scOptions cfg)) `shouldBe` False
          prEnabled (pcCurrentState (scOptions cfg)) `shouldBe` False

          -- Metrics
          mcPrometheusPort (scMetrics cfg) `shouldBe` 8080

          -- Logging
          lgLevel (scLogging cfg) `shouldBe` "info"
          lgFormat (scLogging cfg) `shouldBe` LogFormatText

  describe "parseConfig (minimal-config.json)" $ do
    it "uses defaults for all optional fields" $ do
      result <- parseConfig "test-fixtures/minimal-config.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right cfg -> do
          -- Sync defaults
          ssMode (scSync cfg) `shouldBe` SyncModeAuto
          ssCopyConnections (scSync cfg) `shouldBe` 12

          -- Ledger defaults
          lcEnabled (scLedger cfg) `shouldBe` True
          lcSnapshotInterval (scLedger cfg) `shouldBe` 10

          -- Metrics defaults
          mcPrometheusPort (scMetrics cfg) `shouldBe` 8080

          -- Logging defaults
          lgLevel (scLogging cfg) `shouldBe` "info"
          lgFormat (scLogging cfg) `shouldBe` LogFormatText

          -- Standard extractors enabled by default
          prEnabled (pcCore (scOptions cfg)) `shouldBe` True
          prEnabled (pcUtxo (scOptions cfg)) `shouldBe` True
          prEnabled (pcMultiAsset (scOptions cfg)) `shouldBe` True
          prEnabled (pcPool (scOptions cfg)) `shouldBe` True
          -- cbor and current_state disabled by default
          prEnabled (pcCbor (scOptions cfg)) `shouldBe` False
          prEnabled (pcCurrentState (scOptions cfg)) `shouldBe` False

  describe "parseConfig (no-database.json)" $ do
    it "fails with a config error" $ do
      result <- parseConfig "test-fixtures/no-database.json"
      result `shouldSatisfy` isLeft

  describe "parseConfig (override-options.json)" $ do
    it "overrides only the specified options" $ do
      result <- parseConfig "test-fixtures/override-options.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right cfg -> do
          -- Overridden to false
          prEnabled (pcUtxo (scOptions cfg)) `shouldBe` False
          prEnabled (pcGovernance (scOptions cfg)) `shouldBe` False
          -- Others stay at defaults
          prEnabled (pcCore (scOptions cfg)) `shouldBe` True
          prEnabled (pcMetadata (scOptions cfg)) `shouldBe` True
          prEnabled (pcStakeDelegation (scOptions cfg)) `shouldBe` True

  describe "parseConfig (ingest-mode.json)" $ do
    it "parses ingest sync mode" $ do
      result <- parseConfig "test-fixtures/ingest-mode.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right cfg ->
          ssMode (scSync cfg) `shouldBe` SyncModeIngest

  describe "parseConfig (json-logging.json)" $ do
    it "parses json log format and debug level" $ do
      result <- parseConfig "test-fixtures/json-logging.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right cfg -> do
          lgLevel (scLogging cfg) `shouldBe` "debug"
          lgFormat (scLogging cfg) `shouldBe` LogFormatJson
