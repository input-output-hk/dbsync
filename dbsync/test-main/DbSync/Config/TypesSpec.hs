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
  , ProjectionConfig (..)
  , ProjectionConfigs (..)
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
          dcName (scDatabase cfg) `shouldBe` "cardano"
          dcUser (scDatabase cfg) `shouldBe` "postgres"
          dcPassword (scDatabase cfg) `shouldBe` ""

          -- Sync settings
          ssMode (scSync cfg) `shouldBe` SyncModeAuto
          ssCheckpointDir (scSync cfg) `shouldBe` "/data/checkpoints"
          ssCopyConnections (scSync cfg) `shouldBe` 12

          -- Ledger
          lcEnabled (scLedger cfg) `shouldBe` True
          lcStateDir (scLedger cfg) `shouldBe` "/data/ledger"
          lcSnapshotInterval (scLedger cfg) `shouldBe` 10

          -- Projections
          prEnabled (pcCore (scProjections cfg)) `shouldBe` True
          prEnabled (pcUtxo (scProjections cfg)) `shouldBe` True
          prEnabled (pcCbor (scProjections cfg)) `shouldBe` False
          prEnabled (pcCurrentState (scProjections cfg)) `shouldBe` False

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

          -- Standard projections enabled by default
          prEnabled (pcCore (scProjections cfg)) `shouldBe` True
          prEnabled (pcUtxo (scProjections cfg)) `shouldBe` True
          prEnabled (pcMultiAsset (scProjections cfg)) `shouldBe` True
          -- cbor and current_state disabled by default
          prEnabled (pcCbor (scProjections cfg)) `shouldBe` False
          prEnabled (pcCurrentState (scProjections cfg)) `shouldBe` False

  describe "parseConfig (no-database.json)" $ do
    it "fails with a config error" $ do
      result <- parseConfig "test-fixtures/no-database.json"
      result `shouldSatisfy` isLeft

  describe "parseConfig (override-projections.json)" $ do
    it "overrides only the specified projections" $ do
      result <- parseConfig "test-fixtures/override-projections.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right cfg -> do
          -- Overridden to false
          prEnabled (pcUtxo (scProjections cfg)) `shouldBe` False
          prEnabled (pcGovernance (scProjections cfg)) `shouldBe` False
          -- Others stay at defaults
          prEnabled (pcCore (scProjections cfg)) `shouldBe` True
          prEnabled (pcMetadata (scProjections cfg)) `shouldBe` True
          prEnabled (pcStakeDelegation (scProjections cfg)) `shouldBe` True

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
