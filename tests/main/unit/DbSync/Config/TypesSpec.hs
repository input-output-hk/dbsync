-- | Tests for profile JSON parsing.
--
-- Tests read from actual JSON files in @tests/fixtures/@, so the fixtures
-- double as documentation and examples of valid configs.
module DbSync.Config.TypesSpec
  ( spec
  ) where

import Cardano.Prelude

import qualified Data.Text as Text

import DbSync.Config (parseConfig)
import DbSync.Config.Types
  ( DatabaseConfig (..)
  , LedgerBackend (..)
  , LedgerConfig (..)
  , LogFormat (..)
  , LoggingConfig (..)
  , MetricsConfig (..)
  , SyncOption (..)
  , SyncOptions (..)
  , SyncConfig (..)
  , SyncMode (..)
  , SyncSettings (..)
  , defaultLedgerBackend
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "DbSync.Config" $ do
  describe "parseConfig (full-config.json)" $ do
    it "parses all fields correctly" $ do
      result <- parseConfig "fixtures/full-config.json"
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
          ssLoaderConnections (scSync cfg) `shouldBe` 12

          -- Ledger
          lcEnabled (scLedger cfg) `shouldBe` True

          -- db_options: every key listed in the fixture is on, the rest are off.
          prEnabled (pcUtxo (scOptions cfg))            `shouldBe` True
          prEnabled (pcEpochBoundary (scOptions cfg))   `shouldBe` True
          prEnabled (pcCbor (scOptions cfg))            `shouldBe` False  -- omitted
          prEnabled (pcCurrentState (scOptions cfg))    `shouldBe` False  -- omitted

          -- Metrics
          mcPrometheusPort (scMetrics cfg) `shouldBe` 8080

          -- Logging
          lgLevel (scLogging cfg) `shouldBe` "info"
          lgFormat (scLogging cfg) `shouldBe` LogFormatText

  describe "parseConfig (minimal-config.json)" $ do
    it "uses defaults for all optional fields" $ do
      result <- parseConfig "fixtures/minimal-config.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right cfg -> do
          -- Sync defaults
          ssMode (scSync cfg) `shouldBe` SyncModeAuto
          ssLoaderConnections (scSync cfg) `shouldBe` 12

          -- Ledger defaults — opt-in, off when omitted
          lcEnabled (scLedger cfg) `shouldBe` False

          -- Metrics defaults
          mcPrometheusPort (scMetrics cfg) `shouldBe` 8080

          -- Logging defaults
          lgLevel (scLogging cfg) `shouldBe` "info"
          lgFormat (scLogging cfg) `shouldBe` LogFormatText

          -- All optional extractors default to OFF (opt-in semantics).
          -- The unconditional 'core' extractor isn't represented in
          -- SyncOptions — it's added by buildExtractors regardless.
          prEnabled (pcUtxo (scOptions cfg))            `shouldBe` False
          prEnabled (pcMultiAsset (scOptions cfg))      `shouldBe` False
          prEnabled (pcMetadata (scOptions cfg))        `shouldBe` False
          prEnabled (pcStakeDelegation (scOptions cfg)) `shouldBe` False
          prEnabled (pcPool (scOptions cfg))            `shouldBe` False
          prEnabled (pcScriptsDatums (scOptions cfg))   `shouldBe` False
          prEnabled (pcGovernance (scOptions cfg))      `shouldBe` False
          prEnabled (pcCbor (scOptions cfg))            `shouldBe` False
          prEnabled (pcEpochSyncStats (scOptions cfg))  `shouldBe` False
          prEnabled (pcEpochBoundary (scOptions cfg))   `shouldBe` False
          prEnabled (pcCurrentState (scOptions cfg))    `shouldBe` False

  describe "parseConfig (no-database.json)" $ do
    it "fails with a config error" $ do
      result <- parseConfig "fixtures/no-database.json"
      result `shouldSatisfy` isLeft

  describe "parseConfig (override-options.json)" $ do
    it "enables only the listed options; everything else stays off" $ do
      result <- parseConfig "fixtures/override-options.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right cfg -> do
          -- Listed in fixture
          prEnabled (pcMetadata (scOptions cfg))        `shouldBe` True
          prEnabled (pcStakeDelegation (scOptions cfg)) `shouldBe` True
          -- Not listed → off (opt-in)
          prEnabled (pcUtxo (scOptions cfg))            `shouldBe` False
          prEnabled (pcGovernance (scOptions cfg))      `shouldBe` False
          prEnabled (pcPool (scOptions cfg))            `shouldBe` False

  describe "parseConfig (ingest-mode.json)" $ do
    it "parses ingest sync mode" $ do
      result <- parseConfig "fixtures/ingest-mode.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right cfg ->
          ssMode (scSync cfg) `shouldBe` SyncModeIngest

  describe "parseConfig (json-logging.json)" $ do
    it "parses json log format and debug level" $ do
      result <- parseConfig "fixtures/json-logging.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right cfg -> do
          lgLevel (scLogging cfg) `shouldBe` "debug"
          lgFormat (scLogging cfg) `shouldBe` LogFormatJson

  -- LSM is the only supported backend.
  describe "ledger.backend parsing" $ do
    it "defaults to LSM when backend is omitted (minimal-config.json)" $ do
      result <- parseConfig "fixtures/minimal-config.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right cfg ->
          lcBackend (scLedger cfg) `shouldBe` defaultLedgerBackend

    it "accepts \"lsm\" explicitly (ledger-backend-lsm.json)" $ do
      result <- parseConfig "fixtures/ledger-backend-lsm.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right cfg ->
          lcBackend (scLedger cfg) `shouldBe` LedgerBackendLSM Nothing

    it "rejects \"inmemory\" with a clear D1 error (ledger-backend-inmemory.json)" $ do
      result <- parseConfig "fixtures/ledger-backend-inmemory.json"
      case result of
        Right _ ->
          panic "Expected parse failure for ledger.backend = \"inmemory\""
        Left err ->
          Text.pack (show err) `shouldSatisfy` ("inmemory" `Text.isInfixOf`)
