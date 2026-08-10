-- | Tests for config file parsing.
--
-- Tests read from actual JSON files in @tests/fixtures/@, so the fixtures
-- double as documentation and examples of valid configs.
module DbSync.App.Config.TypesSpec
  ( spec
  ) where

import Cardano.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import DbSync.App.Config.Types (parseConfig)
import DbSync.App.Config.Types
  ( LedgerBackend (..)
  , LedgerConfig (..)
  , LogFormat (..)
  , LoggingConfig (..)
  , MetricsConfig (..)
  , OptionFlag (..)
  , Extractors (..)
  , SyncConfig (..)
  , SyncMode (..)
  , SyncSettings (..)
  , UtxoOption (..)
  , UtxoStrategy (..)
  , defaultLedgerBackend
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "DbSync.App.Config.Types" $ do
  describe "parseConfig (full-config.json)" $ do
    it "parses all fields correctly" $ do
      result <- parseConfig "fixtures/full-config.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right cfg -> do
          -- Sync settings
          ssMode (scSync cfg) `shouldBe` SyncModeAuto

          -- Ledger
          lcEnabled (scLedger cfg) `shouldBe` True

          -- extractors: every key listed in the fixture is on, the rest are off.
          uoEnabled (exUtxo (scExtractors cfg))            `shouldBe` True
          prEnabled (exEpochBoundary (scExtractors cfg))   `shouldBe` True
          prEnabled (exCbor (scExtractors cfg))            `shouldBe` False  -- omitted
          prEnabled (exCurrentState (scExtractors cfg))    `shouldBe` False  -- omitted

          -- Metrics
          mcPrometheusPort (scMetrics cfg) `shouldBe` 8080

          -- Logging
          lgLevel (scLogging cfg) `shouldBe` "info"
          lgFormat (scLogging cfg) `shouldBe` LogFormatText

  describe "parseConfig (minimal-config.json)" $ do
    -- The fixture is an empty object: every section is optional.
    it "uses defaults for all optional fields" $ do
      result <- parseConfig "fixtures/minimal-config.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right cfg -> do
          -- Sync defaults
          ssMode (scSync cfg) `shouldBe` SyncModeAuto

          -- Ledger defaults — opt-in, off when omitted
          lcEnabled (scLedger cfg) `shouldBe` False

          -- Metrics defaults
          mcPrometheusPort (scMetrics cfg) `shouldBe` 8080

          -- Logging defaults
          lgLevel (scLogging cfg) `shouldBe` "info"
          lgFormat (scLogging cfg) `shouldBe` LogFormatText

          -- All optional extractors default to OFF (opt-in semantics).
          -- The unconditional 'core' extractor isn't represented in
          -- Extractors — it's added by buildExtractors regardless.
          uoEnabled (exUtxo (scExtractors cfg))            `shouldBe` False
          -- Per-utxo defaults — opt-in extractor, but back-pointer
          -- on by default once enabled, tx_in populated, archive.
          uoConsumedByTxId (exUtxo (scExtractors cfg))     `shouldBe` True
          uoTxIn (exUtxo (scExtractors cfg))               `shouldBe` True
          uoStrategy (exUtxo (scExtractors cfg))           `shouldBe` StrategyArchive
          prEnabled (exMultiAsset (scExtractors cfg))      `shouldBe` False
          prEnabled (exMetadata (scExtractors cfg))        `shouldBe` False
          prEnabled (exStakeDelegation (scExtractors cfg)) `shouldBe` False
          prEnabled (exStakeDelegationLedger (scExtractors cfg)) `shouldBe` False
          prEnabled (exPool (scExtractors cfg))            `shouldBe` False
          prEnabled (exScriptsDatums (scExtractors cfg))   `shouldBe` False
          prEnabled (exGovernance (scExtractors cfg))      `shouldBe` False
          prEnabled (exCbor (scExtractors cfg))            `shouldBe` False
          prEnabled (exEpochSyncStats (scExtractors cfg))  `shouldBe` False
          prEnabled (exEpochBoundary (scExtractors cfg))   `shouldBe` False
          prEnabled (exPoolStats (scExtractors cfg))       `shouldBe` False
          -- 'epoch' is the sole opt-out: defaults to true when the
          -- config omits the key.
          prEnabled (exEpoch (scExtractors cfg))           `shouldBe` True
          prEnabled (exCurrentState (scExtractors cfg))    `shouldBe` False

  describe "parseConfig (override-options.json)" $ do
    it "enables only the listed options; everything else stays off" $ do
      result <- parseConfig "fixtures/override-options.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right cfg -> do
          -- Listed in fixture
          prEnabled (exMetadata (scExtractors cfg))        `shouldBe` True
          prEnabled (exStakeDelegation (scExtractors cfg)) `shouldBe` True
          -- Not listed → off (opt-in)
          uoEnabled (exUtxo (scExtractors cfg))            `shouldBe` False
          prEnabled (exGovernance (scExtractors cfg))      `shouldBe` False
          prEnabled (exPool (scExtractors cfg))            `shouldBe` False

  describe "exEpoch opt-out semantics" $ do
    it "defaults to true when extractors omits 'epoch'" $ do
      result <- parseConfig "fixtures/full-config.json"
      case result of
        Left err  -> panic $ "Parse failed: " <> show err
        Right cfg -> prEnabled (exEpoch (scExtractors cfg)) `shouldBe` True

    it "defaults to true on a config with no extractors block at all" $ do
      result <- parseConfig "fixtures/minimal-config.json"
      case result of
        Left err  -> panic $ "Parse failed: " <> show err
        Right cfg -> prEnabled (exEpoch (scExtractors cfg)) `shouldBe` True

    it "accepts 'epoch': false explicitly" $ do
      result <- parseConfig "fixtures/epoch-disabled.json"
      case result of
        Left err  -> panic $ "Parse failed: " <> show err
        Right cfg -> prEnabled (exEpoch (scExtractors cfg)) `shouldBe` False

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

  describe "utxo parser rejects values without an implementation" $ do
    it "rejects tx_in: false (deposit backfill joins through tx_in)" $ do
      result <- parseConfig "fixtures/utxo-tx-in-disabled.json"
      case result of
        Right _ ->
          panic "Expected parse failure for utxo.tx_in = false"
        Left err ->
          Text.pack (show err) `shouldSatisfy` ("tx_in" `Text.isInfixOf`)

    it "rejects strategy: prune" $ do
      result <- parseConfig "fixtures/utxo-strategy-prune.json"
      case result of
        Right _ ->
          panic "Expected parse failure for utxo.strategy = \"prune\""
        Left err ->
          Text.pack (show err) `shouldSatisfy` ("prune" `Text.isInfixOf`)

    it "rejects strategy: from_ledger" $ do
      result <- parseConfig "fixtures/utxo-strategy-from-ledger.json"
      case result of
        Right _ ->
          panic "Expected parse failure for utxo.strategy = \"from_ledger\""
        Left err ->
          Text.pack (show err) `shouldSatisfy` ("from_ledger" `Text.isInfixOf`)

  describe "utxo boolean shorthand" $ do
    it "\"utxo\": true means defaults with enabled set" $
      Aeson.eitherDecode "true"
        `shouldBe` Right (UtxoOption True True True StrategyArchive)

    it "\"utxo\": false means defaults" $
      Aeson.eitherDecode "false"
        `shouldBe` Right (UtxoOption False True True StrategyArchive)
