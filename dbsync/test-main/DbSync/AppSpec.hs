-- | Tests for the application startup pipeline.
--
-- Tests buildCoreEnv and the initial App.run startup logging.
module DbSync.AppSpec
  ( spec
  ) where

import Cardano.Prelude

import Data.IORef (newIORef, readIORef)
import qualified Data.Text as Text

import DbSync.App (buildCoreEnv, runStartup)
import DbSync.Config (parseConfig)
import DbSync.Config.Node (parseNodeConfig)
import DbSync.Config.Types (NodeConfig, SyncConfig (..))
import DbSync.Config.Validation (validateConfig)
import DbSync.Env (CoreEnv (..))
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Trace.Backend (mkTestTracer)
import DbSync.Trace.Types (LogMsg (..), Severity (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- | Load a valid SyncConfig + NodeConfig for testing.
loadTestConfigs :: IO (SyncConfig, NodeConfig)
loadTestConfigs = do
  Right syncCfg <- parseConfig "test-fixtures/full-config.json"
  Right validCfg <- pure $ validateConfig syncCfg
  Right nodeCfg <- parseNodeConfig "test-fixtures/node-config.json"
  pure (validCfg, nodeCfg)

spec :: Spec
spec = describe "DbSync.App" $ do
  describe "buildCoreEnv" $ do
    it "constructs CoreEnv with config and node config" $ do
      (syncCfg, nodeCfg) <- loadTestConfigs
      logRef <- newIORef []
      let tracer = mkTestTracer logRef
      env <- buildCoreEnv tracer syncCfg nodeCfg
      ceConfig env `shouldBe` syncCfg
      ceNodeConfig env `shouldBe` nodeCfg

    it "builds extractors list matching enabled config" $ do
      (syncCfg, nodeCfg) <- loadTestConfigs
      logRef <- newIORef []
      let tracer = mkTestTracer logRef
      env <- buildCoreEnv tracer syncCfg nodeCfg
      -- full-config.json: 9 enabled (core, utxo, multi_asset, metadata,
      -- stake_delegation, pool, scripts_datums, governance, epoch_boundary)
      -- 2 disabled (cbor, current_state)
      let projCount = length (ceExtractors env)
      projCount `shouldBe` 9

    it "uses real coreExtractor (not a stub) for 'core'" $ do
      (syncCfg, nodeCfg) <- loadTestConfigs
      logRef <- newIORef []
      let tracer = mkTestTracer logRef
      env <- buildCoreEnv tracer syncCfg nodeCfg
      let coreExts = filter (\e -> pdName e == "core") (ceExtractors env)
      length coreExts `shouldBe` 1
      -- Real coreExtractor owns 3 tables; stub has 0
      let tableCount = length $ pdTables (headDef (panic "no core") coreExts)
      tableCount `shouldBe` 3

  describe "runStartup" $ do
    it "logs startup info from App component" $ do
      (syncCfg, nodeCfg) <- loadTestConfigs
      logRef <- newIORef []
      let tracer = mkTestTracer logRef
      env <- buildCoreEnv tracer syncCfg nodeCfg
      runStartup env
      msgs <- readIORef logRef
      let appInfoMsgs = [m | m <- msgs, lmComponent m == "App", lmSeverity m == Info]
      appInfoMsgs `shouldSatisfy` (not . null)

    it "logs enabled extractor names" $ do
      (syncCfg, nodeCfg) <- loadTestConfigs
      logRef <- newIORef []
      let tracer = mkTestTracer logRef
      env <- buildCoreEnv tracer syncCfg nodeCfg
      runStartup env
      msgs <- readIORef logRef
      let allText = mconcat [lmMessage m | m <- msgs]
      -- Should mention "core" in the extractors output
      allText `shouldSatisfy` (Text.isInfixOf "core")
