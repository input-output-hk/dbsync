-- | Tests for application startup, extractor list construction, and
-- the dependency-validation + topological-sort logic that orders
-- extractors before the pipeline dispatches blocks.
module DbSync.AppSpec
  ( spec
  ) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (Network (..))
import Data.IORef (newIORef, readIORef)
import qualified Data.Text as Text

import DbSync.App
  ( buildCoreEnv
  , buildExtractors
  , runStartup
  )
import DbSync.AppM (runAppM)
import DbSync.App.Config.Types (parseConfig)
import DbSync.App.Config.Node (parseNodeConfig)
import DbSync.App.Config.Types
  ( NodeConfig
  , SyncConfig (..)
  , DbSyncOptions (..)
  , OptionFlag (..)
  , UtxoOption (..)
  , defaultDbSyncOptions
  , defaultUtxoOption
  )
import DbSync.App.Config.Validation (validateConfig)
import DbSync.App.Env (CoreEnv (..))
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Trace.Backend (mkTestTracer)
import DbSync.Trace.Types (LogMsg (..), Severity (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- | Load a valid SyncConfig + NodeConfig for testing.
loadTestConfigs :: IO (SyncConfig, NodeConfig)
loadTestConfigs = do
  Right syncCfg <- parseConfig "fixtures/full-config.json"
  Right validCfg <- pure $ validateConfig syncCfg
  Right nodeCfg <- parseNodeConfig "fixtures/node-config.json"
  pure (validCfg, nodeCfg)

-- | Build DbSyncOptions with selected extractors enabled.
optionsWith :: [Text] -> DbSyncOptions
optionsWith enabled = DbSyncOptions
  { pcUtxo                  = defaultUtxoOption { uoEnabled = "utxo" `elem` enabled }
  , pcMultiAsset            = mk "multi_asset"
  , pcMetadata              = mk "metadata"
  , pcStakeDelegation       = mk "stake_delegation"
  , pcStakeDelegationLedger = mk "stake_delegation_ledger"
  , pcPool                  = mk "pool"
  , pcScriptsDatums         = mk "scripts_datums"
  , pcGovernance            = mk "governance"
  , pcCbor                  = mk "cbor"
  , pcEpochSyncStats        = mk "epoch_sync_stats"
  , pcEpochBoundary         = mk "epoch_boundary"
  , pcPoolStats             = mk "pool_stats"
  , pcEpoch                 = mk "epoch"
  , pcCurrentState          = mk "current_state"
  , pcOffChainPools         = mk "off_chain_pools"
  , pcOffChainVotes         = mk "off_chain_votes"
  }
  where
    mk name = OptionFlag (name `elem` enabled)

spec :: Spec
spec = describe "DbSync.App" $ do
  describe "buildCoreEnv" $ do
    it "constructs CoreEnv with config and node config" $ do
      (syncCfg, nodeCfg) <- loadTestConfigs
      logRef <- newIORef []
      let tracer = mkTestTracer logRef
      env <- buildCoreEnv tracer syncCfg nodeCfg Mainnet
      ceConfig env `shouldBe` syncCfg
      ceNodeConfig env `shouldBe` nodeCfg

    it "builds extractors list matching enabled config" $ do
      (syncCfg, nodeCfg) <- loadTestConfigs
      logRef <- newIORef []
      let tracer = mkTestTracer logRef
      env <- buildCoreEnv tracer syncCfg nodeCfg Mainnet
      -- full-config.json enables 11 optional extractors (utxo, multi_asset,
      -- metadata, stake_delegation, stake_delegation_ledger, pool,
      -- scripts_datums, governance, epoch_sync_stats, epoch_boundary,
      -- pool_stats). 'core' is added unconditionally and 'epoch' defaults to
      -- true, so the resolved list has 13 entries; cbor and current_state
      -- stay off.
      let projCount = length (ceExtractors env)
      projCount `shouldBe` 13

    it "uses real coreExtractor (not a stub) for 'core'" $ do
      (syncCfg, nodeCfg) <- loadTestConfigs
      logRef <- newIORef []
      let tracer = mkTestTracer logRef
      env <- buildCoreEnv tracer syncCfg nodeCfg Mainnet
      let coreExts = filter (\e -> pdName e == "core") (ceExtractors env)
      length coreExts `shouldBe` 1
      -- Real coreExtractor owns 5 tables (block, tx, slot_leader,
      -- stake_address, pool_hash); stub has 0
      let tableCount = length $ pdTables (headDef (panic "no core") coreExts)
      tableCount `shouldBe` 5

  describe "runStartup" $ do
    it "logs startup info from App component" $ do
      (syncCfg, nodeCfg) <- loadTestConfigs
      logRef <- newIORef []
      let tracer = mkTestTracer logRef
      env <- buildCoreEnv tracer syncCfg nodeCfg Mainnet
      runAppM env runStartup
      msgs <- readIORef logRef
      let appInfoMsgs = [m | m <- msgs, lmComponent m == "App", lmSeverity m == Info]
      appInfoMsgs `shouldSatisfy` (not . null)

    it "logs enabled extractor names" $ do
      (syncCfg, nodeCfg) <- loadTestConfigs
      logRef <- newIORef []
      let tracer = mkTestTracer logRef
      env <- buildCoreEnv tracer syncCfg nodeCfg Mainnet
      runAppM env runStartup
      msgs <- readIORef logRef
      let allText = mconcat [lmMessage m | m <- msgs]
      -- Should mention "core" in the extractors output
      allText `shouldSatisfy` (Text.isInfixOf "core")

  describe "buildExtractors" $ do
    it "core always comes first and is unconditional" $ do
      let result = buildExtractors (optionsWith ["utxo", "multi_asset", "stake_delegation", "pool"])
      case result of
        Left err -> panic ("unexpected failure: " <> err)
        Right xs -> headDef "" (map pdName xs) `shouldBe` "core"

    it "returns enabled extractors in declaration order" $ do
      -- Enabling utxo, stake_delegation, and multi_asset resolves to the
      -- order they appear in the option list (utxo, multi_asset, …,
      -- stake_delegation), with core prepended.
      let result = buildExtractors (optionsWith ["utxo", "stake_delegation", "multi_asset"])
      case result of
        Left err -> panic ("unexpected failure: " <> err)
        Right xs ->
          map pdName xs `shouldBe` ["core", "utxo", "multi_asset", "stake_delegation"]

    it "default options yield core + epoch (epoch defaults to true)" $ do
      case buildExtractors defaultDbSyncOptions of
        Left err -> panic ("unexpected failure: " <> err)
        Right xs -> map pdName xs `shouldBe` ["core", "epoch"]
