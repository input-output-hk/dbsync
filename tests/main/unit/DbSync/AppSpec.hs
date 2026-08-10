-- | Tests for extractor list construction and the environment
-- plumbing that feeds the pipeline.
module DbSync.AppSpec
  ( spec
  ) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (Network (..))
import Data.IORef (newIORef)

import DbSync.App
  ( buildCoreEnv
  , buildExtractors
  )
import DbSync.App.Config.Types (parseConfig)
import DbSync.App.Config.Node (parseNodeConfig)
import DbSync.App.Config.Types
  ( NodeConfig
  , SyncConfig (..)
  , Extractors (..)
  , OptionFlag (..)
  , UtxoOption (..)
  , defaultExtractors
  , defaultUtxoOption
  )
import DbSync.App.Config.Validation (validateConfig)
import DbSync.App.Env (CoreEnv (..))
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Trace.Backend (mkTestTracer)
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Load a valid SyncConfig + NodeConfig for testing.
loadTestConfigs :: IO (SyncConfig, NodeConfig)
loadTestConfigs = do
  Right syncCfg <- parseConfig "fixtures/full-config.json"
  Right validCfg <- pure $ validateConfig syncCfg
  Right nodeCfg <- parseNodeConfig "fixtures/node-config.json"
  pure (validCfg, nodeCfg)

-- | Build Extractors with selected extractors enabled.
profileWith :: [Text] -> Extractors
profileWith enabled = Extractors
  { exUtxo                  = defaultUtxoOption { uoEnabled = "utxo" `elem` enabled }
  , exMultiAsset            = mk "multi_asset"
  , exMetadata              = mk "metadata"
  , exStakeDelegation       = mk "stake_delegation"
  , exStakeDelegationLedger = mk "stake_delegation_ledger"
  , exPool                  = mk "pool"
  , exScriptsDatums         = mk "scripts_datums"
  , exGovernance            = mk "governance"
  , exCbor                  = mk "cbor"
  , exEpochSyncStats        = mk "epoch_sync_stats"
  , exEpochBoundary         = mk "epoch_boundary"
  , exPoolStats             = mk "pool_stats"
  , exEpoch                 = mk "epoch"
  , exCurrentState          = mk "current_state"
  , exOffChainPools         = mk "off_chain_pools"
  , exOffChainVotes         = mk "off_chain_votes"
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

  describe "buildExtractors" $ do
    it "core always comes first and is unconditional" $ do
      let result = buildExtractors (profileWith ["utxo", "multi_asset", "stake_delegation", "pool"])
      case result of
        Left err -> panic ("unexpected failure: " <> err)
        Right xs -> headDef "" (map pdName xs) `shouldBe` "core"

    it "returns enabled extractors in declaration order" $ do
      -- Enabling utxo, stake_delegation, and multi_asset resolves to the
      -- order they appear in the option list (utxo, multi_asset, …,
      -- stake_delegation), with core prepended.
      let result = buildExtractors (profileWith ["utxo", "stake_delegation", "multi_asset"])
      case result of
        Left err -> panic ("unexpected failure: " <> err)
        Right xs ->
          map pdName xs `shouldBe` ["core", "utxo", "multi_asset", "stake_delegation"]

    it "default options yield core + epoch (epoch defaults to true)" $ do
      case buildExtractors defaultExtractors of
        Left err -> panic ("unexpected failure: " <> err)
        Right xs -> map pdName xs `shouldBe` ["core", "epoch"]
