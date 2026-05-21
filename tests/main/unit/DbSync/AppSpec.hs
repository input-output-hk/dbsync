-- | Tests for application startup, extractor list construction, and
-- the dependency-validation + topological-sort logic that orders
-- extractors before the pipeline dispatches blocks.
module DbSync.AppSpec
  ( spec
  ) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (Network (..))
import qualified Control.Exception as Exception
import Data.IORef (newIORef, readIORef)
import Data.List (elemIndex)
import qualified Data.Text as Text

import DbSync.App
  ( buildCoreEnv
  , buildExtractors
  , runStartup
  , topoSortExtractors
  , validateExtractorDeps
  )
import DbSync.AppM (runAppM)
import DbSync.Config (parseConfig)
import DbSync.Config.Node (parseNodeConfig)
import DbSync.Config.Types
  ( NodeConfig
  , SyncConfig (..)
  , SyncOptions (..)
  , SyncOption (..)
  , UtxoOption (..)
  , defaultSyncOptions
  , defaultUtxoOption
  )
import DbSync.Config.Validation (validateConfig)
import DbSync.Env (CoreEnv (..))
import DbSync.Error (AppError (..))
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

-- | Build SyncOptions with selected extractors enabled.
optionsWith :: [Text] -> SyncOptions
optionsWith enabled = SyncOptions
  { pcUtxo            = defaultUtxoOption { uoEnabled = "utxo" `elem` enabled }
  , pcMultiAsset      = mk "multi_asset"
  , pcMetadata        = mk "metadata"
  , pcStakeDelegation = mk "stake_delegation"
  , pcPool            = mk "pool"
  , pcScriptsDatums   = mk "scripts_datums"
  , pcGovernance      = mk "governance"
  , pcCbor            = mk "cbor"
  , pcEpochSyncStats  = mk "epoch_sync_stats"
  , pcEpochBoundary   = mk "epoch_boundary"
  , pcCurrentState    = mk "current_state"
  }
  where
    mk name = SyncOption (name `elem` enabled)

-- | Build a synthetic 'ExtractorDef' with the given name, version, and
-- dependency list. Used to exercise validation / topo-sort with shapes
-- that don't (yet) exist among the real extractors — for example
-- dependency cycles.
mkStubDef :: Text -> Int -> [(Text, Int)] -> ExtractorDef
mkStubDef name version deps = ExtractorDef
  { pdName         = name
  , pdVersion      = version
  , pdDependencies = deps
  , pdTables       = []
  , pdProcess      = \_ -> pure ()
  }

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
      -- full-config.json: 10 enabled (core, utxo, multi_asset, metadata,
      -- stake_delegation, pool, scripts_datums, governance, epoch_sync_stats,
      -- epoch_boundary)
      -- 2 disabled (cbor, current_state)
      let projCount = length (ceExtractors env)
      projCount `shouldBe` 10

    it "uses real coreExtractor (not a stub) for 'core'" $ do
      (syncCfg, nodeCfg) <- loadTestConfigs
      logRef <- newIORef []
      let tracer = mkTestTracer logRef
      env <- buildCoreEnv tracer syncCfg nodeCfg Mainnet
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

  describe "buildExtractors / validateExtractorDeps / topoSortExtractors" $ do
    it "rejects multi_asset enabled without utxo with a clear message" $ do
      let result = buildExtractors (optionsWith ["multi_asset"])
      case result of
        Left err  -> do
          err `shouldSatisfy` Text.isInfixOf "multi_asset"
          err `shouldSatisfy` Text.isInfixOf "utxo"
          err `shouldSatisfy` Text.isInfixOf "db_options"
        Right _   -> panic "expected validation failure"

    it "rejects pool enabled without stake_delegation" $ do
      let result = buildExtractors (optionsWith ["pool"])
      case result of
        Left err  -> do
          err `shouldSatisfy` Text.isInfixOf "pool"
          err `shouldSatisfy` Text.isInfixOf "stake_delegation"
        Right _   -> panic "expected validation failure"

    it "rejects utxo enabled without stake_delegation" $ do
      let result = buildExtractors (optionsWith ["utxo"])
      case result of
        Left err  -> do
          err `shouldSatisfy` Text.isInfixOf "utxo"
          err `shouldSatisfy` Text.isInfixOf "stake_delegation"
        Right _   -> panic "expected validation failure"

    it "accepts utxo + stake_delegation + multi_asset together" $ do
      let result = buildExtractors (optionsWith ["utxo", "stake_delegation", "multi_asset"])
      case result of
        Left err -> panic ("unexpected validation failure: " <> err)
        Right xs ->
          map pdName xs `shouldBe` ["core", "stake_delegation", "utxo", "multi_asset"]

    it "topo-sorts: stake_delegation runs before pool" $ do
      let result = buildExtractors (optionsWith ["stake_delegation", "pool"])
      case result of
        Left err -> panic ("unexpected validation failure: " <> err)
        Right xs -> do
          let names = map pdName xs
              stakeIdx = elemIndex "stake_delegation" names
              poolIdx  = elemIndex "pool" names
          (stakeIdx, poolIdx) `shouldSatisfy` \case
            (Just s, Just p) -> s < p
            _                -> False

    it "topo-sorts: utxo runs before multi_asset" $ do
      let result = buildExtractors (optionsWith ["utxo", "stake_delegation", "multi_asset"])
      case result of
        Left err -> panic ("unexpected validation failure: " <> err)
        Right xs -> do
          let names = map pdName xs
              utxoIdx       = elemIndex "utxo" names
              multiAssetIdx = elemIndex "multi_asset" names
          (utxoIdx, multiAssetIdx) `shouldSatisfy` \case
            (Just u, Just m) -> u < m
            _                -> False

    it "core always comes first" $ do
      let result = buildExtractors (optionsWith ["utxo", "multi_asset", "stake_delegation", "pool"])
      case result of
        Left err -> panic ("unexpected validation failure: " <> err)
        Right xs -> do
          let names = map pdName xs
          headDef "" names `shouldBe` "core"

    it "version mismatch is reported with min-version detail" $ do
      let exts = [ mkStubDef "core" 1 []
                 , mkStubDef "consumer" 1 [("core", 5)]  -- requires v5, only have v1
                 ]
      case validateExtractorDeps exts of
        Left err  -> do
          err `shouldSatisfy` Text.isInfixOf "consumer"
          err `shouldSatisfy` Text.isInfixOf "core"
          err `shouldSatisfy` Text.isInfixOf ">= 5"
        Right ()  -> panic "expected version validation failure"

    it "version match (>=) succeeds" $ do
      let exts = [ mkStubDef "core" 5 []
                 , mkStubDef "consumer" 1 [("core", 3)]  -- needs >= 3, has 5
                 ]
      validateExtractorDeps exts `shouldBe` Right ()

    it "detects two-node dependency cycle" $ do
      let exts = [ mkStubDef "alpha" 1 [("beta", 1)]
                 , mkStubDef "beta"  1 [("alpha", 1)]
                 ]
      case topoSortExtractors exts of
        Left err  -> do
          err `shouldSatisfy` Text.isInfixOf "Cyclic"
          err `shouldSatisfy` Text.isInfixOf "alpha"
          err `shouldSatisfy` Text.isInfixOf "beta"
        Right _   -> panic "expected cycle detection"

    it "detects three-node dependency cycle" $ do
      let exts = [ mkStubDef "x" 1 [("y", 1)]
                 , mkStubDef "y" 1 [("z", 1)]
                 , mkStubDef "z" 1 [("x", 1)]
                 ]
      case topoSortExtractors exts of
        Left err  -> err `shouldSatisfy` Text.isInfixOf "Cyclic"
        Right _   -> panic "expected cycle detection"

    it "topoSortExtractors places dependencies before consumers (linear chain)" $ do
      let exts = [ mkStubDef "a" 1 []
                 , mkStubDef "b" 1 [("a", 1)]
                 , mkStubDef "c" 1 [("b", 1)]
                 , mkStubDef "d" 1 [("c", 1)]
                 ]
      case topoSortExtractors exts of
        Left err -> panic ("unexpected sort failure: " <> err)
        Right xs -> map pdName xs `shouldBe` ["a", "b", "c", "d"]

    it "topoSortExtractors handles diamond dependencies" $ do
      -- a → {b, c} → d  (b and c both depend on a; d depends on both)
      let exts = [ mkStubDef "a" 1 []
                 , mkStubDef "b" 1 [("a", 1)]
                 , mkStubDef "c" 1 [("a", 1)]
                 , mkStubDef "d" 1 [("b", 1), ("c", 1)]
                 ]
      case topoSortExtractors exts of
        Left err -> panic ("unexpected sort failure: " <> err)
        Right xs -> do
          let names = map pdName xs
              idx n = elemIndex n names
          -- a before everything else; d after b and c
          (idx "a", idx "b", idx "c", idx "d") `shouldSatisfy`
            \case
              (Just ia, Just ib, Just ic, Just id_)
                -> ia < ib && ia < ic && ib < id_ && ic < id_
              _ -> False

    it "buildCoreEnv aborts startup when buildExtractors returns Left" $ do
      -- Construct a SyncConfig directly with an invalid combo so we
      -- exercise the dependency-validation failure path.
      (validCfg, nodeCfg) <- loadTestConfigs
      let badCfg = validCfg { scOptions = optionsWith ["utxo"] }
      logRef <- newIORef []
      let tracer = mkTestTracer logRef
      result <- Exception.try (buildCoreEnv tracer badCfg nodeCfg Mainnet)
      case (result :: Either AppError CoreEnv) of
        Left (AppInternalError _ msg) ->
          msg `shouldSatisfy` Text.isInfixOf "utxo"
        Left other -> panic ("expected AppInternalError, got: " <> show other)
        Right _    -> panic "expected buildCoreEnv to throw"

    it "default options (everything off) yields just the core extractor" $ do
      case buildExtractors defaultSyncOptions of
        Left err -> panic ("unexpected validation failure: " <> err)
        Right xs -> map pdName xs `shouldBe` ["core"]
