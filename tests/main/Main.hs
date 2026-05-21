-- | Test runner.
--
-- Specs are grouped under three top-level categories matching the
-- @hs-source-dirs@ split in @dbsync-tests.cabal@:
--
--   * "Unit tests" (@main/unit@)        — pure, no IO beyond
--     deterministic helpers; fast. Includes 'PropertySpec'.
--   * "Database integration" (@main/integration@) — require a
--     running @dbsync_test@ PostgreSQL database; each spec sets up
--     and tears down its own schema. No mock chain.
--   * "End-to-end" (@main/e2e@)         — drive the full sync
--     lifecycle through the mock chainsync server; slowest tier.
--
-- All tiers run by default. To run a single tier locally use
-- @cabal test --test-options=\"--match=Unit\"@ etc.
module Main
  ( main
  ) where

import Cardano.Prelude

import System.Timeout (timeout)
import Test.Hspec (SpecWith, around_, describe, expectationFailure, hspec)

-- Unit tests
import qualified DbSync.AppSpec as AppSpec
import qualified DbSync.CliSpec as CliSpec
import qualified DbSync.Config.GenesisSpec as ConfigGenesisSpec
import qualified DbSync.Config.NodeSpec as ConfigNodeSpec
import qualified DbSync.Config.TypesSpec as ConfigTypesSpec
import qualified DbSync.Config.ValidationSpec as ConfigValidationSpec
import qualified DbSync.Db.Statement.IndexesSpec as DbStatementIndexesSpec
import qualified DbSync.Db.Statement.SequencesSpec as DbStatementSequencesSpec
import qualified DbSync.Db.TypesSpec as DbTypesSpec
import qualified DbSync.Extractor.CoreSpec as ExtractorCoreSpec
import qualified DbSync.Extractor.EpochBoundarySpec as ExtractorEpochBoundarySpec
import qualified DbSync.Extractor.PoolSpec as ExtractorPoolSpec
import qualified DbSync.Extractor.StakeDelegationSpec as ExtractorStakeDelegationSpec
import qualified DbSync.Extractor.UTxOSpec as ExtractorUTxOSpec
import qualified DbSync.Phase.Ingest.ConsumerSpec as IngestConsumerSpec
import qualified DbSync.Phase.Ingest.UtxoCacheSpec as IngestUtxoCacheSpec
import qualified DbSync.Block.PipelineSpec as BlockPipelineSpec
import qualified DbSync.Ledger.DepositAccumulatorSpec as LedgerDepositAccumulatorSpec
import qualified DbSync.Ledger.StateSpec as LedgerStateSpec
import qualified DbSync.Ledger.TypesSpec as LedgerTypesSpec
import qualified DbSync.Ledger.WorkerSpec as LedgerWorkerSpec
import qualified DbSync.App.BootSpec as AppBootSpec
import qualified DbSync.Phase.CurrentSpec as PhaseCurrentSpec
import qualified DbSync.Schema.AdaPotsSpec as SchemaAdaPotsSpec
import qualified DbSync.Schema.AddressSpec as SchemaAddressSpec
import qualified DbSync.Schema.CoreSpec as SchemaCoreSpec
import qualified DbSync.Schema.GenerateSpec as SchemaGenerateSpec
import qualified DbSync.Schema.GovernanceSpec as SchemaGovernanceSpec
import qualified DbSync.Schema.RewardSpec as SchemaRewardSpec
import qualified DbSync.Schema.ScriptsDatumsSpec as SchemaScriptsDatumsSpec
import qualified DbSync.Schema.SyncStateSpec as SchemaSyncStateSpec
import qualified DbSync.StateQuery.ObservedSummarySpec as ObservedSummarySpec
import qualified DbSync.StateQuery.SlotDetailsSpec as SlotDetailsSpec
import qualified DbSync.Block.MetadataSpec as BlockMetadataSpec
import qualified DbSync.Util.Bech32Spec as UtilBech32Spec
import qualified DbSync.Util.DedupHashSpec as UtilDedupHashSpec

-- Property tests
import qualified DbSync.PropertySpec as PropertySpec
import qualified DbSync.Worker.TxOutSpec as WorkerTxOutSpec

-- Database integration
import qualified DbSync.Checkpoint.ManagerSpec as CheckpointManagerSpec
import qualified DbSync.Checkpoint.ResumeSpec as CheckpointResumeSpec
import qualified DbSync.Checkpoint.SyncStateSpec as CheckpointSyncStateSpec
import qualified DbSync.Db.LoaderSpec as LoaderSpec
import qualified DbSync.Db.Statement.BackfillSpec as DbStatementBackfillSpec
import qualified DbSync.Db.Statement.BlockSpec as DbStatementBlockSpec
import qualified DbSync.Db.Statement.RoundTripSpec as DbStatementRoundTripSpec
import qualified DbSync.Db.Statement.SlotLeaderSpec as DbStatementSlotLeaderSpec
import qualified DbSync.Db.Statement.SyncStateSpec as DbStatementSyncStateSpec
import qualified DbSync.Phase.Following.BufferedDiffSpec as PhaseFollowBufferedDiffSpec
import qualified DbSync.Phase.Following.RollbackSpec as PhaseRollbackSpec
import qualified DbSync.Phase.Following.RunSpec as PhaseFollowRunSpec
import qualified DbSync.Phase.Preparing.RunSpec as PhasePrepSpec
import qualified DbSync.Schema.InitSpec as SchemaInitSpec

-- End-to-end
import qualified DbSync.Phase.FollowAtTipSpec as PhaseFollowAtTipSpec
import qualified DbSync.Phase.FollowPerfRealisticSpec as PhaseFollowPerfRealisticSpec
import qualified DbSync.Phase.FollowPerfSpec as PhaseFollowPerfSpec
import qualified DbSync.Phase.FollowRestartSpec as PhaseFollowRestartSpec
import qualified DbSync.Phase.FollowRollbackOnBootSpec as PhaseFollowRollbackOnBootSpec
import qualified DbSync.Phase.IngestPrepFollowSpec as PhaseIngestPrepFollowSpec
import qualified DbSync.Phase.IngestRestartSpec as PhaseIngestRestartSpec
import qualified DbSync.Phase.MockChainSpec as PhaseMockChainSpec
import qualified DbSync.Phase.MockNodeSpec as PhaseMockNodeSpec

-- | Cap each spec item at @seconds@ so a hang fails the run with a
-- clear message instead of stalling CI.
withTimeoutSeconds :: Int -> SpecWith a -> SpecWith a
withTimeoutSeconds seconds = around_ $ \action -> do
  result <- timeout (seconds * 1_000_000) action
  case result of
    Just () -> pure ()
    Nothing ->
      expectationFailure $
        "test exceeded " <> show seconds <> "s timeout"

-- | Per-tier budgets. Unit specs are pure; integration specs hit
-- PostgreSQL; e2e specs drive a mock node through a multi-block sync.
unitTimeoutSeconds, integrationTimeoutSeconds, e2eTimeoutSeconds :: Int
unitTimeoutSeconds        = 30
integrationTimeoutSeconds = 120
e2eTimeoutSeconds         = 300

main :: IO ()
main = hspec $ do
  describe "Unit tests" $ withTimeoutSeconds unitTimeoutSeconds $ do
    AppSpec.spec
    CliSpec.spec
    ConfigGenesisSpec.spec
    ConfigNodeSpec.spec
    ConfigTypesSpec.spec
    ConfigValidationSpec.spec
    DbStatementIndexesSpec.spec
    DbStatementSequencesSpec.spec
    DbTypesSpec.spec
    ExtractorCoreSpec.spec
    ExtractorEpochBoundarySpec.spec
    ExtractorPoolSpec.spec
    ExtractorStakeDelegationSpec.spec
    ExtractorUTxOSpec.spec
    IngestConsumerSpec.spec
    IngestUtxoCacheSpec.spec
    BlockPipelineSpec.spec
    LedgerDepositAccumulatorSpec.spec
    LedgerStateSpec.spec
    LedgerTypesSpec.spec
    LedgerWorkerSpec.spec
    AppBootSpec.spec
    PhaseCurrentSpec.spec
    SchemaAdaPotsSpec.spec
    SchemaAddressSpec.spec
    SchemaCoreSpec.spec
    SchemaGenerateSpec.spec
    SchemaGovernanceSpec.spec
    SchemaRewardSpec.spec
    SchemaScriptsDatumsSpec.spec
    SchemaSyncStateSpec.spec
    ObservedSummarySpec.spec
    SlotDetailsSpec.spec
    UtilBech32Spec.spec
    UtilDedupHashSpec.spec
    BlockMetadataSpec.spec
    WorkerTxOutSpec.spec
    PhaseRollbackSpec.schemaWalkSpec

  describe "Property tests" $ withTimeoutSeconds unitTimeoutSeconds $
    PropertySpec.spec

  describe "Database integration" $ withTimeoutSeconds integrationTimeoutSeconds $ do
    CheckpointManagerSpec.spec
    CheckpointResumeSpec.spec
    CheckpointSyncStateSpec.spec
    LoaderSpec.spec
    DbStatementBackfillSpec.spec
    DbStatementBlockSpec.spec
    DbStatementRoundTripSpec.spec
    DbStatementSlotLeaderSpec.spec
    DbStatementSyncStateSpec.spec
    PhaseFollowBufferedDiffSpec.spec
    PhaseFollowRunSpec.spec
    PhasePrepSpec.spec
    PhaseRollbackSpec.cascadeSpec
    PhaseRollbackSpec.kSafetyGuardSpec
    PhaseRollbackSpec.rollbackToSlotSpec
    SchemaInitSpec.spec

  describe "End-to-end" $ withTimeoutSeconds e2eTimeoutSeconds $ do
    PhaseIngestPrepFollowSpec.spec
    PhaseIngestRestartSpec.spec
    PhaseFollowRestartSpec.spec
    PhaseFollowRollbackOnBootSpec.spec
    PhaseFollowAtTipSpec.spec
    PhaseFollowPerfSpec.spec
    PhaseFollowPerfRealisticSpec.spec
    PhaseMockChainSpec.spec
    PhaseMockNodeSpec.spec
