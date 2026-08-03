-- | Test runner.
--
-- Specs are grouped under four top-level categories:
--
--   * "Unit tests" (@main/unit@)        — pure, no IO beyond
--     deterministic helpers; fast.
--   * "Property tests" (@main/unit@)    — QuickCheck properties over
--     arbitrary blocks; separate heading so @--match=Unit@ stays
--     example-only.
--   * "Database integration" (@main/integration@) — targeted tests
--     that stop short of the full mock-node lifecycle. Most need a
--     running @dbsync_test@ PostgreSQL database (each sets up and
--     tears down its own schema); a few drive production STM/queue
--     paths with in-memory state. No mock chainsync server.
--   * "End-to-end" (@main/e2e@)         — drive the full sync
--     lifecycle through the mock chainsync server; slowest tier.
--   * "Test harness" (@main/e2e@)       — self-tests for the mock
--     chain and mock node in @tests/lib@; exercise no production
--     code, so they do not count toward end-to-end coverage.
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
import qualified DbSync.App.CliSpec as CliSpec
import qualified DbSync.App.Config.DatabaseSpec as ConfigDatabaseSpec
import qualified DbSync.App.Config.ExamplesSpec as ConfigExamplesSpec
import qualified DbSync.App.Config.GenesisSpec as ConfigGenesisSpec
import qualified DbSync.App.Config.NodeSpec as ConfigNodeSpec
import qualified DbSync.App.Config.TypesSpec as ConfigTypesSpec
import qualified DbSync.App.Config.ValidationSpec as ConfigValidationSpec
import qualified DbSync.ChainSync.ConnectionSpec as ChainSyncConnectionSpec
import qualified DbSync.Db.Statement.IndexesSpec as DbStatementIndexesSpec
import qualified DbSync.Db.TypesSpec as DbTypesSpec
import qualified DbSync.Error.RenderSpec as ErrorRenderSpec
import qualified DbSync.Extractor.CoreSpec as ExtractorCoreSpec
import qualified DbSync.Extractor.EpochBoundarySpec as ExtractorEpochBoundarySpec
import qualified DbSync.Extractor.GovernanceSpec as ExtractorGovernanceSpec
import qualified DbSync.Extractor.MultiAssetSpec as ExtractorMultiAssetSpec
import qualified DbSync.Extractor.OffChainPoolsSpec as ExtractorOffChainPoolsSpec
import qualified DbSync.Extractor.OffChainVotesSpec as ExtractorOffChainVotesSpec
import qualified DbSync.Extractor.PoolSpec as ExtractorPoolSpec
import qualified DbSync.Extractor.PoolStatsSpec as ExtractorPoolStatsSpec
import qualified DbSync.Extractor.ScriptsDatumsSpec as ExtractorScriptsDatumsSpec
import qualified DbSync.Extractor.StakeDelegationLedgerSpec as ExtractorStakeDelegationLedgerSpec
import qualified DbSync.Extractor.StakeDelegationSpec as ExtractorStakeDelegationSpec
import qualified DbSync.Extractor.UTxOSpec as ExtractorUTxOSpec
import qualified DbSync.Phase.Ingest.ConsumerSpec as IngestConsumerSpec
import qualified DbSync.Phase.Ingest.DedupStoreSpec as IngestDedupStoreSpec
import qualified DbSync.Phase.Ingest.UtxoStoreSpec as IngestUtxoStoreSpec
import qualified DbSync.Extractor.PipelineSpec as BlockPipelineSpec
import qualified DbSync.Worker.Ledger.DepositAccumulatorSpec as LedgerDepositAccumulatorSpec
import qualified DbSync.Worker.Ledger.FingerprintSpec as LedgerFingerprintSpec
import qualified DbSync.Worker.Ledger.StateSpec as LedgerStateSpec
import qualified DbSync.Worker.Ledger.TypesSpec as LedgerTypesSpec
import qualified DbSync.Worker.Ledger.WorkerSpec as LedgerWorkerSpec
import qualified DbSync.App.BootSpec as AppBootSpec
import qualified DbSync.Phase.CurrentSpec as PhaseCurrentSpec
import qualified DbSync.Phase.Following.FlipPredicateSpec as PhaseFollowFlipPredicateSpec
import qualified DbSync.Phase.Following.IdCountsSpec as PhaseFollowIdCountsSpec
import qualified DbSync.Phase.Following.Resolver.UTxOSpec as PhaseFollowResolverUTxOSpec
import qualified DbSync.Worker.TxOut.WorkerSpec as WorkerTxOutSpec
import qualified DbSync.Schema.AdaPotsSpec as SchemaAdaPotsSpec
import qualified DbSync.Schema.AddressSpec as SchemaAddressSpec
import qualified DbSync.Schema.ColumnsConsistencySpec as SchemaColumnsConsistencySpec
import qualified DbSync.Schema.CopyShapeSpec as SchemaCopyShapeSpec
import qualified DbSync.Schema.CoreSpec as SchemaCoreSpec
import qualified DbSync.Schema.EpochBoundarySpec as SchemaEpochBoundarySpec
import qualified DbSync.Schema.EpochViewSpec as SchemaEpochViewSpec
import qualified DbSync.Schema.VersionSpec as SchemaVersionSpec
import qualified DbSync.Schema.GenerateSpec as SchemaGenerateSpec
import qualified DbSync.Schema.GovernanceSpec as SchemaGovernanceSpec
import qualified DbSync.Schema.InitPureSpec as SchemaInitPureSpec
import qualified DbSync.Schema.Migration.DiffSpec as SchemaMigrationDiffSpec
import qualified DbSync.Schema.Migration.RenderSpec as SchemaMigrationRenderSpec
import qualified DbSync.Schema.Migration.RunnerSpec as SchemaMigrationRunnerSpec
import qualified DbSync.Schema.OffChainPoolSpec as SchemaOffChainPoolSpec
import qualified DbSync.Schema.OffChainVoteSpec as SchemaOffChainVoteSpec
import qualified DbSync.Schema.ParentRefsSpec as SchemaParentRefsSpec
import qualified DbSync.Schema.RewardSpec as SchemaRewardSpec
import qualified DbSync.Schema.ScriptsDatumsSpec as SchemaScriptsDatumsSpec
import qualified DbSync.Schema.SyncStateSpec as SchemaSyncStateSpec
import qualified DbSync.StateQuery.ObservedSummarySpec as ObservedSummarySpec
import qualified DbSync.StateQuery.SlotDetailsSpec as SlotDetailsSpec
import qualified DbSync.Trace.ReplaySpec as TraceReplaySpec
import qualified DbSync.Parser.BlockSpec as ParserBlockSpec
import qualified DbSync.Parser.MetadataSpec as BlockMetadataSpec
import qualified DbSync.Parser.ParamProposalSpec as ParserParamProposalSpec
import qualified DbSync.Parser.TxSpec as ParserTxSpec
import qualified DbSync.Util.Bech32Spec as UtilBech32Spec
import qualified DbSync.Util.DedupHashSpec as UtilDedupHashSpec

-- Property tests
import qualified DbSync.PropertySpec as PropertySpec

-- Database integration
import qualified DbSync.App.NetworkGateSpec as NetworkGateSpec
import qualified DbSync.ChainSync.DeliverSpec as ChainSyncDeliverSpec
import qualified DbSync.SyncState.ManagerSpec as SyncStateManagerSpec
import qualified DbSync.SyncState.ResumeSpec as SyncStateResumeSpec
import qualified DbSync.SyncState.RowSpec as SyncStateRowSpec
import qualified DbSync.Worker.OffChain.HttpSpec as WorkerOffChainHttpSpec
import qualified DbSync.Worker.OffChain.PoolSpec as WorkerOffChainPoolSpec
import qualified DbSync.Worker.OffChain.RetrySpec as WorkerOffChainRetrySpec
import qualified DbSync.Worker.OffChain.VoteSpec as WorkerOffChainVoteSpec
import qualified DbSync.Db.LoaderSpec as LoaderSpec
import qualified DbSync.Db.Statement.BackfillSpec as DbStatementBackfillSpec
import qualified DbSync.Db.Statement.BlockSpec as DbStatementBlockSpec
import qualified DbSync.Db.Statement.RoundTripSpec as DbStatementRoundTripSpec
import qualified DbSync.Db.Statement.SlotLeaderSpec as DbStatementSlotLeaderSpec
import qualified DbSync.Db.Statement.SyncStateSpec as DbStatementSyncStateSpec
import qualified DbSync.Phase.Following.BufferedDiffSpec as PhaseFollowBufferedDiffSpec
import qualified DbSync.Phase.Following.RollbackSpec as PhaseRollbackSpec
import qualified DbSync.Phase.Following.RunSpec as PhaseFollowRunSpec
import qualified DbSync.Phase.Following.SameBlockSpendSpec as PhaseFollowSameBlockSpendSpec
import qualified DbSync.Phase.Preparing.RunSpec as PhasePrepSpec
import qualified DbSync.Schema.InitSpec as SchemaInitSpec
import qualified DbSync.Schema.Migration.LadderSpec as SchemaMigrationLadderSpec

-- End-to-end
import qualified DbSync.Phase.AlonzoInvalidTxSpec as PhaseAlonzoInvalidTxSpec
import qualified DbSync.Phase.BoundaryRecrossSpec as PhaseBoundaryRecrossSpec
import qualified DbSync.Phase.FollowAtTipSpec as PhaseFollowAtTipSpec
import qualified DbSync.Phase.FollowEpochBoundarySpec as PhaseFollowEpochBoundarySpec
import qualified DbSync.Phase.FollowGovernanceSpec as PhaseFollowGovernanceSpec
import qualified DbSync.Phase.GovernanceGenesisSpec as PhaseGovernanceGenesisSpec
import qualified DbSync.Phase.FollowScriptsDatumsSpec as PhaseFollowScriptsDatumsSpec
import qualified DbSync.Phase.FollowStakeDelegationLedgerSpec as PhaseFollowStakeDelegationLedgerSpec
import qualified DbSync.Phase.FollowPerfRealisticSpec as PhaseFollowPerfRealisticSpec
import qualified DbSync.Phase.FollowReplayOnBootSpec as PhaseFollowReplayOnBootSpec
import qualified DbSync.Phase.FollowRestartSpec as PhaseFollowRestartSpec
import qualified DbSync.Phase.FollowNodeRestartSpec as PhaseFollowNodeRestartSpec
import qualified DbSync.Phase.HandoffRedeliverySpec as PhaseHandoffRedeliverySpec
import qualified DbSync.Phase.IngestPrepFollowSpec as PhaseIngestPrepFollowSpec
import qualified DbSync.Phase.RecomputeInvariantsSpec as PhaseRecomputeInvariantsSpec
import qualified DbSync.Phase.IngestRestartSpec as PhaseIngestRestartSpec
import qualified DbSync.Phase.LsmLifecycleSpec as PhaseLsmLifecycleSpec

-- Test harness (self-tests for the mock chain/node in tests/lib)
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

-- | Runs every tier in one hspec process.
--
-- Integration and end-to-end specs share the single @dbsync_test@
-- database, each setting up and tearing down its own schema. That is
-- safe only because hspec runs specs sequentially: the schema-migration
-- ladder spec issues @DROP SCHEMA public CASCADE@, so any spec running
-- beside it would see its tables vanish. Do not add hspec @parallel@.
main :: IO ()
main = hspec $ do
  describe "Unit tests" $ withTimeoutSeconds unitTimeoutSeconds $ do
    AppSpec.spec
    CliSpec.spec
    ConfigDatabaseSpec.spec
    ConfigExamplesSpec.spec
    ConfigGenesisSpec.spec
    ConfigNodeSpec.spec
    ConfigTypesSpec.spec
    ConfigValidationSpec.spec
    ChainSyncConnectionSpec.spec
    DbStatementIndexesSpec.spec
    DbTypesSpec.spec
    ErrorRenderSpec.spec
    ExtractorCoreSpec.spec
    ExtractorEpochBoundarySpec.spec
    ExtractorGovernanceSpec.spec
    ExtractorMultiAssetSpec.spec
    ExtractorOffChainPoolsSpec.spec
    ExtractorOffChainVotesSpec.spec
    ExtractorPoolSpec.spec
    ExtractorPoolStatsSpec.spec
    ExtractorScriptsDatumsSpec.spec
    ExtractorStakeDelegationLedgerSpec.spec
    ExtractorStakeDelegationSpec.spec
    ExtractorUTxOSpec.spec
    IngestConsumerSpec.spec
    IngestDedupStoreSpec.spec
    IngestUtxoStoreSpec.spec
    BlockPipelineSpec.spec
    LedgerDepositAccumulatorSpec.spec
    LedgerFingerprintSpec.spec
    LedgerStateSpec.spec
    LedgerTypesSpec.spec
    LedgerWorkerSpec.spec
    AppBootSpec.spec
    PhaseCurrentSpec.spec
    PhaseFollowFlipPredicateSpec.spec
    PhaseFollowIdCountsSpec.spec
    PhaseFollowResolverUTxOSpec.spec
    SchemaAdaPotsSpec.spec
    SchemaAddressSpec.spec
    SchemaColumnsConsistencySpec.spec
    SchemaCopyShapeSpec.spec
    SchemaCoreSpec.spec
    SchemaEpochBoundarySpec.spec
    SchemaEpochViewSpec.spec
    SchemaVersionSpec.spec
    SchemaGenerateSpec.spec
    SchemaGovernanceSpec.spec
    SchemaInitPureSpec.spec
    SchemaMigrationDiffSpec.spec
    SchemaMigrationRenderSpec.spec
    SchemaMigrationRunnerSpec.spec
    SchemaOffChainPoolSpec.spec
    SchemaOffChainVoteSpec.spec
    SchemaParentRefsSpec.spec
    SchemaRewardSpec.spec
    SchemaScriptsDatumsSpec.spec
    SchemaSyncStateSpec.spec
    ObservedSummarySpec.spec
    SlotDetailsSpec.spec
    TraceReplaySpec.spec
    UtilBech32Spec.spec
    UtilDedupHashSpec.spec
    ParserBlockSpec.spec
    BlockMetadataSpec.spec
    ParserParamProposalSpec.spec
    ParserTxSpec.spec
    WorkerTxOutSpec.spec
    WorkerOffChainHttpSpec.spec
    WorkerOffChainRetrySpec.spec
    PhaseRollbackSpec.schemaWalkSpec

  describe "Property tests" $ withTimeoutSeconds unitTimeoutSeconds $
    PropertySpec.spec

  describe "Database integration" $ withTimeoutSeconds integrationTimeoutSeconds $ do
    NetworkGateSpec.spec
    ChainSyncDeliverSpec.spec
    SyncStateManagerSpec.spec
    SyncStateResumeSpec.spec
    SyncStateRowSpec.spec
    WorkerOffChainPoolSpec.spec
    WorkerOffChainVoteSpec.spec
    LoaderSpec.spec
    DbStatementBackfillSpec.spec
    DbStatementBlockSpec.spec
    DbStatementRoundTripSpec.spec
    DbStatementSlotLeaderSpec.spec
    DbStatementSyncStateSpec.spec
    PhaseFollowBufferedDiffSpec.spec
    PhaseFollowRunSpec.spec
    PhaseFollowSameBlockSpendSpec.spec
    PhasePrepSpec.spec
    PhaseRollbackSpec.cascadeSpec
    PhaseRollbackSpec.kSafetyGuardSpec
    PhaseRollbackSpec.rollbackToSlotSpec
    PhaseRollbackSpec.epochKeyedSpec
    PhaseRollbackSpec.consumedByNullOutSpec
    SchemaInitSpec.spec
    SchemaMigrationLadderSpec.spec

  describe "End-to-end" $ withTimeoutSeconds e2eTimeoutSeconds $ do
    PhaseAlonzoInvalidTxSpec.spec
    PhaseIngestPrepFollowSpec.spec
    PhaseHandoffRedeliverySpec.spec
    PhaseIngestRestartSpec.spec
    PhaseLsmLifecycleSpec.spec
    PhaseFollowRestartSpec.spec
    PhaseFollowNodeRestartSpec.spec
    PhaseFollowReplayOnBootSpec.spec
    PhaseFollowAtTipSpec.spec
    PhaseFollowEpochBoundarySpec.spec
    PhaseBoundaryRecrossSpec.spec
    PhaseFollowScriptsDatumsSpec.spec
    PhaseFollowGovernanceSpec.spec
    PhaseGovernanceGenesisSpec.spec
    PhaseFollowStakeDelegationLedgerSpec.spec
    PhaseFollowPerfRealisticSpec.spec
    PhaseRecomputeInvariantsSpec.spec

  describe "Test harness" $ withTimeoutSeconds e2eTimeoutSeconds $ do
    PhaseMockChainSpec.spec
    PhaseMockNodeSpec.spec
