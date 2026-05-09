-- | Test runner.
--
-- Specs are grouped under three top-level categories so the
-- @cabal test@ output makes it obvious what each block exercises:
--
--   * "Unit tests" — pure, no IO beyond deterministic helpers; fast.
--   * "Property tests" — QuickCheck over arbitrary 'CardanoBlock'.
--   * "Database integration" — require a running @dbsync_test@
--     PostgreSQL database; each spec sets up + tears down its own
--     schema.
--
-- All categories run by default in CI and locally. To run a single
-- category use @--match "Database integration"@ etc.
module Main
  ( main
  ) where

import Cardano.Prelude

import Test.Hspec (describe, hspec)

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
import qualified DbSync.Extractor.UTxOSpec as ExtractorUTxOSpec
import qualified DbSync.Ingest.ConsumerSpec as IngestConsumerSpec
import qualified DbSync.Ingest.PipelineSpec as IngestPipelineSpec
import qualified DbSync.Ledger.StateSpec as LedgerStateSpec
import qualified DbSync.Ledger.TypesSpec as LedgerTypesSpec
import qualified DbSync.Ledger.WorkerSpec as LedgerWorkerSpec
import qualified DbSync.Phase.BootSpec as PhaseBootSpec
import qualified DbSync.Schema.AdaPotsSpec as SchemaAdaPotsSpec
import qualified DbSync.Schema.AddressSpec as SchemaAddressSpec
import qualified DbSync.Schema.CoreSpec as SchemaCoreSpec
import qualified DbSync.Schema.GenerateSpec as SchemaGenerateSpec
import qualified DbSync.Schema.GovernanceSpec as SchemaGovernanceSpec
import qualified DbSync.Schema.RewardSpec as SchemaRewardSpec
import qualified DbSync.Schema.ScriptsDatumsSpec as SchemaScriptsDatumsSpec
import qualified DbSync.Schema.SyncStateSpec as SchemaSyncStateSpec
import qualified DbSync.StateQuery.ObservedSummarySpec as ObservedSummarySpec
import qualified DbSync.Block.MetadataSpec as BlockMetadataSpec
import qualified DbSync.Util.Bech32Spec as UtilBech32Spec

-- Property tests
import qualified DbSync.PropertySpec as PropertySpec

-- Database integration
import qualified DbSync.Checkpoint.ManagerSpec as CheckpointManagerSpec
import qualified DbSync.Checkpoint.ResumeSpec as CheckpointResumeSpec
import qualified DbSync.Checkpoint.SyncStateSpec as CheckpointSyncStateSpec
import qualified DbSync.Copy.WriterSpec as CopyWriterSpec
import qualified DbSync.Db.Statement.BlockSpec as DbStatementBlockSpec
import qualified DbSync.Db.Statement.SlotLeaderSpec as DbStatementSlotLeaderSpec
import qualified DbSync.Db.Statement.SyncStateSpec as DbStatementSyncStateSpec
import qualified DbSync.Phase.FollowingChainTipSpec as PhaseFollowingChainTipSpec
import qualified DbSync.Phase.MockChainSpec as PhaseMockChainSpec
import qualified DbSync.Phase.PreparingForChainTipSpec as PhasePreparingForChainTipSpec
import qualified DbSync.Schema.InitSpec as SchemaInitSpec

main :: IO ()
main = hspec $ do
  describe "Unit tests" $ do
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
    ExtractorUTxOSpec.spec
    IngestConsumerSpec.spec
    IngestPipelineSpec.spec
    LedgerStateSpec.spec
    LedgerTypesSpec.spec
    LedgerWorkerSpec.spec
    PhaseBootSpec.spec
    SchemaAdaPotsSpec.spec
    SchemaAddressSpec.spec
    SchemaCoreSpec.spec
    SchemaGenerateSpec.spec
    SchemaGovernanceSpec.spec
    SchemaRewardSpec.spec
    SchemaScriptsDatumsSpec.spec
    SchemaSyncStateSpec.spec
    ObservedSummarySpec.spec
    UtilBech32Spec.spec
    BlockMetadataSpec.spec

  describe "Property tests" $
    PropertySpec.spec

  describe "Database integration" $ do
    CheckpointManagerSpec.spec
    CheckpointResumeSpec.spec
    CheckpointSyncStateSpec.spec
    CopyWriterSpec.spec
    DbStatementBlockSpec.spec
    DbStatementSlotLeaderSpec.spec
    DbStatementSyncStateSpec.spec
    PhaseFollowingChainTipSpec.spec
    PhaseMockChainSpec.spec
    PhasePreparingForChainTipSpec.spec
    SchemaInitSpec.spec
