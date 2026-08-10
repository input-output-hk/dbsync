-- | The single source of truth for what extractors exist, plus the
-- schema fingerprint derived from them.
-- 'DbSync.App.Setup.buildExtractors' resolves the names the config
-- @extractors@ block enables against 'allKnownExtractors'; a name with
-- no match becomes a no-op stub.
module DbSync.Extractor.Registry
  ( allKnownExtractors
  , allDeclaredTables
  , declaredSchemaFingerprint
  ) where

import Cardano.Prelude

import DbSync.Extractor (ExtractorDef (..))
import DbSync.Schema.Version (Fingerprint, schemaFingerprint)

import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.Cbor (cborExtractor)
import DbSync.Extractor.Epoch (epochExtractor)
import DbSync.Extractor.EpochBoundary (epochBoundaryExtractor)
import DbSync.Extractor.EpochSyncStats (epochSyncStatsExtractor)
import DbSync.Extractor.Governance (governanceExtractor)
import DbSync.Extractor.Metadata (metadataExtractor)
import DbSync.Extractor.MultiAsset (multiAssetExtractor)
import DbSync.Extractor.OffChainPools (offChainPoolsExtractor)
import DbSync.Extractor.OffChainVotes (offChainVotesExtractor)
import DbSync.Extractor.Pool (poolExtractor)
import DbSync.Extractor.PoolStats (poolStatsExtractor)
import DbSync.Extractor.ScriptsDatums (scriptsDatumsExtractor)
import DbSync.Extractor.StakeDelegation (stakeDelegationExtractor)
import DbSync.Extractor.StakeDelegationLedger (stakeDelegationLedgerExtractor)
import DbSync.Extractor.UTxO (utxoExtractor)

import DbSync.Db.Schema.EpochParamPending (epochParamPendingTableDef)
import DbSync.Db.Schema.EpochView (createEpochViewsSql)
import DbSync.Db.Schema.SyncState (syncStateTableDef)
import DbSync.Db.Schema.Types (TableDef)

-- | 'coreExtractor' leads the list and is unconditional, because every
-- other extractor's tables reference its block, tx and slot_leader
-- rows. The @extractors@ config block enables the names listed here.
-- @current_state@ has no implementation, so it is absent and
-- 'DbSync.App.Setup.buildExtractors' resolves it to a stub.
allKnownExtractors :: [ExtractorDef]
allKnownExtractors =
  [ coreExtractor
  , utxoExtractor
  , multiAssetExtractor
  , metadataExtractor
  , stakeDelegationExtractor
  , stakeDelegationLedgerExtractor
  , poolExtractor
  , scriptsDatumsExtractor
  , governanceExtractor
  , cborExtractor
  , epochSyncStatsExtractor
  , epochBoundaryExtractor
  , poolStatsExtractor
  , epochExtractor
  , offChainPoolsExtractor
  , offChainVotesExtractor
  ]

-- | Covers exactly the objects a full
-- 'DbSync.Db.Schema.Init.initSchema' creates, so a fingerprint taken
-- from a live, fully-migrated database matches this value. CI pins it
-- against 'releasedSchemaFingerprints', and the boot backstop compares
-- it to the value in @dbsync_sync_state@.
--
-- 'schemaFingerprint' sorts tables by name, so the declaration order of
-- 'allKnownExtractors' does not change the result.
declaredSchemaFingerprint :: Fingerprint
declaredSchemaFingerprint =
  schemaFingerprint allDeclaredTables [createEpochViewsSql]

allDeclaredTables :: [TableDef]
allDeclaredTables =
  concatMap pdTables allKnownExtractors
    <> [ syncStateTableDef
       , epochParamPendingTableDef
       ]
