-- | The canonical registry of every extractor this binary knows how to
-- build, and the schema fingerprint derived from it.
--
-- This is the single source of truth for \"what extractors exist\":
--
--   * 'DbSync.App.Setup.buildExtractors' resolves profile-enabled names
--     against 'allKnownExtractors' (anything not found becomes a no-op
--     stub).
--   * 'declaredSchemaFingerprint' hashes the full declared schema — every
--     known extractor's tables plus the bookkeeping/system tables and the
--     epoch view DDL — so it represents exactly what a full 'initSchema'
--     would build on a fresh database. CI pins it against
--     'releasedSchemaFingerprints'; the boot backstop compares it to the
--     value stored in @dbsync_sync_state@.
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

-- | Every extractor the binary can build, in declaration order.
--
-- 'coreExtractor' is unconditional — every other extractor's tables
-- reference its block / tx / slot_leader rows — and so leads the list.
-- The names here are the ones a profile may enable; @current_state@ has
-- no implementation yet and is intentionally absent (it resolves to a
-- stub in 'buildExtractors').
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

-- | Fingerprint of the full declared schema: every known extractor's
-- tables together with the bookkeeping (@dbsync_sync_state@) and system
-- (@epoch_param_pending@) tables, plus the epoch view DDL. This mirrors
-- exactly the set of objects a full
-- 'DbSync.Db.Schema.Init.initSchema' creates, so a fingerprint taken
-- from a live, fully-migrated database matches this value.
--
-- 'schemaFingerprint' sorts tables by name, so the declaration order of
-- 'allKnownExtractors' does not affect the result.
declaredSchemaFingerprint :: Fingerprint
declaredSchemaFingerprint =
  schemaFingerprint allDeclaredTables [createEpochViewsSql]

-- | All 'TableDef's that make up the declared schema.
allDeclaredTables :: [TableDef]
allDeclaredTables =
  concatMap pdTables allKnownExtractors
    <> [ syncStateTableDef
       , epochParamPendingTableDef
       ]
