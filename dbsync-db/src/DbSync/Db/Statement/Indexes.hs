{-# LANGUAGE OverloadedStrings #-}

-- | DDL builders for the post-load index passes.
--
-- During @IngestChainHistory@ tables are UNLOGGED with no indexes —
-- the COPY path runs flat-out. Three passes build the indexes the
-- resolves and Follow query patterns need:
--
--   * 'ingestResolveIndexStatements' — built at the start of Ingest
--     so the per-epoch address-resolver worker doesn't hash-join
--     the unindexed @tx_out@ / @address@ heaps.
--   * 'preResolveIndexStatements' — the minimum set before the
--     post-load UPDATEs run. Tables are still UNLOGGED so the build
--     skips WAL writes and the second-pass scan that @CONCURRENTLY@
--     would force.
--   * 'tableIndexStatements' — per table, the PK and unique
--     constraints (from 'tdPrimaryKey' and 'tdUniqueConstraints')
--     plus the FK/scope indexes from 'foreignKeyIndexStatements'.
--     @IF NOT EXISTS@ makes a crash-rerun of the pass a no-op.
--
-- Every index the resolve passes create is dropped again before the
-- UNLOGGED → LOGGED flip ('resolveScaffoldingIndexNames' +
-- 'dropIndexSql'): @ALTER TABLE … SET LOGGED@ rewrites the heap and
-- rebuilds every index on the table inside the ALTER, so any index
-- alive at flip time is paid for twice. The production pass after
-- the flip rebuilds the shapes Follow needs exactly once.
module DbSync.Db.Statement.Indexes
  ( IndexStatement (..)
  , tableIndexStatements
  , foreignKeyIndexStatements
  , ingestResolveIndexStatements
  , preResolveIndexStatements
  , postResolveIndexStatements
  , resolveScaffoldingIndexNames
  , dropIndexSql
  , uniqueConstraintIndexName
  , Concurrency (..)
  ) where

import Cardano.Prelude

import Data.List (lookup, nub)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import DbSync.Db.Schema.Address (AddressCols (..), addressCols, addressTableDef)
import DbSync.Db.Schema.CBOR (TxCborCols (..), txCborCols, txCborTableDef)
import DbSync.Db.Schema.Core
  ( BlockCols (..)
  , SlotLeaderCols (..)
  , TxCols (..)
  , blockCols
  , blockTableDef
  , slotLeaderCols
  , slotLeaderTableDef
  , txCols
  , txTableDef
  )
import DbSync.Db.Schema.EpochBoundary
  ( EpochParamCols (..)
  , ReserveCols (..)
  , TreasuryCols (..)
  , epochParamCols
  , epochParamTableDef
  , reserveCols
  , reserveTableDef
  , treasuryCols
  , treasuryTableDef
  )
import DbSync.Db.Schema.Governance
  ( DrepDistrCols (..)
  , ParamProposalCols (..)
  , drepDistrCols
  , drepDistrTableDef
  , paramProposalCols
  , paramProposalTableDef
  )
import DbSync.Db.Schema.Metadata (TxMetadataCols (..), txMetadataCols, txMetadataTableDef)
import DbSync.Db.Schema.MultiAsset
  ( MaTxMintCols (..)
  , MaTxOutCols (..)
  , maTxMintCols
  , maTxMintTableDef
  , maTxOutCols
  , maTxOutTableDef
  )
import DbSync.Db.Schema.Pool
  ( PoolMetadataRefCols (..)
  , PoolOwnerCols (..)
  , PoolRelayCols (..)
  , PoolRetireCols (..)
  , PoolUpdateCols (..)
  , ReservedPoolTickerCols (..)
  , poolMetadataRefCols
  , poolMetadataRefTableDef
  , poolOwnerCols
  , poolOwnerTableDef
  , poolRelayCols
  , poolRelayTableDef
  , poolRetireCols
  , poolRetireTableDef
  , poolUpdateCols
  , poolUpdateTableDef
  , reservedPoolTickerCols
  , reservedPoolTickerTableDef
  )
import DbSync.Db.Schema.ScriptsDatums
  ( DatumCols (..)
  , ExtraKeyWitnessCols (..)
  , RedeemerCols (..)
  , RedeemerDataCols (..)
  , ScriptCols (..)
  , datumCols
  , datumTableDef
  , extraKeyWitnessCols
  , extraKeyWitnessTableDef
  , redeemerCols
  , redeemerDataCols
  , redeemerDataTableDef
  , redeemerTableDef
  , scriptCols
  , scriptTableDef
  )
import DbSync.Db.Schema.StakeDelegation
  ( DelegationCols (..)
  , EpochStakeCols (..)
  , RewardCols (..)
  , StakeDeregistrationCols (..)
  , StakeRegistrationCols (..)
  , WithdrawalCols (..)
  , delegationCols
  , delegationTableDef
  , epochStakeCols
  , epochStakeTableDef
  , rewardCols
  , rewardTableDef
  , stakeDeregistrationCols
  , stakeDeregistrationTableDef
  , stakeRegistrationCols
  , stakeRegistrationTableDef
  , withdrawalCols
  , withdrawalTableDef
  )
import DbSync.Db.Schema.Types (TableColumn (..), TableDef (..))
import DbSync.Db.Schema.UTxO
  ( CollateralTxInCols (..)
  , CollateralTxOutCols (..)
  , ReferenceTxInCols (..)
  , TxInCols (..)
  , TxOutCols (..)
  , collateralTxInCols
  , collateralTxInTableDef
  , collateralTxOutCols
  , collateralTxOutTableDef
  , referenceTxInCols
  , referenceTxInTableDef
  , txInCols
  , txInTableDef
  , txOutCols
  , txOutTableDef
  )
import DbSync.Db.Sql (quoteIdent)

-- | A named @CREATE INDEX@ DDL statement. The name is carried
-- alongside the SQL so log lines and @DROP INDEX@ statements can
-- refer to the index without parsing DDL.
data IndexStatement = IndexStatement
  { isName :: !Text
  , isSql  :: !Text
  }
  deriving stock (Eq, Show)

-- | One entry for the implicit @id@ key when no primary key is
-- declared, plus one per unique constraint. A declared
-- 'tdPrimaryKey' already carries its unique index from CREATE TABLE
-- time, so emitting @\<table>_pkey_idx@ for it would just duplicate
-- the @\<table>_pkey@ constraint index.
-- 'Concurrent' = @CREATE INDEX CONCURRENTLY@ (live database, no
-- @ShareLock@); 'NonConcurrent' = full parallel maintenance worker
-- support.
tableIndexStatements :: Concurrency -> TableDef -> [IndexStatement]
tableIndexStatements conc td =
  pkStatements <> uniqueStatements <> foreignKeyIndexStatements conc td
  where
    pkStatements = case tdPrimaryKey td of
      Just _  -> []
      Nothing ->
        [renderIndex conc Unique (tdName td <> "_pkey_idx") (tdName td) ["id"]]

    uniqueStatements =
      zipWith
        (\n cols ->
           renderIndex conc Unique
             (uniqueConstraintIndexName td n)
             (tdName td)
             (NE.toList cols))
        [1 ..]
        (tdUniqueConstraints td)

-- | The name 'tableIndexStatements' emits for the @n@-th (1-based)
-- entry in @td.tdUniqueConstraints@. Callers that hand-roll a
-- non-concurrent build use this so a later concurrent re-build's
-- @IF NOT EXISTS@ matches.
uniqueConstraintIndexName :: TableDef -> Int -> Text
uniqueConstraintIndexName td n =
  tdName td <> "_unique_" <> show n <> "_idx"

-- ---------------------------------------------------------------------------
-- * Foreign-key / scope indexes
-- ---------------------------------------------------------------------------

-- | The FK and scope-column indexes the Follow query patterns rely
-- on, built table-by-table in the Prep pass. The leading columns
-- already covered by 'preResolveIndexStatements' /
-- 'postResolveIndexStatements' (the @tx_id@ / @tx_in_id@ scope
-- indexes) are omitted here.
foreignKeyIndexStatements :: Concurrency -> TableDef -> [IndexStatement]
foreignKeyIndexStatements conc td =
  [ renderIndex conc NonUnique (fkIndexName td cols) (tdName td) cols
  | cols <- maybe [] (map (map tcName)) (lookup (tdName td) fkIndexColumns)
  ]

-- | @\<table>_\<col…>_idx@ — the repo-wide name shape for a non-unique
-- index, matching the hand-rolled pre/post-resolve names.
fkIndexName :: TableDef -> [Text] -> Text
fkIndexName td cols = tdName td <> "_" <> T.intercalate "_" cols <> "_idx"

-- | Per-table FK / scope index targets, keyed by table name taken
-- straight from each 'TableDef'. Each inner list is one index's
-- column set.
fkIndexColumns :: [(Text, [[TableColumn]])]
fkIndexColumns =
  [ tableEntry blockTableDef
      [ [blockCols.bcTime]
      , [blockCols.bcSlotLeaderId]
      , [blockCols.bcPreviousId]
      ]
  , tableEntry slotLeaderTableDef
      [ [slotLeaderCols.slcPoolHashId] ]
  , tableEntry txOutTableDef
      [ [txOutCols.tocStakeAddressId]
      , [txOutCols.tocInlineDatumId]
      , [txOutCols.tocReferenceScriptId]
      , [txOutCols.tocConsumedByTxId]
      ]
  , tableEntry addressTableDef
      [ [addressCols.acPaymentCred] ]
  , tableEntry collateralTxOutTableDef
      [ [collateralTxOutCols.ctocStakeAddressId]
      , [collateralTxOutCols.ctocInlineDatumId]
      , [collateralTxOutCols.ctocReferenceScriptId]
      ]
  , tableEntry txInTableDef
      [ [txInCols.ticTxOutId]
      , [txInCols.ticRedeemerId]
      ]
  , tableEntry collateralTxInTableDef
      [ [collateralTxInCols.cticTxOutId] ]
  , tableEntry referenceTxInTableDef
      [ [referenceTxInCols.rticTxInId]
      , [referenceTxInCols.rticTxOutId]
      ]
  , tableEntry redeemerTableDef
      [ [redeemerCols.rdcTxId]
      , [redeemerCols.rdcRedeemerDataId]
      ]
  , tableEntry redeemerDataTableDef
      [ [redeemerDataCols.rddcTxId] ]
  , tableEntry datumTableDef
      [ [datumCols.dmcTxId] ]
  , tableEntry scriptTableDef
      [ [scriptCols.sccTxId] ]
  , tableEntry extraKeyWitnessTableDef
      [ [extraKeyWitnessCols.ekwcTxId] ]
  , tableEntry maTxOutTableDef
      [ [maTxOutCols.mtocTxOutId] ]
  , tableEntry maTxMintTableDef
      [ [maTxMintCols.mtmcTxId] ]
  , tableEntry txMetadataTableDef
      [ [txMetadataCols.tmcTxId] ]
  , tableEntry txCborTableDef
      [ [txCborCols.tcbTxId] ]
  , tableEntry stakeRegistrationTableDef
      [ [stakeRegistrationCols.srcTxId]
      , [stakeRegistrationCols.srcAddrId]
      ]
  , tableEntry stakeDeregistrationTableDef
      [ [stakeDeregistrationCols.sdcTxId]
      , [stakeDeregistrationCols.sdcAddrId]
      , [stakeDeregistrationCols.sdcRedeemerId]
      ]
  , tableEntry delegationTableDef
      [ [delegationCols.dcTxId]
      , [delegationCols.dcAddrId]
      , [delegationCols.dcPoolHashId]
      , [delegationCols.dcRedeemerId]
      , [delegationCols.dcActiveEpochNo]
      ]
  , tableEntry withdrawalTableDef
      [ [withdrawalCols.wcAddrId]
      , [withdrawalCols.wcRedeemerId]
      ]
  , tableEntry poolUpdateTableDef
      [ [poolUpdateCols.pucRegisteredTxId]
      , [poolUpdateCols.pucHashId]
      , [poolUpdateCols.pucMetaId]
      , [poolUpdateCols.pucRewardAddrId]
      , [poolUpdateCols.pucActiveEpochNo]
      ]
  , tableEntry poolRetireTableDef
      [ [poolRetireCols.prcHashId]
      , [poolRetireCols.prcAnnouncedTxId]
      ]
  , tableEntry poolMetadataRefTableDef
      [ [poolMetadataRefCols.pmrcRegisteredTxId] ]
  , tableEntry poolRelayTableDef
      [ [poolRelayCols.prlcUpdateId] ]
  , tableEntry poolOwnerTableDef
      [ [poolOwnerCols.pocPoolUpdateId] ]
  , tableEntry reservedPoolTickerTableDef
      [ [reservedPoolTickerCols.rptcPoolHash] ]
  , tableEntry paramProposalTableDef
      [ [paramProposalCols.ppcCostModelId]
      , [paramProposalCols.ppcRegisteredTxId]
      ]
  , tableEntry epochParamTableDef
      [ [epochParamCols.epcCostModelId]
      , [epochParamCols.epcBlockId]
      ]
  , tableEntry epochStakeTableDef
      [ [epochStakeCols.escPoolId]
      , [epochStakeCols.escAddrId]
      ]
  , tableEntry rewardTableDef
      [ [rewardCols.rcPoolId]
      , [rewardCols.rcAddrId]
      , [rewardCols.rcEarnedEpoch]
      ]
  , tableEntry reserveTableDef
      [ [reserveCols.rscTxId]
      , [reserveCols.rscAddrId]
      ]
  , tableEntry treasuryTableDef
      [ [treasuryCols.trcTxId]
      , [treasuryCols.trcAddrId]
      ]
  , tableEntry drepDistrTableDef
      [ [drepDistrCols.ddcHashId, drepDistrCols.ddcEpochNo] ]
  ]
  where
    tableEntry td cols = (tdName td, cols)

-- | Built at the start of Ingest. Index names match what
-- 'tableIndexStatements' would emit later, so the schema-driven Prep
-- pass dedupes via @IF NOT EXISTS@.
--
-- Without these, every per-epoch resolve scans the full @tx_out@
-- heap — the visible cause of @awaitTxOutDrained (epoch N-1)@ stalls
-- late in Ingest.
ingestResolveIndexStatements :: [IndexStatement]
ingestResolveIndexStatements =
  [ -- 'bulkUpdateTxOutAddressIdsStmt' and 'bulkUpdateConsumedByTxIdStmt'
    -- match by 'tx_out.id' (PK). The worker counter assigns ids
    -- monotonically so the btree insert during COPY is right-edge.
    renderIndex NonConcurrent Unique
      (tdName txOutTableDef <> "_pkey_idx")
      (tdName txOutTableDef)
      [txOutCols.tocId.tcName]
    -- 'bulkUpdateCollateralTxOutAddressIdsStmt' matches by id.
  , renderIndex NonConcurrent Unique
      (tdName collateralTxOutTableDef <> "_pkey_idx")
      (tdName collateralTxOutTableDef)
      [collateralTxOutCols.ctocId.tcName]
    -- 'bulkSelectAddressIdsStmt' joins on 'address.raw_hash'
    -- (@GENERATED ALWAYS AS (md5(raw))@). @UNIQUE@ matches the Prep
    -- shape and gives the worker an index nested-loop instead of a
    -- full heap scan.
  , renderIndex NonConcurrent Unique
      (uniqueConstraintIndexName addressTableDef 1)
      (tdName addressTableDef)
      [addressCols.acRawHash.tcName]
  ]

-- | Indexes the CTAS resolve @LEFT JOIN@ depends on, plus indexes on
-- tables the CTAS does not rebuild (so they survive). Non-concurrent
-- because tables are still UNLOGGED: skips WAL and the second-pass
-- scan.
preResolveIndexStatements :: [IndexStatement]
preResolveIndexStatements =
  [ renderIndex NonConcurrent Unique
      (uniqueConstraintIndexName txTableDef 1)
      (tdName txTableDef)
      [txCols.tcHash.tcName]
  , renderIndex NonConcurrent Unique
      (uniqueConstraintIndexName txOutTableDef 1)
      (tdName txOutTableDef)
      [txOutCols.tocTxId.tcName, txOutCols.tocIndex.tcName]
  , renderIndex NonConcurrent NonUnique
      "collateral_tx_out_tx_id_idx"
      (tdName collateralTxOutTableDef)
      [collateralTxOutCols.ctocTxId.tcName]
  , renderIndex NonConcurrent NonUnique
      "withdrawal_tx_id_idx"
      (tdName withdrawalTableDef)
      [withdrawalCols.wcTxId.tcName]
  ]

-- | Built /after/ the CTAS rebuilds. The CTAS DROPs and replaces
-- @tx_in@ and @collateral_tx_in@, so any index on those tables built
-- earlier would be lost.
postResolveIndexStatements :: [IndexStatement]
postResolveIndexStatements =
  [ renderIndex NonConcurrent NonUnique
      "tx_in_tx_in_id_idx"
      (tdName txInTableDef)
      [txInCols.ticTxInId.tcName]
  , renderIndex NonConcurrent NonUnique
      "collateral_tx_in_tx_in_id_idx"
      (tdName collateralTxInTableDef)
      [collateralTxInCols.cticTxInId.tcName]
  ]

-- ---------------------------------------------------------------------------
-- * Scaffolding teardown
-- ---------------------------------------------------------------------------

-- | Every index name the ingest-time and pre/post-resolve passes may
-- have created. Dropped in one pass right before the UNLOGGED →
-- LOGGED flip so the flip rewrites bare heaps; the production index
-- pass afterwards rebuilds the shapes Follow needs (under the same
-- names) exactly once, with per-index pool fan-out and parallel
-- maintenance workers instead of serially inside each table's ALTER.
resolveScaffoldingIndexNames :: [Text]
resolveScaffoldingIndexNames =
  nub $ map isName $
    ingestResolveIndexStatements
      <> preResolveIndexStatements
      <> postResolveIndexStatements

dropIndexSql :: Text -> Text
dropIndexSql idxName = "DROP INDEX IF EXISTS " <> quoteIdent idxName

-- ---------------------------------------------------------------------------
-- * Internals
-- ---------------------------------------------------------------------------

-- | Concurrent builds are required when the table is LOGGED and
-- being written to. Non-concurrent avoids the second validation
-- scan and gets full @max_parallel_maintenance_workers@ — preferred
-- for UNLOGGED tables or LOGGED tables without concurrent writers.
data Concurrency = Concurrent | NonConcurrent

data Uniqueness  = Unique | NonUnique

renderIndex :: Concurrency -> Uniqueness -> Text -> Text -> [Text] -> IndexStatement
renderIndex conc uniq idxName tableName cols =
  IndexStatement idxName $ T.unwords $ filter (not . T.null)
    [ "CREATE"
    , case uniq of Unique -> "UNIQUE INDEX"; NonUnique -> "INDEX"
    , case conc of Concurrent -> "CONCURRENTLY"; NonConcurrent -> ""
    , "IF NOT EXISTS"
    , quoteIdent idxName
    , "ON"
    , quoteIdent tableName
    , "(" <> T.intercalate ", " (map quoteIdent cols) <> ")"
    ]
