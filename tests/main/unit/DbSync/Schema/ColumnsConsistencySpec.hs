{-# LANGUAGE OverloadedStrings #-}

-- | One assertion per per-table column record: the record's field
-- values match the column names declared on the matching 'TableDef'.
module DbSync.Schema.ColumnsConsistencySpec (spec) where

import Cardano.Prelude

import qualified Data.List as List
import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified DbSync.Db.Schema.AdaPots as AdaPots
import qualified DbSync.Db.Schema.Address as Address
import qualified DbSync.Db.Schema.CBOR as CBOR
import qualified DbSync.Db.Schema.Core as Core
import qualified DbSync.Db.Schema.EpochBoundary as EpochBoundary
import qualified DbSync.Db.Schema.EpochParamPending as EpochParamPending
import qualified DbSync.Db.Schema.EpochSyncStats as EpochSyncStats
import qualified DbSync.Db.Schema.EpochView as EpochView
import qualified DbSync.Db.Schema.Governance as Governance
import qualified DbSync.Db.Schema.Metadata as Metadata
import qualified DbSync.Db.Schema.MultiAsset as MultiAsset
import qualified DbSync.Db.Schema.OffChainPool as OffChainPool
import qualified DbSync.Db.Schema.OffChainVote as OffChainVote
import qualified DbSync.Db.Schema.Pool as Pool
import qualified DbSync.Db.Schema.ScriptsDatums as ScriptsDatums
import qualified DbSync.Db.Schema.StakeDelegation as StakeDelegation
import qualified DbSync.Db.Schema.SyncState as SyncState
import qualified DbSync.Db.Schema.UTxO as UTxO
import DbSync.Db.Schema.Types (ColumnDef (..), TableColumn (..), TableDef (..))

-- | One concat entry per migrated schema module.
allColumnRecords :: [(TableDef, [TableColumn])]
allColumnRecords = concat
  [ AdaPots.adaPotsColumnRecords
  , Address.addressColumnRecords
  , CBOR.cborColumnRecords
  , Core.coreColumnRecords
  , EpochBoundary.epochBoundaryColumnRecords
  , EpochParamPending.epochParamPendingColumnRecords
  , EpochSyncStats.epochSyncStatsColumnRecords
  , EpochView.epochViewColumnRecords
  , Governance.governanceColumnRecords
  , Metadata.metadataColumnRecords
  , MultiAsset.multiAssetColumnRecords
  , OffChainPool.offChainPoolColumnRecords
  , OffChainVote.offChainVoteColumnRecords
  , Pool.poolColumnRecords
  , ScriptsDatums.scriptsDatumsColumnRecords
  , StakeDelegation.stakeDelegationColumnRecords
  , SyncState.syncStateColumnRecords
  , UTxO.utxoColumnRecords
  ]

spec :: Spec
spec = describe "Column records match TableDef columns" $
  forM_ allColumnRecords $ \(td, cols) ->
    it (T.unpack (tdName td)) $
      List.sort (map tcName cols) `shouldBe` List.sort (map cdName (tdColumns td))
