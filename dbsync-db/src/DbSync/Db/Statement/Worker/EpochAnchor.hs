{-# LANGUAGE OverloadedStrings #-}

-- | Trimming rules for the epoch-keyed boundary tables.
--
-- Their rows describe an epoch rather than a block, so a cleanup can
-- only scope them by epoch: the rollback cascade against the target
-- block's epoch, the resume cleanup against the cutoff slot's. How
-- much of that epoch is still valid depends on which block wrote the
-- row, which is what 'EpochAnchor' records.
module DbSync.Db.Statement.Worker.EpochAnchor
  ( EpochAnchor (..)
  , epochKeyedColumns
  , epochAnchorFor
  , deleteEpochRowsStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.AdaPots (AdaPotsCols (..), adaPotsCols)
import DbSync.Db.Schema.EpochBoundary
  ( EpochParamCols (..)
  , EpochStateCols (..)
  , epochParamCols
  , epochStateCols
  )
import DbSync.Db.Schema.EpochSyncStats (EpochSyncStatsCols (..), epochSyncStatsCols)
import DbSync.Db.Schema.EpochView (EpochFinalizedCols (..), epochFinalizedCols)
import DbSync.Db.Schema.Governance (DrepDistrCols (..), drepDistrCols)
import DbSync.Db.Schema.Pool (PoolStatCols (..), poolStatCols)
import DbSync.Db.Schema.StakeDelegation
  ( EpochStakeCols (..)
  , EpochStakeProgressCols (..)
  , PotRewardCols (..)
  , RewardCols (..)
  , epochStakeCols
  , epochStakeProgressCols
  , potRewardCols
  , rewardCols
  )
import DbSync.Db.Schema.Types (TableColumn (..), TableDef (..))
import DbSync.Db.Sql.Refs (col, table)

-- ---------------------------------------------------------------------------
-- * Anchors
-- ---------------------------------------------------------------------------

-- | Which blocks produce an epoch-keyed table's rows, and therefore
-- how far back a cleanup anchored at epoch @E@ has to trim.
--
--   * 'EnteredEpoch' — written by the first block of the epoch they
--     carry, so epoch @E@'s own rows predate the anchor block and
--     survive.
--   * 'NextEpochSnapshot' — stake slices of the /next/ epoch's mark
--     snapshot, spread across the blocks of the epoch before it. The
--     snapshot for @E + 1@ froze before the anchor block, so those
--     rows survive as well.
--   * 'CompletedEpoch' — describe a finished epoch, written by the
--     first block of the following one. Anchoring inside epoch @E@
--     un-finishes it, so its row goes.
data EpochAnchor
  = EnteredEpoch
  | NextEpochSnapshot
  | CompletedEpoch
  deriving stock (Eq, Show)

-- | Every epoch-keyed table, paired with the column carrying the
-- epoch it is trimmed on and that column's anchor.
epochKeyedColumns :: [(TableColumn, EpochAnchor)]
epochKeyedColumns =
  [ (epochStateCols.esccEpochNo,         EnteredEpoch)
  , (epochParamCols.epcEpochNo,          EnteredEpoch)
  , (adaPotsCols.apcEpochNo,             EnteredEpoch)
  , (poolStatCols.pstcEpochNo,           EnteredEpoch)
  , (drepDistrCols.ddcEpochNo,           EnteredEpoch)
    -- reward rows land entering their spendable epoch; pot_reward
    -- rows land entering their earned epoch (spendable is one later).
  , (rewardCols.rcSpendableEpoch,        EnteredEpoch)
  , (potRewardCols.prcEarnedEpoch,       EnteredEpoch)
  , (epochStakeCols.escEpochNo,          NextEpochSnapshot)
  , (epochStakeProgressCols.espcEpochNo, NextEpochSnapshot)
  , (epochSyncStatsCols.esscEpochNo,     CompletedEpoch)
  , (epochFinalizedCols.efcNo,           CompletedEpoch)
  ]

epochAnchorFor :: Text -> Maybe (TableColumn, EpochAnchor)
epochAnchorFor tableName =
  find ((== tableName) . tdName . tcTable . fst) epochKeyedColumns

-- ---------------------------------------------------------------------------
-- * SQL builders
-- ---------------------------------------------------------------------------

-- | @DELETE FROM \<table\> WHERE \<column\> \>= $1@. Takes the anchor
-- epoch; the anchor shifts it up to the first epoch that has to go.
deleteEpochRowsStmt :: EpochAnchor -> TableColumn -> Stmt.Statement Word64 Int64
deleteEpochRowsStmt anchor c =
  Stmt.unpreparable sql encoder D.rowsAffected
  where
    -- Epochs the anchor lets the anchor epoch keep, itself included.
    kept :: Word64
    kept = case anchor of
      EnteredEpoch      -> 1
      NextEpochSnapshot -> 2
      CompletedEpoch    -> 0
    sql = T.concat
      [ "DELETE FROM ", table (tcTable c)
      , " WHERE ", col c, " >= $1"
      ]
    encoder =
      (fromIntegral :: Word64 -> Int64) . (+ kept)
        >$< E.param (E.nonNullable E.int8)
