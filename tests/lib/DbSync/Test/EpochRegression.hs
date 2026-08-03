{-# LANGUAGE OverloadedStrings #-}

-- | Per-epoch row counts for the epoch-keyed tables, captured either
-- side of a restart.
--
-- These tables carry no slot, block or tx anchor, so a resume can only
-- scope them by epoch. Blocks at or below @last_committed_slot@ are
-- never re-processed, so any epoch the cleanup trims too eagerly stays
-- short for the life of the database.
module DbSync.Test.EpochRegression
  ( EpochSnapshot
  , snapshotEpochKeyedCounts
  , epochRegressions
  , completedStakeEpochs
  ) where

import Cardano.Prelude

import Data.List (lookup)
import qualified Data.Text as T

import DbSync.Db.Schema.StakeDelegation
  ( EpochStakeProgressCols (..)
  , epochStakeProgressCols
  , epochStakeProgressTableDef
  )
import DbSync.Db.Schema.Types (TableColumn (..), TableDef (..))
import DbSync.Db.Statement.Worker.EpochAnchor (epochKeyedColumns)
import DbSync.Test.Database (queryTestDb)

-- | Row counts per epoch, per table. Only tables present in PG appear;
-- a disabled extractor's tables are never created.
type EpochSnapshot = [(Text, [(Word64, Int)])]

-- | Count rows per epoch for every table the production epoch cleanup
-- scopes. Driven off 'epochKeyedColumns' so a newly registered
-- epoch-keyed table is covered without editing this module.
snapshotEpochKeyedCounts :: IO EpochSnapshot
snapshotEpochKeyedCounts =
  fmap catMaybes . forM (filter (not . excluded) epochKeyedColumns) $ \(c, _) -> do
    let tbl = tdName (tcTable c)
    present <- tableExists tbl
    if present
      then Just . (,) tbl <$> countsByEpoch tbl (tcName c)
      else pure Nothing
  where
    excluded (c, _) = tdName (tcTable c) `elem` acceptedLosses

-- | Tables whose newest row an @IngestResume@ may drop for good.
--
-- A boundary snapshots the id counters into @sync_state@ before it
-- writes the @epoch_sync_stats@ row reporting the epoch that just
-- ended, so that row's id is always above the recorded
-- @epoch_sync_stats_id_counter@ and the counter pass deletes it. The
-- pass has to: the same counter resets the id allocator on resume, so
-- a surviving row would collide with the next insert.
--
-- The resumed sync rewrites the row when it re-crosses that epoch's
-- boundary, so the loss only sticks when the cutoff epoch is the last
-- one the chain reaches — which is where the kill lands often enough
-- to make an assertion on this table flaky. It is sync telemetry, not
-- chain data.
acceptedLosses :: [Text]
acceptedLosses = ["epoch_sync_stats"]

-- | Epochs whose count dropped, as @(table, epoch, before, after)@.
-- Epochs gained between snapshots are ignored — a restarted sync
-- legitimately advances.
epochRegressions :: EpochSnapshot -> EpochSnapshot -> [(Text, Word64, Int, Int)]
epochRegressions before after =
  [ (tbl, epoch, n, m)
  | (tbl, pre) <- before
  , (epoch, n) <- pre
  , let m = fromMaybe 0 (lookup epoch (fromMaybe [] (lookup tbl after)))
  , m < n
  ]

-- | Epochs carrying a completed @epoch_stake_progress@ row. No
-- production code reads that column, so only tests pin it.
completedStakeEpochs :: IO [Word64]
completedStakeEpochs = do
  raw <- queryTestDb $ T.concat
    [ "SELECT ", tcName (espcEpochNo epochStakeProgressCols)
    , " FROM ", tdName epochStakeProgressTableDef
    , " WHERE ", tcName (espcCompleted epochStakeProgressCols)
    , " ORDER BY 1;"
    ]
  pure (mapMaybe parseWord (cells raw))

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

countsByEpoch :: Text -> Text -> IO [(Word64, Int)]
countsByEpoch table epochCol = do
  raw <- queryTestDb $ T.concat
    [ "SELECT ", epochCol, ", count(*) FROM ", table
    , " GROUP BY 1 ORDER BY 1;"
    ]
  pure (mapMaybe parseRow (cells raw))
  where
    parseRow line = case T.splitOn "|" line of
      [e, n] -> (,) <$> parseWord e <*> parseInt n
      _      -> Nothing

-- | 'False' when the table is absent, which is how a disabled
-- extractor's tables present themselves.
tableExists :: Text -> IO Bool
tableExists table = do
  raw <- queryTestDb $ T.concat
    [ "SELECT to_regclass('public.", table, "') IS NOT NULL;" ]
  pure (any ((== "t") . T.strip) (cells raw))

-- | Non-blank lines of a @-t -A@ psql result.
cells :: Text -> [Text]
cells = filter (not . T.null) . map T.strip . T.lines

parseWord :: Text -> Maybe Word64
parseWord = readMaybe . T.unpack

parseInt :: Text -> Maybe Int
parseInt = readMaybe . T.unpack
