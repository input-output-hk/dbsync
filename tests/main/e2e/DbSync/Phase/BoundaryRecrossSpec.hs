{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | End-to-end coverage for rolling back across an epoch boundary and
-- re-crossing it on the replacement fork.
--
-- The rewind must delete every epoch-keyed row above the fork point
-- (including the freshly finalized epoch), and the re-cross must
-- rebuild exactly one row-set per table — an insert without conflict
-- handling would abort the consumer with a unique violation, and a
-- rollback without epoch-keyed cleanup would leave stale rows whose
-- @block_id@ points at a deleted block.
module DbSync.Phase.BoundaryRecrossSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Ouroboros.Network.Block as Network

import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

import Ouroboros.Network.Block (data BlockPoint)

import DbSync.Db.Schema.AdaPots (adaPotsTableDef)
import DbSync.Db.Schema.Core (BlockCols (..), blockCols)
import DbSync.Db.Schema.EpochBoundary
  ( EpochParamCols (..)
  , epochParamCols
  , epochParamTableDef
  , epochStateTableDef
  )
import DbSync.Db.Schema.EpochView (EpochFinalizedCols (..), epochFinalizedCols)
import DbSync.Db.Schema.Types (TableColumn (..), TableDef (..))
import DbSync.Test.AppHarness
  ( ledgerEnabledTestConfig
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.Database (queryTestDb)
import DbSync.Test.E2E (conwayConfigDir, withAppSession)
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockNode
  ( currentTip
  , forgeAndPushBlocks
  , forgeAndPushUntilNextEpoch
  , rollbackMockNode
  , withMockNode
  )
import DbSync.Test.PgAssertions (countRows)
import DbSync.Test.RecomputeInvariants
  ( blockTxCountDriftCount
  , consumedByDriftCount
  , duplicateEpochRowGroupCount
  , epochContiguityGapCount
  , epochFinalizedDriftCount
  )

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Boundary rollback-recross" $
  it "re-crossing a rolled-back boundary rebuilds each epoch-keyed row exactly once" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-boundary-recross" $ \ledgerDir -> do
        tracer <- quietTracer
        _ <- forgeAndPushBlocks mn 250
        withAppSession tracer ledgerEnabledTestConfig mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          baselineBlocks <- countRows (tdName blockTableDef)
          baselineAdaPots <- countRows (tdName adaPotsTableDef)
          baselineEpochParam <- countRows (tdName epochParamTableDef)
          baselineEpochState <- countRows (tdName epochStateTableDef)
          baselineFinalized <- countRows (tdName finalizedTableDef)

          -- First crossing: the live Follow consumer lands the boundary
          -- writes and finalizes the epoch it just left.
          tipBefore <- currentTip mn
          crossing1 <- forgeAndPushUntilNextEpoch mn
          let blocksAfterCross = baselineBlocks + length crossing1
          waitFor "boundary writes land after first crossing"
            (do blks <- countRows (tdName blockTableDef)
                pots <- countRows (tdName adaPotsTableDef)
                pure (blks == blocksAfterCross && pots > baselineAdaPots))
            60

          afterCrossAdaPots <- countRows (tdName adaPotsTableDef)
          afterCrossEpochParam <- countRows (tdName epochParamTableDef)
          afterCrossEpochState <- countRows (tdName epochStateTableDef)
          afterCrossFinalized <- countRows (tdName finalizedTableDef)
          afterCrossFinalized `shouldBe` baselineFinalized + 1
          crossedEpoch <- maxBlockEpoch

          -- Roll back only the crossing block: the fork point is the
          -- last block of the previous epoch.
          forkPoint <- case reverse crossing1 of
            _crossing : lastOldEpoch : _ -> pure (Network.blockPoint lastOldEpoch)
            _ -> case tipBefore of
              Network.Tip slot hash _bn -> pure (BlockPoint slot hash)
              Network.TipGenesis -> panic "recross scenario: tip at genesis"
          rollbackMockNode mn forkPoint

          -- The rewind deletes the crossing block, the boundary rows,
          -- and the finalized row of the re-opened epoch.
          waitFor "rollback rewinds the crossing block and its boundary rows"
            (do blks <- countRows (tdName blockTableDef)
                pots <- countRows (tdName adaPotsTableDef)
                pure (blks == blocksAfterCross - 1 && pots == baselineAdaPots))
            60
          countRows (tdName epochParamTableDef) `shouldReturn` baselineEpochParam
          countRows (tdName epochStateTableDef) `shouldReturn` baselineEpochState
          countRows (tdName finalizedTableDef) `shouldReturn` baselineFinalized

          -- Re-cross on the replacement fork; the consumer must survive
          -- the second round of boundary writes for the same epoch.
          crossing2 <- forgeAndPushUntilNextEpoch mn
          let blocksAfterRecross = blocksAfterCross - 1 + length crossing2
          waitFor "boundary writes land again after the re-cross"
            (do blks <- countRows (tdName blockTableDef)
                pots <- countRows (tdName adaPotsTableDef)
                pure (blks == blocksAfterRecross && pots == afterCrossAdaPots))
            60

          countRows (tdName epochParamTableDef) `shouldReturn` afterCrossEpochParam
          countRows (tdName epochStateTableDef) `shouldReturn` afterCrossEpochState
          countRows (tdName finalizedTableDef) `shouldReturn` afterCrossFinalized
          maxBlockEpoch `shouldReturn` crossedEpoch

          -- The re-finalized epoch holds exactly one row, and the
          -- re-crossed epoch_param row points at a live block.
          finalizedRowsFor (crossedEpoch - 1) `shouldReturn` 1
          danglingEpochParamBlockIds `shouldReturn` 0

          duplicateEpochRowGroupCount `shouldReturn` 0
          epochFinalizedDriftCount `shouldReturn` 0
          blockTxCountDriftCount `shouldReturn` 0
          epochContiguityGapCount `shouldReturn` 0
          consumedByDriftCount `shouldReturn` 0

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

blockTableDef :: TableDef
blockTableDef = tcTable blockCols.bcId

finalizedTableDef :: TableDef
finalizedTableDef = tcTable epochFinalizedCols.efcNo

maxBlockEpoch :: IO Int
maxBlockEpoch =
  readCount $
    T.unwords
      [ "SELECT MAX(" <> blockCols.bcEpochNo.tcName <> ")"
      , "FROM " <> tdName blockTableDef
      , "WHERE " <> blockCols.bcEpochNo.tcName <> " IS NOT NULL"
      ]

finalizedRowsFor :: Int -> IO Int
finalizedRowsFor no =
  readCount $
    T.unwords
      [ "SELECT COUNT(*) FROM " <> tdName finalizedTableDef
      , "WHERE " <> epochFinalizedCols.efcNo.tcName <> " = " <> show no
      ]

-- | @epoch_param@ rows whose @block_id@ no longer resolves to a block —
-- the footprint of a boundary row that survived a rollback of its
-- crossing block.
danglingEpochParamBlockIds :: IO Int
danglingEpochParamBlockIds =
  readCount $
    T.unwords
      [ "SELECT COUNT(*) FROM " <> tdName epochParamTableDef <> " ep"
      , "LEFT JOIN " <> tdName blockTableDef <> " b"
      , "  ON b." <> blockCols.bcId.tcName <> " = ep." <> epochParamCols.epcBlockId.tcName
      , "WHERE ep." <> epochParamCols.epcBlockId.tcName <> " IS NOT NULL"
      , "  AND b." <> blockCols.bcId.tcName <> " IS NULL"
      ]

readCount :: Text -> IO Int
readCount q = do
  t <- T.strip <$> queryTestDb q
  pure (fromMaybe (-1) (readMaybe (T.unpack t)))
