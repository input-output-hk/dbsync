{-# LANGUAGE OverloadedStrings #-}

-- | Coverage for the Conway-genesis governance snapshot and the
-- Conway-only @epoch_state@ gate.
--
--   * config-conway boots with a 4-member genesis committee and a
--     genesis constitution. The first epoch boundary crossed during
--     Ingest must seed both as NULL-proposal rows, and every
--     @epoch_state@ row must carry the genesis @committee_id@.
--   * A pre-Conway (Alonzo) chain has no governance state, so
--     @epoch_state@ stays empty even though @epoch_param@ is written
--     at every boundary.
module DbSync.Phase.GovernanceGenesisSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.App.Config.Types (DbProfile (..), OptionFlag (..), SyncConfig (..))
import DbSync.Db.Schema.EpochBoundary (epochParamTableDef, epochStateTableDef)
import DbSync.Db.Schema.Governance
  ( committeeMemberTableDef
  , committeeTableDef
  , constitutionTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Test.AppHarness
  ( ledgerEnabledTestConfig
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.Database (queryTestDb)
import DbSync.Test.E2E (alonzoConfigDir, conwayConfigDir, withAppSession)
import DbSync.Test.MockNode (forgeAndPushBlocks, withMockNode)
import DbSync.Test.PgAssertions (countRows)

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Governance genesis snapshot" $ do
  it "seeds the genesis committee, members and constitution during Conway ingest" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-gov-genesis" $ \ledgerDir -> do
        tracer <- quietTracer
        _ <- forgeAndPushBlocks mn 250
        withAppSession tracer governanceTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          genCommittee <-
            countWhere (tdName committeeTableDef) "gov_action_proposal_id IS NULL"
          genCommittee `shouldBe` "1"

          genMembers <-
            T.strip <$> queryTestDb
              ( "SELECT count(*) FROM " <> tdName committeeMemberTableDef
                  <> " WHERE committee_id = (SELECT id FROM "
                  <> tdName committeeTableDef
                  <> " WHERE gov_action_proposal_id IS NULL)"
              )
          genMembers `shouldBe` "4"

          genConstitution <-
            countWhere (tdName constitutionTableDef) "gov_action_proposal_id IS NULL"
          genConstitution `shouldBe` "1"

          -- Every Conway epoch_state row resolves the genesis committee.
          unpopulated <- countWhere (tdName epochStateTableDef) "committee_id IS NULL"
          unpopulated `shouldBe` "0"

          epochStates <- countRows (tdName epochStateTableDef)
          epochStates `shouldSatisfy` (>= 1)

  it "writes no epoch_state rows for pre-Conway epochs" $
    withMockNode alonzoConfigDir $ \mn ->
      withTempDir "dbsync-test-gov-preconway" $ \ledgerDir -> do
        tracer <- quietTracer
        _ <- forgeAndPushBlocks mn 250
        withAppSession tracer governanceTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          -- epoch_param is written at every boundary, so boundaries were
          -- crossed; epoch_state, gated on Conway gov state, stays empty.
          epochParams <- countRows (tdName epochParamTableDef)
          epochParams `shouldSatisfy` (>= 1)

          epochStates <- countRows (tdName epochStateTableDef)
          epochStates `shouldBe` 0

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | 'ledgerEnabledTestConfig' with @pcGovernance@ flipped on so the
-- governance boundary (and its genesis seeding) runs.
governanceTestProfile :: SyncConfig
governanceTestProfile =
  ledgerEnabledTestConfig
    { scDbProfile = (scDbProfile ledgerEnabledTestConfig)
        { pcGovernance = OptionFlag True
        }
    }

countWhere :: Text -> Text -> IO Text
countWhere table predicate =
  T.strip <$> queryTestDb ("SELECT count(*) FROM " <> table <> " WHERE " <> predicate)
