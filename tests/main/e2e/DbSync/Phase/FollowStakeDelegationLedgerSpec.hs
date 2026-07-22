{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Follow-phase coverage of the @stake_delegation_ledger@ boundary
-- handler.
--
-- Two @it@ blocks against 'conwayRewardsConfigDir' (rho = 0.03):
--
--   * Reward path — 250 Ingest blocks, then two epoch boundaries in
--     Follow cross the point where 'LedgerTotalRewards' fires for the
--     genesis-registered pools, growing the @reward@ table.
--   * 'pot_reward' path — bootstrap governance (stake creds, DRep,
--     committee), donate to treasury, propose+vote a treasury
--     withdrawal, then advance three epoch boundaries: votes
--     finalise on the first, ratification on the second, and
--     'LedgerGovInfo' fires with a non-empty @garMTreasury@ on the
--     third, growing the @pot_reward@ table. The DRep vote-delegation
--     also lands @drep_distr@ rows once the pulsing snapshot settles.
module DbSync.Phase.FollowStakeDelegationLedgerSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T

import Cardano.Ledger.Address (AccountAddress (..), AccountId (..))
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Conway.Governance as Governance
import Cardano.Ledger.Conway.Tx (AlonzoTx (..), Tx (..))
import qualified Cardano.Ledger.Core as Core
import Ouroboros.Consensus.Shelley.Eras (ConwayEra)

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Cardano.Mock.Forging.Interpreter as MockInt
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import qualified Cardano.Mock.Forging.Tx.Generic as Generic
import qualified Cardano.Mock.Forging.Types as Mock

import DbSync.App.Config.Types
  ( SyncConfig (..)
  , OptionFlag (..)
  , DbSyncOptions (..)
  )
import DbSync.Db.Schema.Core (blockTableDef)
import DbSync.Db.Schema.Governance (drepDistrTableDef)
import DbSync.Db.Schema.StakeDelegation
  ( epochStakeProgressTableDef
  , epochStakeTableDef
  , potRewardTableDef
  , rewardTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Test.AppHarness
  ( ledgerEnabledTestProfile
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.Database (queryTestDb)
import DbSync.Test.E2E
  ( conwayRewardsConfigDir
  , withAppSession
  )
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockChain (MockChain (..))
import DbSync.Test.MockNode
  ( MockNode (..)
  , forgeAndPush
  , forgeAndPushBlocks
  , forgeAndPushCommitteeCreds
  , forgeAndPushDRepsAndDelegateVotes
  , forgeAndPushUntilNextEpoch
  , forgeAndPushWithStakeCreds
  , withMockNode
  )
import DbSync.Test.PgAssertions (countRows)

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Follow stake_delegation_ledger writes" $ do
  it "lands reward + epoch_stake rows after Follow crosses an epoch boundary" $
    withMockNode conwayRewardsConfigDir $ \mn ->
      withTempDir "dbsync-test-follow-sdl-reward" $ \ledgerDir -> do
        tracer <- quietTracer
        -- 250 blocks settles Ingest past two boundaries (0->1, 1->2).
        -- Block-production rewards for epoch 0 are paid at the
        -- 2->3 crossing, which Follow drives below.
        _ <- forgeAndPushBlocks mn 250

        withAppSession tracer rewardProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          baselineBlocks   <- countRows (tdName blockTableDef)
          baselineStake    <- countRows (tdName epochStakeTableDef)
          baselineProgress <- countRows (tdName epochStakeProgressTableDef)
          baselineReward   <- countRows (tdName rewardTableDef)

          -- Cross two more boundaries: the first surfaces the
          -- epoch-2 boundary writes in Follow; the second hits the
          -- 2->3 boundary where rewards from epoch-0 production are
          -- paid out.
          followBlocks1 <- forgeAndPushUntilNextEpoch mn
          followBlocks2 <- forgeAndPushUntilNextEpoch mn
          let expectedBlocks = baselineBlocks + length followBlocks1 + length followBlocks2
          waitFor
            (tdName blockTableDef <> " count reaches " <> show expectedBlocks)
            (do n <- countRows (tdName blockTableDef); pure (n >= expectedBlocks))
            120

          -- reward, epoch_stake and epoch_stake_progress each grow as
          -- Follow drains the epoch-2/epoch-3 snapshots and pays the
          -- epoch-0 production rewards. Exact counts are genesis-config
          -- dependent (one row per earning cred x leader/member split),
          -- so pin the row content instead.
          waitFor
            (tdName rewardTableDef <> " count grows after Follow boundary")
            (do n <- countRows (tdName rewardTableDef); pure (n > baselineReward))
            60
          waitFor
            (tdName epochStakeTableDef <> " count grows after Follow boundary")
            (do n <- countRows (tdName epochStakeTableDef); pure (n > baselineStake))
            60
          waitFor
            (tdName epochStakeProgressTableDef <> " count grows after Follow boundary")
            (do n <- countRows (tdName epochStakeProgressTableDef); pure (n > baselineProgress))
            60

          -- The newest reward row is block-production content: a
          -- positive amount tagged leader or member.
          rewardAmountPos <- T.strip <$> queryTestDb
            ( "SELECT (amount > 0)::text FROM " <> tdName rewardTableDef
                <> " ORDER BY id DESC LIMIT 1" )
          rewardAmountPos `shouldBe` "true"
          rewardTyp <- T.strip <$> queryTestDb
            ( "SELECT type FROM " <> tdName rewardTableDef
                <> " ORDER BY id DESC LIMIT 1" )
          (rewardTyp `elem` ["leader", "member"]) `shouldBe` True

          -- The newest epoch_stake row is a positive delegation; the
          -- newest epoch_stake_progress row marks a completed snapshot.
          stakeAmountPos <- T.strip <$> queryTestDb
            ( "SELECT (amount > 0)::text FROM " <> tdName epochStakeTableDef
                <> " ORDER BY id DESC LIMIT 1" )
          stakeAmountPos `shouldBe` "true"
          progressCompleted <- T.strip <$> queryTestDb
            ( "SELECT completed::text FROM " <> tdName epochStakeProgressTableDef
                <> " ORDER BY id DESC LIMIT 1" )
          progressCompleted `shouldBe` "true"

  it "lands pot_reward after a Conway treasury withdrawal enacts at tip" $
    withMockNode conwayRewardsConfigDir $ \mn ->
      withTempDir "dbsync-test-follow-sdl-treasury" $ \ledgerDir -> do
        tracer <- quietTracer
        _ <- forgeAndPushBlocks mn 250

        withAppSession tracer treasuryProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          baselinePotReward <- countRows (tdName potRewardTableDef)
          baselineDrepDistr <- countRows (tdName drepDistrTableDef)

          -- Bootstrap governance: stake creds, DRep + delegation,
          -- one epoch for DRep distribution to settle, committee
          -- hot-key authorization.
          _ <- forgeAndPushWithStakeCreds mn
          _ <- forgeAndPushDRepsAndDelegateVotes mn
          _ <- forgeAndPushUntilNextEpoch mn
          _ <- forgeAndPushCommitteeCreds mn

          -- Fund the treasury so the withdrawal has something to pay.
          _ <- forgeAndPush mn [Mock.TxConway (Conway.mkDonationTx (Coin 50_000))]

          -- One block carrying the withdrawal proposal + a yes-vote
          -- from every registered DRep and committee voter.
          proposalAndVote <- buildTreasuryWithdrawalTxs mn
          _ <- forgeAndPush mn proposalAndVote

          -- Three epoch boundaries: votes finalised on the first,
          -- ratification on the second, enactment on the third
          -- (where @LedgerGovInfo enacted@ fires with a non-empty
          -- @garMTreasury@).
          _ <- forgeAndPushUntilNextEpoch mn
          _ <- forgeAndPushUntilNextEpoch mn
          _ <- forgeAndPushUntilNextEpoch mn

          waitFor
            (tdName potRewardTableDef <> " count grows after treasury enactment")
            (do n <- countRows (tdName potRewardTableDef)
                pure (n > baselinePotReward))
            60

          -- Conway has no MIR path, so the enacted withdrawal is the
          -- only pot_reward source: exactly one row, tagged treasury,
          -- for the 10_000 lovelace paid out.
          followPotReward <- countRows (tdName potRewardTableDef)
          (followPotReward - baselinePotReward) `shouldBe` 1

          mostRecentType <- T.strip <$> queryTestDb
            ( "SELECT type FROM " <> tdName potRewardTableDef
                <> " ORDER BY id DESC LIMIT 1" )
          mostRecentType `shouldBe` "treasury"
          mostRecentAmount <- T.strip <$> queryTestDb
            ( "SELECT (amount = 10000)::text FROM " <> tdName potRewardTableDef
                <> " ORDER BY id DESC LIMIT 1" )
          mostRecentAmount `shouldBe` "true"

          -- The DRep vote-delegation from bootstrap surfaces in the
          -- pulsing snapshot the ledger finalises at each boundary:
          -- drep_distr grows and at least one entry carries real
          -- delegated stake.
          waitFor
            (tdName drepDistrTableDef <> " count grows after DRep delegation settles")
            (do n <- countRows (tdName drepDistrTableDef)
                pure (n > baselineDrepDistr))
            60
          drepDistrHasStake <- T.strip <$> queryTestDb
            ( "SELECT (max(amount) > 0)::text FROM " <> tdName drepDistrTableDef )
          drepDistrHasStake `shouldBe` "true"

-- ---------------------------------------------------------------------------
-- * Profiles
-- ---------------------------------------------------------------------------

-- | Ledger on, @stake_delegation_ledger@ on. Enough for the reward
-- assertion: no governance txs are submitted in that test.
rewardProfile :: SyncConfig
rewardProfile = ledgerEnabledTestProfile
  { scOptions = (scOptions ledgerEnabledTestProfile)
      { pcStakeDelegationLedger = OptionFlag True
      }
  }

-- | Ledger on, @stake_delegation_ledger@ on, @governance@ on so the
-- proposal and vote txs land their per-block rows.
treasuryProfile :: SyncConfig
treasuryProfile = ledgerEnabledTestProfile
  { scOptions = (scOptions ledgerEnabledTestProfile)
      { pcStakeDelegationLedger = OptionFlag True
      , pcGovernance            = OptionFlag True
      }
  }

-- ---------------------------------------------------------------------------
-- * Tx builders
-- ---------------------------------------------------------------------------

-- | Build the (proposal, vote) tx pair against the interpreter's
-- current ledger state, picking the first genesis-registered stake
-- credential as the withdrawal target. The vote tx references the
-- proposal tx's id directly so both can land in the same block.
buildTreasuryWithdrawalTxs :: MockNode -> IO [Mock.TxEra]
buildTreasuryWithdrawalTxs mn =
  MockInt.withConwayLedgerState (mcInterpreter (mnChain mn)) $ \ledger -> do
    rewardCred <- Generic.resolveStakeCreds (Mock.StakeIndex 0) ledger
    let rewardAccount = AccountAddress Testnet (AccountId rewardCred)
        proposalTx    = Conway.mkTreasuryWithdrawalTx rewardAccount (Coin 10_000)
        govActionId   = govActionIdFor proposalTx 0
        voteTx        = Conway.mkGovVoteYesTx govActionId
                          (Generic.drepVoters ++ Generic.committeeVoters)
    pure [Mock.TxConway proposalTx, Mock.TxConway voteTx]

-- | Compute the 'GovActionId' for a proposal carried by the given tx
-- at the given @OSet@ index. 'mkTreasuryWithdrawalTx' puts exactly
-- one proposal at index 0.
govActionIdFor
  :: Core.Tx Core.TopTx ConwayEra -> Word16 -> Governance.GovActionId
govActionIdFor (MkConwayTx atx) ix =
  Governance.GovActionId (Core.txIdTxBody (atBody atx)) (Governance.GovActionIx ix)
