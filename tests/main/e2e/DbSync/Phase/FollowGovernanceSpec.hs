{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Follow-phase coverage of the @governance@ extractor across all
-- three sub-passes (certs, proposals, votes).
--
-- Five Follow-window blocks at tip:
--
--   * Block A — DRep registration cert
--                 → @drep_hash@ + @drep_registration@
--   * Block B — Committee hot-key authorization cert
--                 → @committee_hash@ (cold + hot) + @committee_registration@
--   * Block C — 'ParameterChange' proposal
--                 → @gov_action_proposal@ + @param_proposal@ + @voting_anchor@
--   * Block D — 'TreasuryWithdrawals' proposal
--                 → @gov_action_proposal@ + @treasury_withdrawal@
--   * Block E — DRep vote on Block C's proposal
--                 → @voting_procedure@ (exercises the cross-block proposal cache)
--
-- The @NewConstitution@ / @UpdateCommittee@ / @HardForkInitiation@ /
-- @NoConfidence@ / @InfoAction@ proposal arms share the same plumbing
-- as @ParameterChange@; the @DRep deregistration@ / @committee resign@
-- cert arms share the same plumbing as the two cert arms covered here.
-- Unit-level coverage of the unexercised arms lives in
-- 'DbSync.Extractor.GovernanceSpec'.
module DbSync.Phase.FollowGovernanceSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T

import Cardano.Ledger.Address (AccountAddress (..), AccountId (..))
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Conway.Governance as Governance
import Cardano.Ledger.Conway.Tx (AlonzoTx (..), Tx (..))
import qualified Cardano.Ledger.Core as Core
import Ouroboros.Consensus.Shelley.Eras (ConwayEra)

import Test.Hspec (Spec, describe, it, shouldSatisfy)

import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import qualified Cardano.Mock.Forging.Tx.Generic as Generic
import qualified Cardano.Mock.Forging.Types as Mock

import DbSync.App.Config.Types
  ( SyncConfig (..)
  , SyncOption (..)
  , SyncOptions (..)
  )
import DbSync.Db.Schema.Core (blockTableDef)
import DbSync.Db.Schema.Governance
  ( committeeHashTableDef
  , committeeRegistrationTableDef
  , drepHashTableDef
  , drepRegistrationTableDef
  , govActionProposalTableDef
  , paramProposalTableDef
  , treasuryWithdrawalTableDef
  , votingAnchorTableDef
  , votingProcedureTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Test.AppHarness
  ( ledgerEnabledTestProfile
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.Database (queryTestDb)
import DbSync.Test.E2E (conwayConfigDir, withAppSession)
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockNode
  ( forgeAndPush
  , forgeAndPushBlocks
  , withMockNode
  )
import DbSync.Test.PgAssertions (countRows)

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Follow governance writes" $
  it "lands cert, proposal, vote rows for Conway-era governance txs at tip" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-follow-governance" $ \ledgerDir -> do
        tracer <- quietTracer

        -- 250 empty blocks puts the tip past two epoch boundaries so
        -- Ingest exits cleanly before withAppSession enters Follow.
        _ <- forgeAndPushBlocks mn 250

        withAppSession tracer governanceTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          baselineBlocks   <- countRows (tdName blockTableDef)
          baselineDrepH    <- countRows (tdName drepHashTableDef)
          baselineDrepReg  <- countRows (tdName drepRegistrationTableDef)
          baselineCommH    <- countRows (tdName committeeHashTableDef)
          baselineCommReg  <- countRows (tdName committeeRegistrationTableDef)
          baselineGap      <- countRows (tdName govActionProposalTableDef)
          baselineParam    <- countRows (tdName paramProposalTableDef)
          baselineWithdraw <- countRows (tdName treasuryWithdrawalTableDef)
          baselineAnchor   <- countRows (tdName votingAnchorTableDef)
          baselineVoting   <- countRows (tdName votingProcedureTableDef)

          -- Block A — DRep registration cert.
          drepTxs <- buildDRepRegTxs
          _ <- forgeAndPush mn drepTxs

          -- Block B — committee hot-key authorization cert.
          committeeTxs <- buildCommitteeAuthTxs
          _ <- forgeAndPush mn committeeTxs

          -- Block C — ParameterChange proposal. Capture its txid so
          -- Block E's vote can reference it via the cross-block
          -- proposal cache.
          let paramChange = Conway.mkParamChangeTx
              paramChangeGovId = govActionIdFor paramChange 0
          _ <- forgeAndPush mn [Mock.TxConway paramChange]

          -- Block D — TreasuryWithdrawals proposal.
          _ <- forgeAndPush mn [Mock.TxConway treasuryTx]

          -- Block E — DRep vote on Block C's proposal.
          _ <- forgeAndPush mn [Mock.TxConway (voteOnProposal paramChangeGovId)]

          let expectedBlocks = baselineBlocks + 5
          waitFor
            (tdName blockTableDef <> " count reaches " <> show expectedBlocks)
            (do n <- countRows (tdName blockTableDef); pure (n >= expectedBlocks))
            120

          waitFor
            (tdName votingProcedureTableDef <> " count grows")
            (do n <- countRows (tdName votingProcedureTableDef); pure (n > baselineVoting))
            30

          followDrepH    <- countRows (tdName drepHashTableDef)
          followDrepReg  <- countRows (tdName drepRegistrationTableDef)
          followCommH    <- countRows (tdName committeeHashTableDef)
          followCommReg  <- countRows (tdName committeeRegistrationTableDef)
          followGap      <- countRows (tdName govActionProposalTableDef)
          followParam    <- countRows (tdName paramProposalTableDef)
          followWithdraw <- countRows (tdName treasuryWithdrawalTableDef)
          followAnchor   <- countRows (tdName votingAnchorTableDef)
          followVoting   <- countRows (tdName votingProcedureTableDef)

          -- Cert tables.
          (followDrepH    - baselineDrepH)    `shouldSatisfy` (>= 1)
          (followDrepReg  - baselineDrepReg)  `shouldSatisfy` (>= 1)
          (followCommH    - baselineCommH)    `shouldSatisfy` (>= 2)  -- cold + hot
          (followCommReg  - baselineCommReg)  `shouldSatisfy` (>= 1)
          -- Proposal tables.
          (followGap      - baselineGap)      `shouldSatisfy` (>= 2)  -- ParameterChange + TreasuryWithdrawals
          (followParam    - baselineParam)    `shouldSatisfy` (>= 1)
          (followWithdraw - baselineWithdraw) `shouldSatisfy` (>= 1)
          (followAnchor   - baselineAnchor)   `shouldSatisfy` (>= 1)
          -- Vote table — cross-block lookup hit means the row landed.
          (followVoting   - baselineVoting)   `shouldSatisfy` (>= 1)

          -- The ParameterChange row carries a non-NULL @param_proposal@ FK.
          paramFk <- T.strip <$> queryTestDb
            ( "SELECT COALESCE(param_proposal::text, '') FROM "
                <> tdName govActionProposalTableDef
                <> " WHERE type = 'ParameterChange' "
                <> "ORDER BY id DESC LIMIT 1"
            )
          paramFk `shouldSatisfy` (not . T.null)

          -- The vote row points at a gov_action_proposal id (sanity check
          -- that the cross-block cache resolved the reference).
          voteFk <- T.strip <$> queryTestDb
            ( "SELECT COALESCE(gov_action_proposal_id::text, '') FROM "
                <> tdName votingProcedureTableDef
                <> " ORDER BY id DESC LIMIT 1"
            )
          voteFk `shouldSatisfy` (not . T.null)

-- ---------------------------------------------------------------------------
-- * Profile
-- ---------------------------------------------------------------------------

-- | 'ledgerEnabledTestProfile' with @pcGovernance@ flipped on.
governanceTestProfile :: SyncConfig
governanceTestProfile =
  ledgerEnabledTestProfile
    { scOptions = (scOptions ledgerEnabledTestProfile)
        { pcGovernance = SyncOption True
        }
    }

-- ---------------------------------------------------------------------------
-- * Tx builders
-- ---------------------------------------------------------------------------

-- | DRep registration with no anchor. The mock builder hard-codes a
-- 500_000_000 lovelace deposit; the @drep_registration@ row carries
-- it as a positive 'Just' value.
buildDRepRegTxs :: IO [Mock.TxEra]
buildDRepRegTxs = case Generic.unregisteredDRepIds of
  drepCred : _ -> case Conway.mkRegisterDRepTx drepCred of
    Right tx -> pure [Mock.TxConway tx]
    Left err -> panic $ "buildDRepRegTxs: " <> show err
  [] -> panic "buildDRepRegTxs: unregisteredDRepIds is empty"

-- | Committee hot-key authorization for the first bootstrap cold key.
-- Writes two @committee_hash@ rows (cold + hot) plus one
-- @committee_registration@ row linking the pair.
buildCommitteeAuthTxs :: IO [Mock.TxEra]
buildCommitteeAuthTxs = case Generic.bootstrapCommitteeCreds of
  (cold, hot) : _ -> case Conway.mkCommitteeAuthTx cold hot of
    Right tx -> pure [Mock.TxConway tx]
    Left err -> panic $ "buildCommitteeAuthTxs: " <> show err
  [] -> panic "buildCommitteeAuthTxs: bootstrapCommitteeCreds is empty"

-- | A 'TreasuryWithdrawals' proposal returning 10 ada to the first
-- unregistered stake credential.
treasuryTx :: Core.Tx Core.TopTx ConwayEra
treasuryTx = case Generic.unregisteredStakeCredentials of
  cred : _ ->
    Conway.mkTreasuryWithdrawalTx
      (AccountAddress Testnet (AccountId cred))
      (Coin 10_000_000)
  [] -> panic "treasuryTx: unregisteredStakeCredentials is empty"

-- | DRep vote @Yes@ on the given proposal.
voteOnProposal :: Governance.GovActionId -> Core.Tx Core.TopTx ConwayEra
voteOnProposal gaId =
  Conway.mkGovVoteYesTx gaId (Generic.drepVoters)

-- ---------------------------------------------------------------------------
-- * Pure txid derivation
-- ---------------------------------------------------------------------------

-- | Compute the 'GovActionId' for a proposal carried by the given tx
-- at the given @OSet@ index.
--
-- The mock's proposal builder ('mkGovActionProposalTx' and its
-- specific arms like 'mkParamChangeTx') puts exactly one proposal at
-- index 0; the @GovActionIx@ matches the order of
-- @ctbProposalProcedures@.
govActionIdFor
  :: Core.Tx Core.TopTx ConwayEra -> Word16 -> Governance.GovActionId
govActionIdFor (MkConwayTx atx) ix =
  Governance.GovActionId (Core.txIdTxBody (atBody atx)) (Governance.GovActionIx ix)
