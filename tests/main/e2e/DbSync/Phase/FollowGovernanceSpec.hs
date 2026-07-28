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

import qualified Data.Set as Set
import qualified Data.Text as T

import Cardano.Crypto.Hash (hashToTextAsHex)
import Cardano.Ledger.Address (AccountAddress (..), AccountId (..), Withdrawals (..))
import Cardano.Ledger.BaseTypes (EpochNo (..), Network (..), StrictMaybe (..), textToUrl)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Conway.Governance as Governance
import Cardano.Ledger.Conway.Tx (AlonzoTx (..), Tx (..))
import Cardano.Ledger.Conway.TxCert (Delegatee (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..))
import qualified Cardano.Ledger.DRep as Ledger
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Ouroboros.Consensus.Shelley.Eras (ConwayEra)

import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import qualified Cardano.Mock.Forging.Tx.Generic as Generic
import qualified Cardano.Mock.Forging.Types as Mock

import DbSync.App.Config.Types
  ( SyncConfig (..)
  , OptionFlag (..)
  , DbProfile (..)
  )
import DbSync.Db.Schema.Core (blockTableDef)
import DbSync.Db.Schema.EpochBoundary (epochParamTableDef)
import DbSync.Db.Schema.Governance
  ( committeeHashTableDef
  , committeeMemberTableDef
  , committeeRegistrationTableDef
  , committeeTableDef
  , constitutionTableDef
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
  ( ledgerEnabledTestConfig
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.Database (queryTestDb)
import DbSync.Test.E2E (conwayConfigDir, withAppSession)
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockNode
  ( MockNode
  , forgeAndPush
  , forgeAndPushBlocks
  , forgeAndPushCommitteeCreds
  , forgeAndPushDRepsAndDelegateVotes
  , forgeAndPushUntilNextEpoch
  , forgeAndPushWithStakeCreds
  , voteAllOnAction
  , withMockNode
  )
import DbSync.Test.PgAssertions (countRows)

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Follow governance writes" $ do
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

          -- Cert tables — one DRep reg, one committee auth (hot key only).
          (followDrepH    - baselineDrepH)    `shouldBe` 1
          (followDrepReg  - baselineDrepReg)  `shouldBe` 1
          -- Only the hot key is new; the cold key is a genesis committee
          -- member seeded during Ingest, so its committee_hash pre-exists.
          (followCommH    - baselineCommH)    `shouldBe` 1
          (followCommReg  - baselineCommReg)  `shouldBe` 1
          -- Proposal tables — ParameterChange + TreasuryWithdrawals.
          (followGap      - baselineGap)      `shouldBe` 2
          (followParam    - baselineParam)    `shouldBe` 1
          (followWithdraw - baselineWithdraw) `shouldBe` 1
          (followAnchor   - baselineAnchor)   `shouldBe` 1
          -- Vote table — cross-block lookup hit means the row landed.
          (followVoting   - baselineVoting)   `shouldBe` 1

          -- The ParameterChange row carries a non-NULL @param_proposal@ FK.
          paramFk <- T.strip <$> queryTestDb
            ( "SELECT COALESCE(param_proposal::text, '') FROM "
                <> tdName govActionProposalTableDef
                <> " WHERE type = 'ParameterChange' "
                <> "ORDER BY id DESC LIMIT 1"
            )
          paramFk `shouldNotBe` ""

          -- The vote's gov_action_proposal_id resolves to Block C's
          -- ParameterChange row: the cross-block cache linked the Block E
          -- vote back to the Block C action.
          paramChangeRowId <- T.strip <$> queryTestDb
            ( "SELECT id::text FROM "
                <> tdName govActionProposalTableDef
                <> " WHERE type = 'ParameterChange' "
                <> "ORDER BY id DESC LIMIT 1"
            )
          voteFk <- T.strip <$> queryTestDb
            ( "SELECT COALESCE(gov_action_proposal_id::text, '') FROM "
                <> tdName votingProcedureTableDef
                <> " ORDER BY id DESC LIMIT 1"
            )
          voteFk `shouldBe` paramChangeRowId

  -- | Both abstract DReps share @(raw=NULL, has_script=FALSE)@; the
  -- Follow-phase SELECT must filter on @view@ to disambiguate them.
  -- A naive @WHERE raw IS NOT DISTINCT FROM $1 AND has_script = $2@
  -- lookup would match both rows and crash with
  -- @UnexpectedRowCountStatementError@.
  it "resolves abstract DReps at tip when both NULL-raw rows already exist" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-follow-abstract-drep" $ \ledgerDir -> do
        tracer <- quietTracer

        case Generic.unregisteredStakeCredentials of
          credA : credB : credC : _ -> do
            -- 250 empty blocks plus two cert blocks that seed both
            -- abstract DReps during Ingest.
            _ <- forgeAndPushBlocks mn 250
            _ <- forgeAndPush mn
                   [buildRegDelegVoteTx credA Ledger.DRepAlwaysAbstain]
            _ <- forgeAndPush mn
                   [buildRegDelegVoteTx credB Ledger.DRepAlwaysNoConfidence]

            withAppSession tracer governanceTestProfile mn ledgerDir $ \_ -> do
              waitForSyncComplete 120

              -- Ingest landed exactly two NULL-raw rows.
              abstractIngest <- queryDrepHashNullRawCount
              abstractIngest `shouldBe` "2"

              -- One more block at tip that targets DRepAlwaysAbstain
              -- again. With the SELECT keyed only on (raw, has_script)
              -- this lookup returns 2 rows and the consumer panics.
              _ <- forgeAndPush mn
                     [buildRegDelegVoteTx credC Ledger.DRepAlwaysAbstain]

              let expectedBlocks = 250 + 3
              waitFor
                (tdName blockTableDef <> " count reaches " <> show expectedBlocks)
                (do n <- countRows (tdName blockTableDef); pure (n >= expectedBlocks))
                60

              -- No new NULL-raw row: the existing drep_always_abstain
              -- row was reused.
              abstractFollow <- queryDrepHashNullRawCount
              abstractFollow `shouldBe` "2"
          _ -> panic "unregisteredStakeCredentials has fewer than 3 entries"

  it "lands constitution and committee rows after enactment at tip" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-follow-gov-enact-cc" $ \ledgerDir -> do
        tracer <- quietTracer
        _ <- forgeAndPushBlocks mn 250
        withAppSession tracer governanceTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          baselineConst <- countRows (tdName constitutionTableDef)
          baselineComm  <- countRows (tdName committeeTableDef)
          baselineProp  <- countRows (tdName govActionProposalTableDef)

          bootstrapGovernance mn

          let constTx = Conway.mkNewConstitutionTx newConstitutionAnchor
              commCred = case Generic.unregisteredCommitteeCreds of
                c : _ -> c
                [] -> panic "unregisteredCommitteeCreds is empty"
              commTx = Conway.mkAddCommitteeTx Nothing commCred
              constGaid = govActionIdFor constTx 0
              commGaid  = govActionIdFor commTx 0
          voteConst <- voteAllOnAction mn constGaid
          voteComm  <- voteAllOnAction mn commGaid
          _ <- forgeAndPush mn
            [ Mock.TxConway constTx
            , Mock.TxConway commTx
            , voteConst
            , voteComm
            ]

          crossEnactmentBoundaries mn

          waitFor
            (tdName constitutionTableDef <> " count grows")
            (do n <- countRows (tdName constitutionTableDef); pure (n > baselineConst))
            120

          followConst <- countRows (tdName constitutionTableDef)
          followComm  <- countRows (tdName committeeTableDef)
          followProp  <- countRows (tdName govActionProposalTableDef)
          (followConst - baselineConst) `shouldBe` 1
          (followComm  - baselineComm)  `shouldBe` 1
          (followProp  - baselineProp)  `shouldBe` 2

  it "links chained committee proposals via prev_gov_action_proposal" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-follow-gov-chained" $ \ledgerDir -> do
        tracer <- quietTracer
        _ <- forgeAndPushBlocks mn 250
        withAppSession tracer governanceTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          baselineComm <- countRows (tdName committeeTableDef)
          baselineProp <- countRows (tdName govActionProposalTableDef)

          bootstrapGovernance mn

          let (cred1, cred2) = case Generic.unregisteredCommitteeCreds of
                a : b : _ -> (a, b)
                _ -> panic "unregisteredCommitteeCreds has fewer than 2 entries"
              p1     = Conway.mkAddCommitteeTx Nothing cred1
              p1Gaid = govActionIdFor p1 0
          voteP1 <- voteAllOnAction mn p1Gaid
          _ <- forgeAndPush mn [Mock.TxConway p1, voteP1]
          crossEnactmentBoundaries mn

          let p2     = Conway.mkAddCommitteeTx
                         (Just (Governance.GovPurposeId p1Gaid))
                         cred2
              p2Gaid = govActionIdFor p2 0
          voteP2 <- voteAllOnAction mn p2Gaid
          _ <- forgeAndPush mn [Mock.TxConway p2, voteP2]
          crossEnactmentBoundaries mn

          waitFor
            (tdName committeeTableDef <> " count reaches baseline+2")
            (do n <- countRows (tdName committeeTableDef); pure (n >= baselineComm + 2))
            120

          followComm <- countRows (tdName committeeTableDef)
          followProp <- countRows (tdName govActionProposalTableDef)
          (followComm - baselineComm) `shouldBe` 2
          (followProp - baselineProp) `shouldBe` 2

          -- The latest committee row carries the full resolved membership
          -- (genesis members plus cred1 and cred2), not just the single
          -- added member p2's tx body carries.
          genesisMembers <- committeeMemberCount
            ( "(SELECT id FROM " <> tdName committeeTableDef
                <> " WHERE gov_action_proposal_id IS NULL LIMIT 1)"
            )
          latestMembers <- committeeMemberCount
            ("(SELECT max(id) FROM " <> tdName committeeTableDef <> ")")
          latestMembers `shouldBe` genesisMembers + 2

  it "resolves full committee membership across an add-then-remove chain" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-follow-gov-committee-members" $ \ledgerDir -> do
        tracer <- quietTracer
        _ <- forgeAndPushBlocks mn 250
        withAppSession tracer governanceTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          bootstrapGovernance mn

          (cred1, cred2) <- case Generic.unregisteredCommitteeCreds of
            a : b : _ -> pure (a, b)
            _ -> panic "unregisteredCommitteeCreds has fewer than 2 entries"
          let genesisCred = case Generic.bootstrapCommitteeCreds of
                (cold, _) : _ -> cold
                [] -> panic "bootstrapCommitteeCreds is empty"

          baselineComm <- countRows (tdName committeeTableDef)
          genesisMembers <- committeeMemberCount
            ( "(SELECT id FROM " <> tdName committeeTableDef
                <> " WHERE gov_action_proposal_id IS NULL LIMIT 1)"
            )

          -- p1 adds cred1 on top of the genesis committee.
          let p1 = Conway.mkAddCommitteeTx Nothing cred1
              p1Gaid = govActionIdFor p1 0
          voteP1 <- voteAllOnAction mn p1Gaid
          _ <- forgeAndPush mn [Mock.TxConway p1, voteP1]
          crossEnactmentBoundaries mn

          -- p2 removes cred1 and adds cred2, chained on p1.
          let p2 =
                Conway.mkUpdateCommitteeTx
                  (Just (Governance.GovPurposeId p1Gaid))
                  (Set.singleton cred1)
                  [(cred2, EpochNo 20)]
              p2Gaid = govActionIdFor p2 0
          voteP2 <- voteAllOnAction mn p2Gaid
          _ <- forgeAndPush mn [Mock.TxConway p2, voteP2]
          crossEnactmentBoundaries mn

          waitFor
            (tdName committeeTableDef <> " count reaches baseline+2")
            (do n <- countRows (tdName committeeTableDef); pure (n >= baselineComm + 2))
            120

          -- The p2 committee row carries the full resolved set: every
          -- genesis member survives both updates, cred2 is added and
          -- cred1 removed — not the single-member tx-body delta.
          latestMembers <- committeeMemberCount
            ("(SELECT max(id) FROM " <> tdName committeeTableDef <> ")")
          latestMembers `shouldBe` genesisMembers + 1

          cred2Present <- latestCommitteeHasMember (credColdKeyHex cred2)
          cred2Present `shouldBe` True
          cred1Present <- latestCommitteeHasMember (credColdKeyHex cred1)
          cred1Present `shouldBe` False
          genesisPresent <- latestCommitteeHasMember (credColdKeyHex genesisCred)
          genesisPresent `shouldBe` True

  it "lands epoch_param row reflecting parameterChange enactment at tip" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-follow-gov-paramchange" $ \ledgerDir -> do
        tracer <- quietTracer
        _ <- forgeAndPushBlocks mn 250
        withAppSession tracer governanceTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          bootstrapGovernance mn

          let proposalTx = Conway.mkParamChangeTx
              gaid = govActionIdFor proposalTx 0
          vote <- voteAllOnAction mn gaid
          _ <- forgeAndPush mn [Mock.TxConway proposalTx, vote]
          crossEnactmentBoundaries mn
          _ <- forgeAndPushUntilNextEpoch mn
          _ <- forgeAndPushUntilNextEpoch mn

          let latestMaxTxSizeQuery =
                "SELECT max_tx_size FROM " <> tdName epochParamTableDef
                  <> " ORDER BY id DESC LIMIT 1"
          -- 'mkParamChangeTx' flips max_tx_size to 32_000.
          waitFor
            "max_tx_size flips to 32000"
            (do v <- T.strip <$> queryTestDb latestMaxTxSizeQuery; pure (v == "32000"))
            120

          latestMaxTxSize <- T.strip <$> queryTestDb latestMaxTxSizeQuery
          latestMaxTxSize `shouldBe` "32000"

  it "flips epoch_param.protocol_major after HardForkInitiation enactment at tip" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-follow-gov-hardfork" $ \ledgerDir -> do
        tracer <- quietTracer
        _ <- forgeAndPushBlocks mn 250
        withAppSession tracer governanceTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          bootstrapGovernance mn

          let proposalTx = Conway.mkHardForkInitTx
              gaid = govActionIdFor proposalTx 0
          vote <- voteAllOnAction mn gaid
          _ <- forgeAndPush mn [Mock.TxConway proposalTx, vote]
          crossEnactmentBoundaries mn
          _ <- forgeAndPushUntilNextEpoch mn
          _ <- forgeAndPushUntilNextEpoch mn

          let latestMajorQuery =
                "SELECT protocol_major FROM " <> tdName epochParamTableDef
                  <> " ORDER BY id DESC LIMIT 1"
          -- 'mkHardForkInitTx' targets ProtVer 11.
          waitFor
            "protocol_major flips to 11"
            (do v <- T.strip <$> queryTestDb latestMajorQuery; pure (v == "11"))
            120

          latestMajor <- T.strip <$> queryTestDb latestMajorQuery
          latestMajor `shouldBe` "11"

  it "records InfoAction proposal and votes but never enacts" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-follow-gov-info" $ \ledgerDir -> do
        tracer <- quietTracer
        _ <- forgeAndPushBlocks mn 250
        withAppSession tracer governanceTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          baselineProp  <- countRows (tdName govActionProposalTableDef)
          baselineVote  <- countRows (tdName votingProcedureTableDef)
          baselineConst <- countRows (tdName constitutionTableDef)
          baselineComm  <- countRows (tdName committeeTableDef)

          bootstrapGovernance mn

          let proposalTx = Conway.mkInfoTx
              gaid = govActionIdFor proposalTx 0
          vote <- voteAllOnAction mn gaid
          _ <- forgeAndPush mn [Mock.TxConway proposalTx, vote]

          -- InfoAction never enacts; a few boundary crossings are
          -- enough to assert no side-effect tables fired.
          replicateM_ 3 $ do
            _ <- forgeAndPushUntilNextEpoch mn
            pure ()

          waitFor
            (tdName votingProcedureTableDef <> " count grows")
            (do n <- countRows (tdName votingProcedureTableDef); pure (n > baselineVote))
            120

          followProp  <- countRows (tdName govActionProposalTableDef)
          followVote  <- countRows (tdName votingProcedureTableDef)
          followConst <- countRows (tdName constitutionTableDef)
          followComm  <- countRows (tdName committeeTableDef)

          infoCount <- T.strip <$> queryTestDb
            ( "SELECT count(*) FROM " <> tdName govActionProposalTableDef
                <> " WHERE type = 'InfoAction'"
            )
          infoCount `shouldBe` "1"

          (followProp - baselineProp) `shouldBe` 1
          -- 'voteAllOnAction' casts one Yes vote per voter — every DRep,
          -- every committee hot key, and the three live stake pools — and
          -- each lands its own voting_procedure row.
          let expectedInfoVotes =
                length Generic.drepVoters + length Generic.committeeVoters + 3
          (followVote - baselineVote) `shouldBe` expectedInfoVotes
          (followConst - baselineConst) `shouldBe` 0
          (followComm  - baselineComm)  `shouldBe` 0

-- ---------------------------------------------------------------------------
-- * Profile
-- ---------------------------------------------------------------------------

-- | 'ledgerEnabledTestConfig' with @pcGovernance@ flipped on.
governanceTestProfile :: SyncConfig
governanceTestProfile =
  ledgerEnabledTestConfig
    { scDbProfile = (scDbProfile ledgerEnabledTestConfig)
        { pcGovernance = OptionFlag True
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

-- | One-tx pair: register @cred@ as a stake credential, then in the
-- following cert delegate its vote to @drep@. The governance extractor
-- materialises a @drep_hash@ row for the abstract sentinels and a
-- @delegation_vote@ row linking the stake credential.
buildRegDelegVoteTx
  :: Credential Core.Staking -> Ledger.DRep -> Mock.TxEra
buildRegDelegVoteTx cred drep =
  case Conway.mkDCertTx [regCert, delegCert] (Withdrawals mempty) Nothing of
    Right tx -> Mock.TxConway tx
    Left err -> panic $ "buildRegDelegVoteTx: " <> show err
  where
    regCert   = Conway.mkRegTxCert SNothing cred
    delegCert = Conway.mkDelegTxCert (DelegVote drep) cred

-- | @SELECT count(*) FROM drep_hash WHERE raw IS NULL@ as a stripped
-- 'Text', so callers can compare against literals like @"2"@.
queryDrepHashNullRawCount :: IO Text
queryDrepHashNullRawCount =
  T.strip <$> queryTestDb "SELECT count(*) FROM drep_hash WHERE raw IS NULL"

-- | Member count of the committee selected by the given @id@ subquery.
committeeMemberCount :: Text -> IO Int
committeeMemberCount committeeIdExpr = do
  t <- T.strip <$> queryTestDb
    ( "SELECT count(*) FROM " <> tdName committeeMemberTableDef
        <> " WHERE committee_id = " <> committeeIdExpr
    )
  pure (fromMaybe 0 (readMaybe (T.unpack t)))

-- | Whether the latest committee has a member whose cold-key hash
-- matches the given hex.
latestCommitteeHasMember :: Text -> IO Bool
latestCommitteeHasMember hashHex = do
  t <- T.strip <$> queryTestDb
    ( "SELECT count(*) FROM " <> tdName committeeMemberTableDef <> " cm JOIN "
        <> tdName committeeHashTableDef <> " ch ON ch.id = cm.committee_hash_id "
        <> "WHERE cm.committee_id = (SELECT max(id) FROM " <> tdName committeeTableDef
        <> ") AND encode(ch.raw, 'hex') = '" <> hashHex <> "'"
    )
  pure (fromMaybe (0 :: Int) (readMaybe (T.unpack t)) > 0)

-- | Cold-key hash (hex) of a committee credential, matching the
-- @encode(raw, 'hex')@ form stored in @committee_hash@.
credColdKeyHex :: Credential kr -> Text
credColdKeyHex cred = case cred of
  KeyHashObj (KeyHash h) -> hashToTextAsHex h
  ScriptHashObj (ScriptHash h) -> hashToTextAsHex h

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

-- ---------------------------------------------------------------------------
-- * Enactment helpers
-- ---------------------------------------------------------------------------

-- | Register pool stake creds, register DReps and delegate stake votes,
-- settle one epoch, then authorize committee hot keys.
bootstrapGovernance :: MockNode -> IO ()
bootstrapGovernance mn = do
  _ <- forgeAndPushWithStakeCreds mn
  _ <- forgeAndPushDRepsAndDelegateVotes mn
  _ <- forgeAndPushUntilNextEpoch mn
  _ <- forgeAndPushCommitteeCreds mn
  pure ()

-- | Force three epoch boundaries so a submitted proposal can finalise
-- votes, ratify, and enact.
crossEnactmentBoundaries :: MockNode -> IO ()
crossEnactmentBoundaries mn = do
  _ <- forgeAndPushUntilNextEpoch mn
  _ <- forgeAndPushUntilNextEpoch mn
  _ <- forgeAndPushUntilNextEpoch mn
  pure ()

-- | Anchor stamped on the proposals used in the enactment tests.
newConstitutionAnchor :: Governance.Anchor
newConstitutionAnchor =
  Governance.Anchor
    { Governance.anchorUrl =
        fromMaybe (panic "newConstitutionAnchor: textToUrl failed")
                  (textToUrl 64 "best.cc")
    , Governance.anchorDataHash =
        Core.hashAnnotated (Governance.AnchorData mempty)
    }
