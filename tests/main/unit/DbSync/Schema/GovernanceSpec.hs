{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the @governance@ COPY encoders: @\\N@ for absent
-- optional columns, hex for bytea, and the exact ASCII string each
-- governance enum constructor emits (Vote, VoterRole, GovActionType,
-- AnchorType — drift between Haskell constructor and PG string
-- corrupts data silently).
module DbSync.Schema.GovernanceSpec (spec) where

import Cardano.Prelude

import Data.List ((!!))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Governance
  ( CommitteeHash (..)
  , CommitteeRegistration (..)
  , Constitution (..)
  , DelegationVote (..)
  , DrepDistr (..)
  , DrepHash (..)
  , DrepRegistration (..)
  , EventInfo (..)
  , GovActionProposal (..)
  , ParamProposal (..)
  , TreasuryWithdrawal (..)
  , VotingAnchor (..)
  , VotingProcedure (..)
  , encodeCommitteeHashCopy
  , encodeCommitteeRegistrationCopy
  , encodeConstitutionCopy
  , encodeDelegationVoteCopy
  , encodeDrepDistrCopy
  , encodeDrepHashCopy
  , encodeDrepRegistrationCopy
  , encodeEventInfoCopy
  , encodeGovActionProposalCopy
  , encodeParamProposalCopy
  , encodeTreasuryWithdrawalCopy
  , encodeVotingAnchorCopy
  , encodeVotingProcedureCopy
  )
import DbSync.Db.Schema.Ids
  ( BlockId (..)
  , CommitteeHashId (..)
  , ConstitutionId (..)
  , DrepHashId (..)
  , EventInfoId (..)
  , GovActionProposalId (..)
  , ParamProposalId (..)
  , StakeAddressId (..)
  , TxId (..)
  , VotingAnchorId (..)
  )
import DbSync.Db.Types
  ( AnchorType (..)
  , DbLovelace (..)
  , GovActionType (..)
  , Vote (..)
  , VoteUrl (..)
  , VoterRole (..)
  )

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  drepHashSpec
  drepRegistrationSpec
  drepDistrSpec
  delegationVoteSpec
  govActionProposalSpec
  votingProcedureSpec
  votingAnchorSpec
  constitutionSpec
  committeeHashSpec
  committeeRegistrationSpec
  paramProposalSpec
  treasuryWithdrawalSpec
  eventInfoSpec

-- ---------------------------------------------------------------------------
-- DrepHash
-- ---------------------------------------------------------------------------

drepHashSpec :: Spec
drepHashSpec = describe "encodeDrepHashCopy" $ do
  it "encodes a NULL raw as \\N" $ do
    let row = encodeDrepHashCopy (DrepHashId 1)
                (DrepHash Nothing "drep_always_abstain" False)
        fields = BS8.split '\t' (BS8.init row)
    fields !! 1 `shouldBe` "\\N"
    fields !! 2 `shouldBe` "drep_always_abstain"
    fields !! 3 `shouldBe` "f"

  it "encodes raw bytes as hex when present" $ do
    let row = encodeDrepHashCopy (DrepHashId 1)
                (DrepHash (Just (BS.replicate 28 0xab)) "drep1abc" True)
        fields = BS8.split '\t' (BS8.init row)
    fields !! 1 `shouldBe` "\\\\x" <> BS8.concat (replicate 28 "ab")
    fields !! 3 `shouldBe` "t"

-- ---------------------------------------------------------------------------
-- DrepRegistration
-- ---------------------------------------------------------------------------

drepRegistrationSpec :: Spec
drepRegistrationSpec = describe "encodeDrepRegistrationCopy" $
  it "writes optional deposit and voting_anchor_id as \\N when absent" $ do
    let row = encodeDrepRegistrationCopy
                (DrepRegistration (TxId 7) 0 Nothing (DrepHashId 9) Nothing)
        fields = BS8.split '\t' (BS8.init row)
    -- id is IDENTITY-managed; the COPY row omits it. Indexes here run
    -- 0..4 across tx_id, cert_index, deposit, drep_hash_id, voting_anchor_id.
    fields !! 2 `shouldBe` "\\N"
    fields !! 4 `shouldBe` "\\N"

-- ---------------------------------------------------------------------------
-- DrepDistr
-- ---------------------------------------------------------------------------

drepDistrSpec :: Spec
drepDistrSpec = describe "encodeDrepDistrCopy" $
  it "writes active_until as \\N when absent" $ do
    let row = encodeDrepDistrCopy
                (DrepDistr (DrepHashId 7) 1000000 210 Nothing)
        fields = BS8.split '\t' (BS8.init row)
    fields !! 3 `shouldBe` "\\N"

-- ---------------------------------------------------------------------------
-- DelegationVote
-- ---------------------------------------------------------------------------

delegationVoteSpec :: Spec
delegationVoteSpec = describe "encodeDelegationVoteCopy" $
  it "encodes redeemer_id as \\N when absent" $ do
    let row = encodeDelegationVoteCopy
                (DelegationVote (StakeAddressId 1) 0 (DrepHashId 2)
                                (TxId 3) Nothing)
        fields = BS8.split '\t' (BS8.init row)
    fields !! 4 `shouldBe` "\\N"

-- ---------------------------------------------------------------------------
-- GovActionProposal
-- ---------------------------------------------------------------------------

govActionProposalSpec :: Spec
govActionProposalSpec = describe "encodeGovActionProposalCopy" $ do
  it "writes the JSONB description as plain text" $ do
    let row = encodeGovActionProposalCopy (GovActionProposalId 1) sampleProposal
        fields = BS8.split '\t' (BS8.init row)
    fields !! 9 `shouldBe` "{\"title\":\"Increase Treasury Growth\"}"

  it "encodes every GovActionType constructor as the matching PG string" $
    forM_
      [ (ParameterChange,     "ParameterChange")
      , (HardForkInitiation,  "HardForkInitiation")
      , (TreasuryWithdrawals, "TreasuryWithdrawals")
      , (NoConfidence,        "NoConfidence")
      , (NewCommitteeType,    "NewCommittee")  -- constructor renamed to avoid clash
      , (NewConstitution,     "NewConstitution")
      , (InfoAction,          "InfoAction")
      ] $ \(t, expected) -> do
        let row = encodeGovActionProposalCopy (GovActionProposalId 1)
                    sampleProposal { govActionProposalType = t }
            fields = BS8.split '\t' (BS8.init row)
        fields !! 8 `shouldBe` expected

  it "writes nullable epoch fields as \\N when absent" $ do
    let row = encodeGovActionProposalCopy (GovActionProposalId 1) sampleProposal
        fields = BS8.split '\t' (BS8.init row)
    fields !! 11 `shouldBe` "\\N" -- ratified_epoch
    fields !! 12 `shouldBe` "\\N" -- enacted_epoch
    fields !! 13 `shouldBe` "\\N" -- dropped_epoch
    fields !! 14 `shouldBe` "\\N" -- expired_epoch

-- ---------------------------------------------------------------------------
-- VotingProcedure
-- ---------------------------------------------------------------------------

votingProcedureSpec :: Spec
votingProcedureSpec = describe "encodeVotingProcedureCopy" $ do
  -- id is IDENTITY-managed; the COPY row omits it. Indexes here run
  -- 0..9 across tx_id, index, gov_action_proposal_id, voter_role,
  -- drep_voter, pool_voter, vote, voting_anchor_id, committee_voter,
  -- invalid.
  it "encodes every Vote constructor as the matching PG string" $
    forM_
      [ (VoteYes,     "Yes")
      , (VoteNo,      "No")
      , (VoteAbstain, "Abstain")
      ] $ \(v, expected) -> do
        let row = encodeVotingProcedureCopy
                    sampleVote { votingProcedureVote = v }
            fields = BS8.split '\t' (BS8.init row)
        fields !! 6 `shouldBe` expected

  it "encodes every VoterRole constructor as the matching PG string" $
    forM_
      [ (ConstitutionalCommittee, "ConstitutionalCommittee")
      , (DRep,                    "DRep")
      , (SPO,                     "SPO")
      ] $ \(r, expected) -> do
        let row = encodeVotingProcedureCopy
                    sampleVote { votingProcedureVoterRole = r }
            fields = BS8.split '\t' (BS8.init row)
        fields !! 3 `shouldBe` expected

  it "DRep voter sets drep_voter and leaves pool/committee NULL" $ do
    let row = encodeVotingProcedureCopy sampleVote
        fields = BS8.split '\t' (BS8.init row)
    fields !! 4 `shouldBe` "9"     -- drep_voter
    fields !! 5 `shouldBe` "\\N"   -- pool_voter
    fields !! 8 `shouldBe` "\\N"   -- committee_voter

-- ---------------------------------------------------------------------------
-- VotingAnchor
-- ---------------------------------------------------------------------------

votingAnchorSpec :: Spec
votingAnchorSpec = describe "encodeVotingAnchorCopy" $
  it "encodes every AnchorType constructor as the matching PG string" $
    forM_
      [ (GovActionAnchor,      "gov_action")
      , (DrepAnchor,           "drep")
      , (OtherAnchor,          "other")
      , (VoteAnchor,           "vote")
      , (CommitteeDeRegAnchor, "committee_dereg")
      , (ConstitutionAnchor,   "constitution")
      ] $ \(t, expected) -> do
        let row = encodeVotingAnchorCopy (VotingAnchorId 1)
                    sampleAnchor { votingAnchorType = t }
            fields = BS8.split '\t' (BS8.init row)
        fields !! 3 `shouldBe` expected

-- ---------------------------------------------------------------------------
-- Constitution
-- ---------------------------------------------------------------------------

constitutionSpec :: Spec
constitutionSpec = describe "encodeConstitutionCopy" $
  it "writes \\N for absent gov_action_proposal_id and script_hash" $ do
    let row = encodeConstitutionCopy (ConstitutionId 1)
                (Constitution Nothing (VotingAnchorId 1) Nothing)
        fields = BS8.split '\t' (BS8.init row)
    fields !! 1 `shouldBe` "\\N"
    fields !! 3 `shouldBe` "\\N"

-- ---------------------------------------------------------------------------
-- CommitteeHash + CommitteeRegistration
-- ---------------------------------------------------------------------------

committeeHashSpec :: Spec
committeeHashSpec = describe "encodeCommitteeHashCopy" $
  it "encodes 28-byte raw hash as hex" $ do
    let row = encodeCommitteeHashCopy (CommitteeHashId 1)
                (CommitteeHash (BS.replicate 28 0xcd) True)
        fields = BS8.split '\t' (BS8.init row)
    fields !! 1 `shouldBe` "\\\\x" <> BS8.concat (replicate 28 "cd")
    fields !! 2 `shouldBe` "t"

committeeRegistrationSpec :: Spec
committeeRegistrationSpec = describe "encodeCommitteeRegistrationCopy" $
  it "writes both key columns as decimal ints" $ do
    let row = encodeCommitteeRegistrationCopy
                (CommitteeRegistration (TxId 1) 0 (CommitteeHashId 7)
                                       (CommitteeHashId 8))
        fields = BS8.split '\t' (BS8.init row)
    -- id is IDENTITY-managed; cold_key_id and hot_key_id sit at 2 and 3.
    fields !! 2 `shouldBe` "7"
    fields !! 3 `shouldBe` "8"

-- ---------------------------------------------------------------------------
-- ParamProposal — the wide one
-- ---------------------------------------------------------------------------

paramProposalSpec :: Spec
paramProposalSpec = describe "encodeParamProposalCopy" $ do
  it "encodes every nullable field as \\N when the proposal sets nothing" $ do
    let row = encodeParamProposalCopy (ParamProposalId 7) emptyParamProposal
        fields = BS8.split '\t' (BS8.init row)
    -- id and registered_tx_id are non-NULL, everything else \\N. Of the
    -- 55 fields exactly 53 should be \\N (id is "7", registered_tx_id is "99").
    fields !! 0 `shouldBe` "7"
    let nullCount = length [ () | f <- fields, f == "\\N" ]
    nullCount `shouldBe` 53

  it "round-trips a representative Double via the TEXT codec" $ do
    let row = encodeParamProposalCopy (ParamProposalId 1)
                emptyParamProposal { paramProposalInfluence = Just 0.3 }
        fields = BS8.split '\t' (BS8.init row)
    -- influence is column 12 in tdColumns (id=0, epoch_no=1, key=2,
    -- min_fee_a..optimal_pool_count=3..11, influence=12)
    fields !! 12 `shouldBe` "0.3"

-- ---------------------------------------------------------------------------
-- TreasuryWithdrawal
-- ---------------------------------------------------------------------------

treasuryWithdrawalSpec :: Spec
treasuryWithdrawalSpec = describe "encodeTreasuryWithdrawalCopy" $
  it "writes amount as decimal" $ do
    let row = encodeTreasuryWithdrawalCopy
                (TreasuryWithdrawal (GovActionProposalId 5)
                                    (StakeAddressId 7)
                                    (DbLovelace 1234567890))
        fields = BS8.split '\t' (BS8.init row)
    -- id is IDENTITY-managed; amount is the 3rd remaining column.
    fields !! 2 `shouldBe` "1234567890"

-- ---------------------------------------------------------------------------
-- EventInfo
-- ---------------------------------------------------------------------------

eventInfoSpec :: Spec
eventInfoSpec = describe "encodeEventInfoCopy" $
  it "writes tx_id and explanation as \\N when absent" $ do
    let row = encodeEventInfoCopy (EventInfoId 1)
                (EventInfo Nothing 210 "DroppedProposal" Nothing)
        fields = BS8.split '\t' (BS8.init row)
    fields !! 1 `shouldBe` "\\N"
    fields !! 4 `shouldBe` "\\N"

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

sampleProposal :: GovActionProposal
sampleProposal = GovActionProposal
  { govActionProposalTxId                  = TxId 1
  , govActionProposalIndex                 = 0
  , govActionProposalPrevGovActionProposal = Nothing
  , govActionProposalDeposit               = DbLovelace 100000000000
  , govActionProposalReturnAddress         = StakeAddressId 5
  , govActionProposalExpiration            = Just 215
  , govActionProposalVotingAnchorId        = Just (VotingAnchorId 9)
  , govActionProposalType                  = ParameterChange
  , govActionProposalDescription           = "{\"title\":\"Increase Treasury Growth\"}"
  , govActionProposalParamProposal         = Just (ParamProposalId 11)
  , govActionProposalRatifiedEpoch         = Nothing
  , govActionProposalEnactedEpoch          = Nothing
  , govActionProposalDroppedEpoch          = Nothing
  , govActionProposalExpiredEpoch          = Nothing
  }

sampleVote :: VotingProcedure
sampleVote = VotingProcedure
  { votingProcedureTxId                = TxId 1
  , votingProcedureIndex               = 0
  , votingProcedureGovActionProposalId = GovActionProposalId 5
  , votingProcedureVoterRole           = DRep
  , votingProcedureDrepVoter           = Just (DrepHashId 9)
  , votingProcedurePoolVoter           = Nothing
  , votingProcedureVote                = VoteYes
  , votingProcedureVotingAnchorId      = Nothing
  , votingProcedureCommitteeVoter      = Nothing
  , votingProcedureInvalid             = Nothing
  }

sampleAnchor :: VotingAnchor
sampleAnchor = VotingAnchor
  { votingAnchorUrl      = VoteUrl "https://example.org/anchor"
  , votingAnchorDataHash = BS.replicate 32 0xab
  , votingAnchorType     = GovActionAnchor
  , votingAnchorBlockId  = BlockId 1
  }

-- | A 'ParamProposal' with every optional field at 'Nothing' — useful
-- for verifying the @\\N@ encoding.
emptyParamProposal :: ParamProposal
emptyParamProposal = ParamProposal
  { paramProposalEpochNo                    = Nothing
  , paramProposalKey                        = Nothing
  , paramProposalMinFeeA                    = Nothing
  , paramProposalMinFeeB                    = Nothing
  , paramProposalMaxBlockSize               = Nothing
  , paramProposalMaxTxSize                  = Nothing
  , paramProposalMaxBhSize                  = Nothing
  , paramProposalKeyDeposit                 = Nothing
  , paramProposalPoolDeposit                = Nothing
  , paramProposalMaxEpoch                   = Nothing
  , paramProposalOptimalPoolCount           = Nothing
  , paramProposalInfluence                  = Nothing
  , paramProposalMonetaryExpandRate         = Nothing
  , paramProposalTreasuryGrowthRate         = Nothing
  , paramProposalDecentralisation           = Nothing
  , paramProposalEntropy                    = Nothing
  , paramProposalProtocolMajor              = Nothing
  , paramProposalProtocolMinor              = Nothing
  , paramProposalMinUtxoValue               = Nothing
  , paramProposalMinPoolCost                = Nothing
  , paramProposalCostModelId                = Nothing
  , paramProposalPriceMem                   = Nothing
  , paramProposalPriceStep                  = Nothing
  , paramProposalMaxTxExMem                 = Nothing
  , paramProposalMaxTxExSteps               = Nothing
  , paramProposalMaxBlockExMem              = Nothing
  , paramProposalMaxBlockExSteps            = Nothing
  , paramProposalMaxValSize                 = Nothing
  , paramProposalCollateralPercent          = Nothing
  , paramProposalMaxCollateralInputs        = Nothing
  , paramProposalRegisteredTxId             = TxId 99
  , paramProposalCoinsPerUtxoSize           = Nothing
  , paramProposalPvtMotionNoConfidence      = Nothing
  , paramProposalPvtCommitteeNormal         = Nothing
  , paramProposalPvtCommitteeNoConfidence   = Nothing
  , paramProposalPvtHardForkInitiation      = Nothing
  , paramProposalPvtppSecurityGroup         = Nothing
  , paramProposalDvtMotionNoConfidence      = Nothing
  , paramProposalDvtCommitteeNormal         = Nothing
  , paramProposalDvtCommitteeNoConfidence   = Nothing
  , paramProposalDvtUpdateToConstitution    = Nothing
  , paramProposalDvtHardForkInitiation      = Nothing
  , paramProposalDvtPPNetworkGroup          = Nothing
  , paramProposalDvtPPEconomicGroup         = Nothing
  , paramProposalDvtPPTechnicalGroup        = Nothing
  , paramProposalDvtPPGovGroup              = Nothing
  , paramProposalDvtTreasuryWithdrawal      = Nothing
  , paramProposalCommitteeMinSize           = Nothing
  , paramProposalCommitteeMaxTermLength     = Nothing
  , paramProposalGovActionLifetime          = Nothing
  , paramProposalGovActionDeposit           = Nothing
  , paramProposalDrepDeposit                = Nothing
  , paramProposalDrepActivity               = Nothing
  , paramProposalMinFeeRefScriptCostPerByte = Nothing
  }
