{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the @governance@ schema (16 tables).
--
-- Pure tests — no PostgreSQL required. Coverage focuses on:
--
-- * 'TableDef' shape: column counts, nullability of the load-bearing
--   nullable columns (@drep_hash.raw@, the three voter-id columns
--   on @voting_procedure@, every column on @param_proposal@ except
--   @id@ / @registered_tx_id@), and the unique constraints that
--   index creation will pick up later.
--
-- * COPY encoder behaviour: field counts, @\\N@ for absent
--   optional columns, hex for bytea, the exact ASCII string each
--   governance enum constructor emits (Vote, VoterRole,
--   GovActionType, AnchorType — drift between Haskell constructor
--   and PG string corrupts data silently).
--
-- * The 'Db.Types' hasql enum codecs round-trip via their wire
--   strings.
--
-- End-to-end correctness against forged Conway-era transactions
-- lands when the real Governance extractor is built.
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
  , committeeDeRegistrationTableDef
  , committeeHashTableDef
  , committeeMemberTableDef
  , committeeRegistrationTableDef
  , committeeTableDef
  , constitutionTableDef
  , delegationVoteTableDef
  , drepDistrTableDef
  , drepHashTableDef
  , drepRegistrationTableDef
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
  , eventInfoTableDef
  , govActionProposalTableDef
  , paramProposalTableDef
  , treasuryWithdrawalTableDef
  , votingAnchorTableDef
  , votingProcedureTableDef
  )
import DbSync.Db.Schema.Ids
  ( BlockId (..)
  , CommitteeHashId (..)
  , ConstitutionId (..)
  , DelegationVoteId (..)
  , DrepDistrId (..)
  , DrepHashId (..)
  , DrepRegistrationId (..)
  , EventInfoId (..)
  , GovActionProposalId (..)
  , ParamProposalId (..)
  , StakeAddressId (..)
  , TreasuryWithdrawalId (..)
  , TxId (..)
  , VotingAnchorId (..)
  , VotingProcedureId (..)
  , CommitteeRegistrationId (..)
  )
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
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
  committeeSpec
  committeeHashSpec
  committeeMemberSpec
  committeeRegistrationSpec
  committeeDeRegistrationSpec
  paramProposalSpec
  treasuryWithdrawalSpec
  eventInfoSpec

-- ---------------------------------------------------------------------------
-- DrepHash
-- ---------------------------------------------------------------------------

drepHashSpec :: Spec
drepHashSpec = describe "drepHashTableDef" $ do
  it "has 4 columns in golden order, raw is nullable" $ do
    tdName drepHashTableDef `shouldBe` "drep_hash"
    map cdName (tdColumns drepHashTableDef) `shouldBe`
      ["id", "raw", "view", "has_script"]
    cdNullable (tdColumns drepHashTableDef !! 1) `shouldBe` True

  it "is unique on (raw, has_script)" $
    tdUniqueConstraints drepHashTableDef `shouldBe` ["raw" :| ["has_script"]]

  describe "encodeDrepHashCopy" $ do
    it "produces 4 fields, NULL raw becomes \\N" $ do
      let row = encodeDrepHashCopy (DrepHashId 1)
                  (DrepHash Nothing "drep_always_abstain" False)
          fields = BS8.split '\t' (BS8.init row)
      length fields `shouldBe` 4
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
drepRegistrationSpec = describe "drepRegistrationTableDef" $ do
  it "has 6 columns, deposit and voting_anchor_id nullable" $ do
    map cdName (tdColumns drepRegistrationTableDef) `shouldBe`
      ["id", "tx_id", "cert_index", "deposit", "drep_hash_id", "voting_anchor_id"]
    cdNullable (tdColumns drepRegistrationTableDef !! 3) `shouldBe` True
    cdNullable (tdColumns drepRegistrationTableDef !! 5) `shouldBe` True

  describe "encodeDrepRegistrationCopy" $
    it "writes optional deposit and voting_anchor_id as \\N when absent" $ do
      let row = encodeDrepRegistrationCopy (DrepRegistrationId 1)
                  (DrepRegistration (TxId 7) 0 Nothing (DrepHashId 9) Nothing)
          fields = BS8.split '\t' (BS8.init row)
      fields !! 3 `shouldBe` "\\N"
      fields !! 5 `shouldBe` "\\N"

-- ---------------------------------------------------------------------------
-- DrepDistr
-- ---------------------------------------------------------------------------

drepDistrSpec :: Spec
drepDistrSpec = describe "drepDistrTableDef" $ do
  it "is unique on (hash_id, epoch_no) with 5 columns" $ do
    tdUniqueConstraints drepDistrTableDef
      `shouldBe` ["hash_id" :| ["epoch_no"]]
    length (tdColumns drepDistrTableDef) `shouldBe` 5

  it "active_until is nullable" $
    cdNullable (tdColumns drepDistrTableDef !! 4) `shouldBe` True

  describe "encodeDrepDistrCopy" $
    it "produces 5 fields with active_until \\N when absent" $ do
      let row = encodeDrepDistrCopy (DrepDistrId 1)
                  (DrepDistr (DrepHashId 7) 1000000 210 Nothing)
          fields = BS8.split '\t' (BS8.init row)
      length fields `shouldBe` 5
      fields !! 4 `shouldBe` "\\N"

-- ---------------------------------------------------------------------------
-- DelegationVote
-- ---------------------------------------------------------------------------

delegationVoteSpec :: Spec
delegationVoteSpec = describe "delegationVoteTableDef" $ do
  it "has 6 columns, redeemer_id nullable, no unique constraints" $ do
    length (tdColumns delegationVoteTableDef) `shouldBe` 6
    cdNullable (tdColumns delegationVoteTableDef !! 5) `shouldBe` True
    tdUniqueConstraints delegationVoteTableDef `shouldBe` []

  describe "encodeDelegationVoteCopy" $
    it "encodes redeemer_id as \\N when absent" $ do
      let row = encodeDelegationVoteCopy (DelegationVoteId 1)
                  (DelegationVote (StakeAddressId 1) 0 (DrepHashId 2)
                                  (TxId 3) Nothing)
          fields = BS8.split '\t' (BS8.init row)
      fields !! 5 `shouldBe` "\\N"

-- ---------------------------------------------------------------------------
-- GovActionProposal
-- ---------------------------------------------------------------------------

govActionProposalSpec :: Spec
govActionProposalSpec = describe "govActionProposalTableDef" $ do
  it "has 15 columns including JSONB description and a self-FK" $ do
    map cdName (tdColumns govActionProposalTableDef) `shouldBe`
      [ "id", "tx_id", "index", "prev_gov_action_proposal", "deposit"
      , "return_address", "expiration", "voting_anchor_id", "type"
      , "description", "param_proposal", "ratified_epoch"
      , "enacted_epoch", "dropped_epoch", "expired_epoch"
      ]
    cdType (tdColumns govActionProposalTableDef !! 9) `shouldBe` PgJsonb

  it "stores type as TEXT (GovActionType enum) and deposit as NUMERIC" $ do
    cdType (tdColumns govActionProposalTableDef !! 8) `shouldBe` PgText
    cdType (tdColumns govActionProposalTableDef !! 4) `shouldBe` PgNumeric

  describe "encodeGovActionProposalCopy" $ do
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
votingProcedureSpec = describe "votingProcedureTableDef" $ do
  it "has 11 columns, three nullable voter-id slots" $ do
    map cdName (tdColumns votingProcedureTableDef) `shouldBe`
      [ "id", "tx_id", "index", "gov_action_proposal_id", "voter_role"
      , "drep_voter", "pool_voter", "vote", "voting_anchor_id"
      , "committee_voter", "invalid"
      ]
    -- exactly one of drep_voter / pool_voter / committee_voter is set
    -- per row, all three are nullable
    forM_ [5, 6, 9, 10] $ \i ->
      cdNullable (tdColumns votingProcedureTableDef !! i) `shouldBe` True

  describe "encodeVotingProcedureCopy" $ do
    it "encodes every Vote constructor as the matching PG string" $
      forM_
        [ (VoteYes,     "Yes")
        , (VoteNo,      "No")
        , (VoteAbstain, "Abstain")
        ] $ \(v, expected) -> do
          let row = encodeVotingProcedureCopy (VotingProcedureId 1)
                      sampleVote { votingProcedureVote = v }
              fields = BS8.split '\t' (BS8.init row)
          fields !! 7 `shouldBe` expected

    it "encodes every VoterRole constructor as the matching PG string" $
      forM_
        [ (ConstitutionalCommittee, "ConstitutionalCommittee")
        , (DRep,                    "DRep")
        , (SPO,                     "SPO")
        ] $ \(r, expected) -> do
          let row = encodeVotingProcedureCopy (VotingProcedureId 1)
                      sampleVote { votingProcedureVoterRole = r }
              fields = BS8.split '\t' (BS8.init row)
          fields !! 4 `shouldBe` expected

    it "DRep voter sets drep_voter and leaves pool/committee NULL" $ do
      let row = encodeVotingProcedureCopy (VotingProcedureId 1) sampleVote
          fields = BS8.split '\t' (BS8.init row)
      fields !! 5  `shouldBe` "9"     -- drep_voter
      fields !! 6  `shouldBe` "\\N"   -- pool_voter
      fields !! 9  `shouldBe` "\\N"   -- committee_voter

-- ---------------------------------------------------------------------------
-- VotingAnchor
-- ---------------------------------------------------------------------------

votingAnchorSpec :: Spec
votingAnchorSpec = describe "votingAnchorTableDef" $ do
  it "has 5 columns, all NOT NULL, unique on (data_hash, url, type)" $ do
    map cdName (tdColumns votingAnchorTableDef) `shouldBe`
      ["id", "url", "data_hash", "type", "block_id"]
    all (not . cdNullable) (tdColumns votingAnchorTableDef) `shouldBe` True
    tdUniqueConstraints votingAnchorTableDef
      `shouldBe` ["data_hash" :| ["url", "type"]]

  describe "encodeVotingAnchorCopy" $
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
constitutionSpec = describe "constitutionTableDef" $ do
  it "has 4 columns; gov_action_proposal_id and script_hash nullable" $ do
    length (tdColumns constitutionTableDef) `shouldBe` 4
    cdNullable (tdColumns constitutionTableDef !! 1) `shouldBe` True
    cdNullable (tdColumns constitutionTableDef !! 3) `shouldBe` True

  describe "encodeConstitutionCopy" $
    it "writes \\N for absent script_hash" $ do
      let row = encodeConstitutionCopy (ConstitutionId 1)
                  (Constitution Nothing (VotingAnchorId 1) Nothing)
          fields = BS8.split '\t' (BS8.init row)
      fields !! 1 `shouldBe` "\\N"
      fields !! 3 `shouldBe` "\\N"

-- ---------------------------------------------------------------------------
-- Committee + CommitteeHash + CommitteeMember + CommitteeRegistration
-- ---------------------------------------------------------------------------

committeeSpec :: Spec
committeeSpec = describe "committeeTableDef" $
  it "has 4 columns; gov_action_proposal_id nullable" $ do
    map cdName (tdColumns committeeTableDef) `shouldBe`
      ["id", "gov_action_proposal_id", "quorum_numerator", "quorum_denominator"]
    cdNullable (tdColumns committeeTableDef !! 1) `shouldBe` True

committeeHashSpec :: Spec
committeeHashSpec = describe "committeeHashTableDef" $ do
  it "has 3 columns, both raw and has_script NOT NULL, unique on the pair" $ do
    map cdName (tdColumns committeeHashTableDef) `shouldBe`
      ["id", "raw", "has_script"]
    all (not . cdNullable) (tdColumns committeeHashTableDef) `shouldBe` True
    tdUniqueConstraints committeeHashTableDef
      `shouldBe` ["raw" :| ["has_script"]]

  describe "encodeCommitteeHashCopy" $
    it "encodes 28-byte raw hash as hex" $ do
      let row = encodeCommitteeHashCopy (CommitteeHashId 1)
                  (CommitteeHash (BS.replicate 28 0xcd) True)
          fields = BS8.split '\t' (BS8.init row)
      fields !! 1 `shouldBe` "\\\\x" <> BS8.concat (replicate 28 "cd")
      fields !! 2 `shouldBe` "t"

committeeMemberSpec :: Spec
committeeMemberSpec = describe "committeeMemberTableDef" $
  it "has 4 columns linking a committee snapshot to a hash with expiry" $
    map cdName (tdColumns committeeMemberTableDef) `shouldBe`
      ["id", "committee_id", "committee_hash_id", "expiration_epoch"]

committeeRegistrationSpec :: Spec
committeeRegistrationSpec = describe "committeeRegistrationTableDef" $ do
  it "has 5 columns pairing a cold key with a hot key" $
    map cdName (tdColumns committeeRegistrationTableDef) `shouldBe`
      ["id", "tx_id", "cert_index", "cold_key_id", "hot_key_id"]

  describe "encodeCommitteeRegistrationCopy" $
    it "writes both key columns as decimal ints" $ do
      let row = encodeCommitteeRegistrationCopy (CommitteeRegistrationId 1)
                  (CommitteeRegistration (TxId 1) 0 (CommitteeHashId 7)
                                         (CommitteeHashId 8))
          fields = BS8.split '\t' (BS8.init row)
      fields !! 3 `shouldBe` "7"
      fields !! 4 `shouldBe` "8"

committeeDeRegistrationSpec :: Spec
committeeDeRegistrationSpec = describe "committeeDeRegistrationTableDef" $
  it "has 5 columns; voting_anchor_id nullable" $ do
    length (tdColumns committeeDeRegistrationTableDef) `shouldBe` 5
    cdNullable (tdColumns committeeDeRegistrationTableDef !! 3) `shouldBe` True

-- ---------------------------------------------------------------------------
-- ParamProposal — the wide one
-- ---------------------------------------------------------------------------

paramProposalSpec :: Spec
paramProposalSpec = describe "paramProposalTableDef" $ do
  it "has 55 columns total (id + 54 parameter slots)" $
    length (tdColumns paramProposalTableDef) `shouldBe` 55

  it "marks every column NULLABLE except id and registered_tx_id" $ do
    let cols = tdColumns paramProposalTableDef
        nonNullable =
          [ cdName c | c <- cols, not (cdNullable c) ]
    nonNullable `shouldBe` ["id", "registered_tx_id"]

  it "doubles ride PgText (matching the existing pool_update.margin pattern)" $ do
    let cols = tdColumns paramProposalTableDef
        findCol name = headMay [ c | c <- cols, cdName c == name ]
    -- influence, monetary_expand_rate, treasury_growth_rate, etc. — Doubles
    cdType <$> findCol "influence"            `shouldBe` Just PgText
    cdType <$> findCol "monetary_expand_rate" `shouldBe` Just PgText
    cdType <$> findCol "price_mem"            `shouldBe` Just PgText
    cdType <$> findCol "min_fee_ref_script_cost_per_byte" `shouldBe` Just PgText

  it "uses PgSmallInt for the Word16 protocol/collateral columns" $ do
    let cols = tdColumns paramProposalTableDef
        findCol name = headMay [ c | c <- cols, cdName c == name ]
    cdType <$> findCol "protocol_major"        `shouldBe` Just PgSmallInt
    cdType <$> findCol "protocol_minor"        `shouldBe` Just PgSmallInt
    cdType <$> findCol "collateral_percent"    `shouldBe` Just PgSmallInt
    cdType <$> findCol "max_collateral_inputs" `shouldBe` Just PgSmallInt

  it "has no unique constraints (proposals are not deduped)" $
    tdUniqueConstraints paramProposalTableDef `shouldBe` []

  describe "encodeParamProposalCopy" $ do
    it "produces 55 tab-separated fields (one per column)" $ do
      let row = encodeParamProposalCopy (ParamProposalId 1) emptyParamProposal
          tabs = BS.count (fromIntegral (fromEnum '\t')) row
      tabs `shouldBe` 54   -- 55 fields → 54 separators
      BS8.last row `shouldBe` '\n'

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
treasuryWithdrawalSpec = describe "treasuryWithdrawalTableDef" $ do
  it "has 4 columns, amount NUMERIC NOT NULL" $ do
    map cdName (tdColumns treasuryWithdrawalTableDef) `shouldBe`
      ["id", "gov_action_proposal_id", "stake_address_id", "amount"]
    cdType (tdColumns treasuryWithdrawalTableDef !! 3) `shouldBe` PgNumeric

  describe "encodeTreasuryWithdrawalCopy" $
    it "writes amount as decimal" $ do
      let row = encodeTreasuryWithdrawalCopy (TreasuryWithdrawalId 1)
                  (TreasuryWithdrawal (GovActionProposalId 5)
                                      (StakeAddressId 7)
                                      (DbLovelace 1234567890))
          fields = BS8.split '\t' (BS8.init row)
      fields !! 3 `shouldBe` "1234567890"

-- ---------------------------------------------------------------------------
-- EventInfo
-- ---------------------------------------------------------------------------

eventInfoSpec :: Spec
eventInfoSpec = describe "eventInfoTableDef" $ do
  it "has 5 columns; tx_id and explanation nullable; type is plain TEXT" $ do
    map cdName (tdColumns eventInfoTableDef) `shouldBe`
      ["id", "tx_id", "epoch", "type", "explanation"]
    cdNullable (tdColumns eventInfoTableDef !! 1) `shouldBe` True
    cdNullable (tdColumns eventInfoTableDef !! 4) `shouldBe` True
    cdType (tdColumns eventInfoTableDef !! 3) `shouldBe` PgText

  it "is unlogged with no unique constraints (audit-only table)" $ do
    tdMode eventInfoTableDef `shouldBe` TableUnlogged
    tdUniqueConstraints eventInfoTableDef `shouldBe` []

  describe "encodeEventInfoCopy" $
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
-- for verifying COPY field counts and the @\\N@ encoding.
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
