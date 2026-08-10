{-# LANGUAGE OverloadedStrings #-}

-- | Count the ids an extractor pipeline assigns for a block. The
-- Follow path pre-allocates the FK-referenced ids in one
-- 'Hasql.Pipeline' call, which needs the exact per-sequence count up
-- front.
--
-- The counts cover only the ids that @assignXxxId@ assigns. A dedup
-- table resolves through its own SELECT and INSERT path, and a leaf
-- table lets PostgreSQL allocate through IDENTITY.
module DbSync.Phase.Following.IdCounts
  ( IdCounts (..)
  , emptyIdCounts
  , countAssignableIds
  ) where

import Cardano.Prelude

import DbSync.Extractor (ExtractorDef, scriptsDatumsEnabled)
import DbSync.Parser.Types
  ( CertAction (..)
  , GenericBlock (..)
  , GenericGovAction (..)
  , GenericGovActionProposal (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  , GenericTxRedeemer
  , PoolRegistrationData (..)
  )

-- | Per-sequence id demand for one block. The field order matches
-- the order the extractors consume the ids, and the order the
-- allocator's pipeline issues its nextvals, so a test can assert the
-- allocator returns the shape it received.
data IdCounts = IdCounts
  { icTxIds                 :: !Int
  , icTxOutIds              :: !Int
  , icCollateralTxOutIds    :: !Int
  , icPoolUpdateIds         :: !Int
  , icPoolMetadataRefIds    :: !Int
  , icRedeemerIds           :: !Int
  , icGovActionProposalIds  :: !Int
  , icParamProposalIds      :: !Int
  , icCommitteeIds          :: !Int
  , icConstitutionIds       :: !Int
  }
  deriving stock (Eq, Show)

emptyIdCounts :: IdCounts
emptyIdCounts = IdCounts
  { icTxIds                = 0
  , icTxOutIds             = 0
  , icCollateralTxOutIds   = 0
  , icPoolUpdateIds        = 0
  , icPoolMetadataRefIds   = 0
  , icRedeemerIds          = 0
  , icGovActionProposalIds = 0
  , icParamProposalIds     = 0
  , icCommitteeIds         = 0
  , icConstitutionIds      = 0
  }

-- | Walk every transaction in the block once and tally the id demand
-- per sequence.
--
-- The redeemer count matches the pipeline's assignment gate: it
-- counts none with the @scripts_datums@ extractor off, because its
-- sequence may not exist, and none for a phase-2 invalid tx, because
-- nothing writes redeemer rows for one.
countAssignableIds :: [ExtractorDef] -> GenericBlock -> IdCounts
countAssignableIds extractors blk =
  foldl' (tally (scriptsDatumsEnabled extractors)) emptyIdCounts (blkTxs blk)

tally :: Bool -> IdCounts -> GenericTx -> IdCounts
tally redeemersOn !c tx = c
  { icTxIds                = icTxIds c + 1
  , icTxOutIds             = icTxOutIds c + nOuts
  , icCollateralTxOutIds   = icCollateralTxOutIds c + nCollOuts
  , icPoolUpdateIds        = icPoolUpdateIds c + ccPoolUpdate certCounts
  , icPoolMetadataRefIds   = icPoolMetadataRefIds c + ccPoolMetaRef certCounts
  , icRedeemerIds          = icRedeemerIds c + nRedeemers
  , icGovActionProposalIds = icGovActionProposalIds c + pcGovActionProposal propCounts
  , icParamProposalIds     = icParamProposalIds c + pcParamProposal propCounts
  , icCommitteeIds         = icCommitteeIds c + pcCommittee propCounts
  , icConstitutionIds      = icConstitutionIds c + pcConstitution propCounts
  }
  where
    !valid      = txValidContract tx
    -- The parser writes every tx's surviving output into 'txOutputs'
    -- (a failed tx's collateral-return is folded in there), so a
    -- tx_out id is assigned per entry regardless of validity.
    !nOuts      = length (txOutputs tx)
    -- 'txCollateralOutput' is only 'Just' for a valid tx with an
    -- explicit collateral return; the extractor's collateral-out pass
    -- is gated on 'valid' to match.
    !nCollOuts  = if valid
                    then maybe 0 (const 1) (txCollateralOutput tx)
                    else 0
    !certCounts = foldl' tallyCert emptyCertCounts (txCertificates tx)
    !nRedeemers = if redeemersOn && valid
                    then length (txRedeemers tx :: [GenericTxRedeemer])
                    else 0
    !propCounts = foldl' tallyProposal emptyProposalCounts (txProposals tx)

-- | Per-cert-kind tally accumulated while walking 'txCertificates'.
data CertCounts = CertCounts
  { ccPoolUpdate  :: !Int
  , ccPoolMetaRef :: !Int
  }

emptyCertCounts :: CertCounts
emptyCertCounts = CertCounts 0 0

tallyCert :: CertCounts -> GenericTxCertificate -> CertCounts
tallyCert !cc cert = case txCertAction cert of
  CertPoolRegistration prd ->
    cc { ccPoolUpdate  = ccPoolUpdate cc + 1
       , ccPoolMetaRef = ccPoolMetaRef cc
                          + maybe 0 (const 1) (prdMetadata prd)
       }
  _ -> cc

-- | Per-proposal counters tallied while walking 'txProposals'. One
-- @gov_action_proposal@ id per entry; the inner counters fire on
-- specific 'GenericGovAction' arms.
data ProposalCounts = ProposalCounts
  { pcGovActionProposal :: !Int
  , pcParamProposal     :: !Int
  , pcCommittee         :: !Int
  , pcConstitution      :: !Int
  }

emptyProposalCounts :: ProposalCounts
emptyProposalCounts = ProposalCounts 0 0 0 0

tallyProposal :: ProposalCounts -> GenericGovActionProposal -> ProposalCounts
tallyProposal !pc prop = pc
  { pcGovActionProposal = pcGovActionProposal pc + 1
  , pcParamProposal     = pcParamProposal pc + nParam
  , pcCommittee         = pcCommittee pc + nCommittee
  , pcConstitution      = pcConstitution pc + nConstitution
  }
  where
    (!nParam, !nCommittee, !nConstitution) = case ggapAction prop of
      GovParameterChange {} -> (1, 0, 0)
      GovUpdateCommittee {} -> (0, 1, 0)
      GovNewConstitution {} -> (0, 0, 1)
      _                     -> (0, 0, 0)
