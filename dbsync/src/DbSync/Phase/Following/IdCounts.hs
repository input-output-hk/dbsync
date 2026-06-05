{-# LANGUAGE OverloadedStrings #-}

-- | Count the IDs an extractor pipeline will assign for a block.
--
-- The Follow path pre-allocates the FK-referenced IDs in a single
-- 'Hasql.Pipeline' call before any extractor runs. That requires
-- knowing the exact count per sequence up front; this module
-- supplies the walker.
--
-- Counts cover only the IDs assigned with @assignXxxId@ — dedup
-- tables ('slot_leader', 'pool_hash', 'stake_address',
-- 'multi_asset', 'address') resolve through their own SELECT/INSERT
-- paths and leaf tables let PostgreSQL allocate via IDENTITY at
-- INSERT time, so neither is pre-allocated here.
module DbSync.Phase.Following.IdCounts
  ( IdCounts (..)
  , emptyIdCounts
  , countAssignableIds
  ) where

import Cardano.Prelude

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

-- | Per-sequence ID demand for one block.
--
-- The field order matches the dependency order the IDs are consumed
-- in by the extractors, which in turn matches the order the
-- allocator's pipeline issues nextvals — letting tests assert that
-- the allocator returns the same shape it was asked for.
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

-- | Walk every transaction in the block once and tally the ID
-- demand per sequence. Pure; no IO.
countAssignableIds :: GenericBlock -> IdCounts
countAssignableIds blk = foldl' tally emptyIdCounts (blkTxs blk)

tally :: IdCounts -> GenericTx -> IdCounts
tally !c tx = c
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
    !nOuts      = if valid then length (txOutputs tx) else 0
    !nCollOuts  = if valid then 0 else case txCollateralOutput tx of
                                         Nothing -> 0
                                         Just _  -> 1
    !certCounts = foldl' tallyCert emptyCertCounts (txCertificates tx)
    !nRedeemers = length (txRedeemers tx :: [GenericTxRedeemer])
    !propCounts = foldl' tallyProposal emptyProposalCounts (txProposals tx)

-- | Per-cert-kind tally accumulated while walking 'txCertificates'.
data CertCounts = CertCounts
  { ccPoolUpdate  :: !Int
  , ccPoolMetaRef :: !Int
  }

emptyCertCounts :: CertCounts
emptyCertCounts = CertCounts 0 0

tallyCert :: CertCounts -> GenericTxCertificate -> CertCounts
tallyCert !cc (GenericTxCertificate _ action) = case action of
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
