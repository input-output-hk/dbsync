{-# LANGUAGE OverloadedStrings #-}

-- | Count the IDs an extractor pipeline will assign for a block.
--
-- The Follow path pre-allocates all assignable IDs in a single
-- 'Hasql.Pipeline' call before any extractor runs. That requires
-- knowing the exact count per sequence up front; this module
-- supplies the walker.
--
-- Counts cover only the IDs assigned with @assignXxxId@ — dedup
-- tables ('slot_leader', 'pool_hash', 'stake_address',
-- 'multi_asset', 'address') resolve through their own SELECT/INSERT
-- paths and aren't pre-allocated here.
module DbSync.Phase.Following.IdCounts
  ( IdCounts (..)
  , emptyIdCounts
  , countAssignableIds
  ) where

import Cardano.Prelude

import DbSync.Block.Types
  ( CertAction (..)
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  , GenericTxOut (..)
  , PoolRegistrationData (..)
  )

-- | Per-sequence ID demand for one block.
--
-- The field order matches the dependency order the IDs are consumed
-- in by the extractors, which in turn matches the order the
-- allocator's pipeline issues nextvals — letting tests assert that
-- the allocator returns the same shape it was asked for.
data IdCounts = IdCounts
  { -- | One per tx in the block (excluding EBBs which have no txs).
    icTxIds                 :: !Int
  , -- | One per tx output across the block. Failed txs contribute
    -- 0 outputs (their on-chain effect is only collateral).
    icTxOutIds              :: !Int
  , -- | One per regular tx input. Failed txs contribute none.
    icTxInIds               :: !Int
  , -- | One per collateral input across the block.
    icCollateralTxInIds     :: !Int
  , -- | At most one per tx (failed txs only).
    icCollateralTxOutIds    :: !Int
  , -- | One per reference input.
    icReferenceTxInIds      :: !Int
  , -- | One per top-level @tx_metadata@ key the extractor will emit.
    icTxMetadataIds         :: !Int
  , -- | One per minted (policy, asset, qty) triple.
    icMaTxMintIds           :: !Int
  , -- | One per (output, embedded asset) pair across the block.
    icMaTxOutIds            :: !Int
  , -- | One per tx that carries non-empty @txCborRaw@.
    icTxCborIds             :: !Int
  , icStakeRegistrationIds  :: !Int
  , icStakeDeregistrationIds :: !Int
  , icDelegationIds         :: !Int
  , icWithdrawalIds         :: !Int
  , icPoolUpdateIds         :: !Int
  , -- | One per pool registration cert that carries metadata.
    icPoolMetadataRefIds    :: !Int
  , -- | One per owner across all pool registrations in the block.
    icPoolOwnerIds          :: !Int
  , icPoolRetireIds         :: !Int
  , -- | One per relay declared across all pool registrations.
    icPoolRelayIds          :: !Int
  }
  deriving stock (Eq, Show)

-- | All counters at zero. Useful for tests and as a 'Monoid' unit
-- if a caller wants to combine multiple blocks (we don't, but it
-- shows the shape).
emptyIdCounts :: IdCounts
emptyIdCounts = IdCounts
  { icTxIds                  = 0
  , icTxOutIds               = 0
  , icTxInIds                = 0
  , icCollateralTxInIds      = 0
  , icCollateralTxOutIds     = 0
  , icReferenceTxInIds       = 0
  , icTxMetadataIds          = 0
  , icMaTxMintIds            = 0
  , icMaTxOutIds             = 0
  , icTxCborIds              = 0
  , icStakeRegistrationIds   = 0
  , icStakeDeregistrationIds = 0
  , icDelegationIds          = 0
  , icWithdrawalIds          = 0
  , icPoolUpdateIds          = 0
  , icPoolMetadataRefIds     = 0
  , icPoolOwnerIds           = 0
  , icPoolRetireIds          = 0
  , icPoolRelayIds           = 0
  }

-- | Walk every transaction in the block once and tally the ID
-- demand per sequence. Pure; no IO.
--
-- Conventions follow the existing extractors:
--
--   * Valid txs contribute one @tx_in@ per input, one @tx_out@ per
--     output, and per-output assets to @ma_tx_out@.
--   * Failed txs contribute only the collateral output (if any).
--   * Collateral inputs are counted for every tx (script-witness on
--     valid txs, actually-consumed on failed txs).
--   * Metadata IDs are one per key inside the structured
--     @txMetadata@ map.
--   * Pool registration owners and relays are counted per cert,
--     not deduplicated — the extractor inserts one row per owner /
--     relay even if the same key appears in multiple registrations.
countAssignableIds :: GenericBlock -> IdCounts
countAssignableIds blk = foldl' tally emptyIdCounts (blkTxs blk)

tally :: IdCounts -> GenericTx -> IdCounts
tally !c tx =
  let !n           = c { icTxIds = icTxIds c + 1 }
      !valid       = txValidContract tx
      !nOuts       = if valid then length (txOutputs tx) else 0
      !nIns        = if valid then length (txInputs  tx) else 0
      !nRefIns     = length (txReferenceInputs tx)
      !nCollOuts   = if valid then 0 else case txCollateralOutput tx of
                                            Nothing -> 0
                                            Just _  -> 1
      !nCollIns    = length (txCollateralInputs tx)
      !nMetaKeys   = maybe 0 length (txMetadata tx)
      !nMints      = length (txMint tx)
      !nMaOuts     = if valid then sum (length . txOutAssets <$> txOutputs tx) else 0
      !nCbor       = case txCborRaw tx of
                       Nothing -> 0
                       Just _  -> 1
      !certCounts  = foldl' tallyCert emptyCertCounts (txCertificates tx)
      !nWithdraw   = length (txWithdrawals tx)
  in n
       { icTxOutIds               = icTxOutIds n + nOuts
       , icTxInIds                = icTxInIds  n + nIns
       , icReferenceTxInIds       = icReferenceTxInIds n + nRefIns
       , icCollateralTxInIds      = icCollateralTxInIds n + nCollIns
       , icCollateralTxOutIds     = icCollateralTxOutIds n + nCollOuts
       , icTxMetadataIds          = icTxMetadataIds n + nMetaKeys
       , icMaTxMintIds            = icMaTxMintIds n + nMints
       , icMaTxOutIds             = icMaTxOutIds n + nMaOuts
       , icTxCborIds              = icTxCborIds n + nCbor
       , icStakeRegistrationIds   = icStakeRegistrationIds n + ccStakeReg certCounts
       , icStakeDeregistrationIds = icStakeDeregistrationIds n + ccStakeDereg certCounts
       , icDelegationIds          = icDelegationIds n + ccDelegation certCounts
       , icWithdrawalIds          = icWithdrawalIds n + nWithdraw
       , icPoolUpdateIds          = icPoolUpdateIds n + ccPoolUpdate certCounts
       , icPoolMetadataRefIds     = icPoolMetadataRefIds n + ccPoolMetaRef certCounts
       , icPoolOwnerIds           = icPoolOwnerIds n + ccPoolOwner certCounts
       , icPoolRetireIds          = icPoolRetireIds n + ccPoolRetire certCounts
       , icPoolRelayIds           = icPoolRelayIds n + ccPoolRelay certCounts
       }

-- | Per-cert-kind tally accumulated while walking 'txCertificates'.
data CertCounts = CertCounts
  { ccStakeReg     :: !Int
  , ccStakeDereg   :: !Int
  , ccDelegation   :: !Int
  , ccPoolUpdate   :: !Int
  , ccPoolMetaRef  :: !Int
  , ccPoolOwner    :: !Int
  , ccPoolRetire   :: !Int
  , ccPoolRelay    :: !Int
  }

emptyCertCounts :: CertCounts
emptyCertCounts = CertCounts 0 0 0 0 0 0 0 0

tallyCert :: CertCounts -> GenericTxCertificate -> CertCounts
tallyCert !cc (GenericTxCertificate _ action) = case action of
  CertStakeRegistration _ _    ->
    cc { ccStakeReg = ccStakeReg cc + 1 }
  CertStakeDeregistration _    ->
    cc { ccStakeDereg = ccStakeDereg cc + 1 }
  CertDelegation _ _           ->
    cc { ccDelegation = ccDelegation cc + 1 }
  CertConwayRegDeleg _ _ _     ->
    cc { ccStakeReg   = ccStakeReg   cc + 1
       , ccDelegation = ccDelegation cc + 1
       }
  -- Conway *-vote certs touch governance tables that aren't ported
  -- yet; the StakeDelegation extractor reads only the stake half.
  CertConwayDelegVote _ _      ->
    cc { ccDelegation = ccDelegation cc + 1 }
  CertConwayDelegStakeVote _ _ _ ->
    cc { ccDelegation = ccDelegation cc + 1 }
  CertPoolRegistration prd     ->
    cc { ccPoolUpdate  = ccPoolUpdate cc + 1
       , ccPoolMetaRef = ccPoolMetaRef cc
                          + maybe 0 (const 1) (prdMetadata prd)
       , ccPoolOwner   = ccPoolOwner cc + length (prdOwners prd)
       , ccPoolRelay   = ccPoolRelay cc + length (prdRelays prd)
       }
  CertPoolRetirement _ _       ->
    cc { ccPoolRetire = ccPoolRetire cc + 1 }
  -- Governance / MIR / genesis-delegation certs land in tables that
  -- aren't ported; they contribute no IDs to the Follow-side path.
  _                            -> cc

-- | Number of @(policy, name, qty)@ triples in a tx output.
-- Inlined here to avoid pulling in the UTxO extractor's helpers.
txOutAssets :: GenericTxOut -> [(ByteString, ByteString, Integer)]
txOutAssets = txOutMultiAssets
