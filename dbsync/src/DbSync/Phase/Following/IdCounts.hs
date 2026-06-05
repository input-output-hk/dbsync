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
  , GenericTx (..)
  , GenericTxCertificate (..)
  , PoolRegistrationData (..)
  )

-- | Per-sequence ID demand for one block.
--
-- The field order matches the dependency order the IDs are consumed
-- in by the extractors, which in turn matches the order the
-- allocator's pipeline issues nextvals — letting tests assert that
-- the allocator returns the same shape it was asked for.
data IdCounts = IdCounts
  { icTxIds              :: !Int
  , icTxOutIds           :: !Int
  , icCollateralTxOutIds :: !Int
  , icPoolUpdateIds      :: !Int
  , icPoolMetadataRefIds :: !Int
  }
  deriving stock (Eq, Show)

emptyIdCounts :: IdCounts
emptyIdCounts = IdCounts
  { icTxIds              = 0
  , icTxOutIds           = 0
  , icCollateralTxOutIds = 0
  , icPoolUpdateIds      = 0
  , icPoolMetadataRefIds = 0
  }

-- | Walk every transaction in the block once and tally the ID
-- demand per sequence. Pure; no IO.
countAssignableIds :: GenericBlock -> IdCounts
countAssignableIds blk = foldl' tally emptyIdCounts (blkTxs blk)

tally :: IdCounts -> GenericTx -> IdCounts
tally !c tx =
  let !n           = c { icTxIds = icTxIds c + 1 }
      !valid       = txValidContract tx
      !nOuts       = if valid then length (txOutputs tx) else 0
      !nCollOuts   = if valid then 0 else case txCollateralOutput tx of
                                            Nothing -> 0
                                            Just _  -> 1
      !certCounts  = foldl' tallyCert emptyCertCounts (txCertificates tx)
  in n
       { icTxOutIds           = icTxOutIds n + nOuts
       , icCollateralTxOutIds = icCollateralTxOutIds n + nCollOuts
       , icPoolUpdateIds      = icPoolUpdateIds n + ccPoolUpdate certCounts
       , icPoolMetadataRefIds = icPoolMetadataRefIds n + ccPoolMetaRef certCounts
       }

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
