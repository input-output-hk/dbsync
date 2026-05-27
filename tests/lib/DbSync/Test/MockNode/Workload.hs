{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Descriptors for forging blocks whose transaction shape mimics
-- realistic Cardano workloads.
--
-- Consumed by 'DbSync.Test.MockNode.forgeAndPushBlocksWith'. The
-- @Workload@ record describes the per-block payment-tx population;
-- the presets here cover the common cases tests need.
--
-- Every workload produces payment transactions that spend one UTxO
-- and create:
--
--   * @wOutputsPerTx@ outputs at freshly-derived addresses (see
--     'paymentCredentialAt'), each with @wOutputLovelace@; and
--   * one change output back to the spent UTxO's address.
--
-- Setting @wOutputsPerTx = 0@ produces change-only transactions
-- (each tx leaves the live UTxO map size unchanged). Setting it
-- @> 0@ grows the UTxO map by @wOutputsPerTx@ per tx, which lets
-- subsequent blocks use a higher @wTxsPerBlock@.
module DbSync.Test.MockNode.Workload
  ( -- * Workload descriptor
    Workload (..)

    -- * Presets
  , mainnetLikeWorkload
  , warmupWorkload
  , stressWorkload

    -- * Fresh address derivation
  , paymentCredentialAt
  ) where

import Cardano.Prelude

import Cardano.Ledger.Core (ADDRHASH)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Mock.Forging.Tx.Generic (mkDummyHash)

-- ---------------------------------------------------------------------------
-- * Workload descriptor
-- ---------------------------------------------------------------------------

-- | Per-block payment-tx workload.
--
-- @wTxsPerBlock@ payment txs are built against the same pre-block
-- ledger state, so each tx must spend a distinct @UTxOIndex@.
-- Callers (i.e. 'forgeAndPushBlocksWith') must therefore ensure the
-- live UTxO map holds at least @wTxsPerBlock@ entries before
-- forging a block.
data Workload = Workload
  { wTxsPerBlock    :: !Int
    -- ^ Transactions per forged block.
  , wOutputsPerTx   :: !Int
    -- ^ Fresh outputs per tx (excluding the implicit change output).
    -- @0@ keeps the UTxO map size stable; @> 0@ grows it.
  , wFeeLovelace    :: !Integer
    -- ^ Fee deducted from the change output per tx, in lovelace.
  , wOutputLovelace :: !Integer
    -- ^ Value of each non-change output, in lovelace.
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Presets
-- ---------------------------------------------------------------------------

-- | Approximates a mid-traffic Conway mainnet block: 10 payment txs,
-- change-only outputs. Holds the live UTxO map size constant so the
-- workload runs forever against a fixed-size genesis UTxO set.
mainnetLikeWorkload :: Workload
mainnetLikeWorkload = Workload
  { wTxsPerBlock    = 10
  , wOutputsPerTx   = 0
  , wFeeLovelace    = 1_000
  , wOutputLovelace = 1_000_000
  }

-- | Fan-out workload: 10 payment txs per block, each producing 10
-- fresh outputs (plus change). Grows the live UTxO map by ~100
-- entries per block. Used to pre-bloat the UTxO set before running
-- 'stressWorkload' so the latter has enough distinct inputs to
-- spend in each block.
--
-- Output value is sized so fresh outputs remain comfortably above
-- @stressWorkload@'s fee even after many subsequent stress txs
-- chip away at them, given the bundled test fixtures' 900K-lovelace
-- genesis UTxOs.
warmupWorkload :: Workload
warmupWorkload = Workload
  { wTxsPerBlock    = 10
  , wOutputsPerTx   = 10
  , wFeeLovelace    = 1_000
  , wOutputLovelace = 80_000
  }

-- | High-density workload: 100 payment txs per block, change-only.
-- Used by regression tests that need to drive enough writes through
-- 'DbSync.Phase.Ingest.UtxoStore' to fill its write buffer in a
-- reasonable number of blocks.
--
-- Requires the live UTxO map to hold at least 100 entries; precede
-- with one or more 'warmupWorkload' blocks if the test genesis
-- starts smaller. Fee is the protocol minimum so each spent UTxO
-- only loses a few hundred lovelace per cycle.
stressWorkload :: Workload
stressWorkload = Workload
  { wTxsPerBlock    = 100
  , wOutputsPerTx   = 0
  , wFeeLovelace    = 500
  , wOutputLovelace = 0  -- unused: no fresh outputs
  }

-- ---------------------------------------------------------------------------
-- * Fresh address derivation
-- ---------------------------------------------------------------------------

-- | Derive a distinct payment 'Credential' from a monotonic integer
-- index. Different indices produce different credentials (the hash
-- function is injective on the inputs we care about).
paymentCredentialAt :: Int -> Credential Payment
paymentCredentialAt n =
  KeyHashObj (KeyHash (mkDummyHash (Proxy @ADDRHASH) n))
