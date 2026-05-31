{-# LANGUAGE NumericUnderscores #-}

-- | 'StateQueryVar' construction and the retry policy for the node
-- fallback.
--
-- Kept separate from the main 'DbSync.StateQuery' module so callers
-- that only need to allocate a fresh handle (boot path) or tweak the
-- retry policy (tests) don't pull in the slot-lookup machinery.
module DbSync.StateQuery.Var
  ( -- * Construction
    newStateQueryVar

    -- * Retry policy
  , RetryConfig (..)
  , defaultRetryConfig
  ) where

import Cardano.Prelude

import Control.Concurrent.STM (newEmptyTMVarIO, newTVarIO)

import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Config (TopLevelConfig)

import DbSync.StateQuery.ObservedSummary (initObservedSummary)
import DbSync.StateQuery.Types (StateQueryVar (..))

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

-- | Create a new 'StateQueryVar' with an empty interpreter cache and a
-- Byron-only initial observed summary derived from the consensus
-- 'TopLevelConfig'.
newStateQueryVar
  :: TopLevelConfig (CardanoBlock StandardCrypto)
  -> IO StateQueryVar
newStateQueryVar topLevelCfg =
  StateQueryVar
    <$> newEmptyTMVarIO
    <*> newTVarIO Nothing
    <*> newTVarIO (initObservedSummary topLevelCfg)

-- ---------------------------------------------------------------------------
-- * Retry policy
-- ---------------------------------------------------------------------------

-- | Retry policy for the node-interpreter fallback in
-- 'DbSync.StateQuery.getSlotDetailsIOWith'.
--
-- The fallback is taken when neither the cached interpreter nor the
-- observed summary can answer for the requested slot. Each attempt
-- queries the node, validates the response against that slot, and (on
-- failure) sleeps for @'rcBackoffMicros' n@ microseconds before
-- attempt @n + 1@.
data RetryConfig = RetryConfig
  { rcMaxAttempts   :: !Int
    -- ^ Total number of node-query attempts. The last attempt does
    -- not back off; if it fails the call throws.
  , rcBackoffMicros :: !(Int -> Int)
    -- ^ Microseconds to wait between attempts. Argument is the
    -- zero-based index of the attempt that just failed (so the wait
    -- before attempt @n + 1@).
  }

-- | Production retry policy: 10 attempts; geometric backoff capped at
-- 300 seconds; the nine backoffs between the ten attempts sum to
-- 1,800 seconds (= 30 minutes).
--
-- Sequence: 20, 40, 80, 160, 300, 300, 300, 300, 300 seconds.
defaultRetryConfig :: RetryConfig
defaultRetryConfig = RetryConfig
  { rcMaxAttempts   = 10
  , rcBackoffMicros = \n -> 1_000_000 * min 300 (20 * (2 ^ min n (4 :: Int)))
  }
