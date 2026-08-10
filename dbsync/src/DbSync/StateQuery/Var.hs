{-# LANGUAGE NumericUnderscores #-}

-- | 'StateQueryVar' construction and the retry policy for the node
-- fallback.
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

-- | Empty interpreter cache plus a Byron-only observed summary.
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
data RetryConfig = RetryConfig
  { rcMaxAttempts   :: !Int
    -- ^ Total number of node-query attempts. The last attempt does
    -- not back off; if it fails the call throws.
  , rcBackoffMicros :: !(Int -> Int)
    -- ^ Microseconds to wait between attempts. Argument is the
    -- zero-based index of the attempt that just failed (so the wait
    -- before attempt @n + 1@).
  }

-- | Ten attempts with geometric backoff capped at 300 seconds:
-- 20, 40, 80, 160, 300, 300, 300, 300, 300 — 1,800 seconds in total.
defaultRetryConfig :: RetryConfig
defaultRetryConfig = RetryConfig
  { rcMaxAttempts   = 10
  , rcBackoffMicros = \n -> 1_000_000 * min 300 (20 * (2 ^ min n (4 :: Int)))
  }
