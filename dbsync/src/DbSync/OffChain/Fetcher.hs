-- | Off-chain data fetcher.
--
-- Fetches off-chain metadata (pool metadata, governance vote anchors)
-- from URLs referenced in on-chain transactions. Runs as a background
-- worker with rate limiting and retry logic.
module DbSync.OffChain.Fetcher
  ( -- TODO: startFetcher, FetcherHandle
  ) where

import Cardano.Prelude
