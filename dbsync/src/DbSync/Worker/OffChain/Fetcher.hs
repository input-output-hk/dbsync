-- | Off-chain data fetcher.
--
-- Fetches off-chain metadata (pool metadata, governance vote anchors)
-- from URLs referenced in on-chain transactions. Intended to run as
-- a background worker with rate limiting and retry logic. Reserved
-- module slot pending implementation.
module DbSync.Worker.OffChain.Fetcher
  (
  ) where
