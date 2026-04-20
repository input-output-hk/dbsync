{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : DbSync.OffChain.Types
Description : Types for off-chain metadata fetching.

Defines the function-record interface for fetching off-chain metadata
(stake pool metadata, governance voting anchors) and the associated
reference and result types.
-}
module DbSync.OffChain.Types
  ( -- * Fetcher interface
    OffChainFetcher (..)

    -- * Reference types
  , PoolMetadataRef (..)
  , VotingAnchorRef (..)

    -- * Result types
  , PoolMetadata (..)
  , VoteMetadata (..)

    -- * Errors
  , FetchError (..)
  ) where

import Cardano.Prelude

-- ---------------------------------------------------------------------------
-- * Fetcher interface
-- ---------------------------------------------------------------------------

-- | Function record for off-chain metadata fetching.
--
-- Implemented by the HTTP fetching layer; consumed by the off-chain
-- worker that schedules and retries fetches.
data OffChainFetcher = OffChainFetcher
  { ofFetchPoolMetadata :: !(PoolMetadataRef -> IO (Either FetchError PoolMetadata))
      -- ^ Fetch pool metadata from the URL in the reference
  , ofFetchVoteMetadata :: !(VotingAnchorRef -> IO (Either FetchError VoteMetadata))
      -- ^ Fetch vote/governance anchor metadata
  , ofGetPendingPools   :: !(IO [PoolMetadataRef])
      -- ^ Retrieve pool metadata references awaiting fetch
  , ofGetPendingVotes   :: !(IO [VotingAnchorRef])
      -- ^ Retrieve voting anchor references awaiting fetch
  , ofSavePoolResult    :: !(PoolMetadataRef -> Either FetchError PoolMetadata -> IO ())
      -- ^ Persist the result (success or failure) of a pool metadata fetch
  , ofSaveVoteResult    :: !(VotingAnchorRef -> Either FetchError VoteMetadata -> IO ())
      -- ^ Persist the result (success or failure) of a vote metadata fetch
  }

-- ---------------------------------------------------------------------------
-- * Reference types
-- ---------------------------------------------------------------------------

-- | Reference to off-chain pool metadata.
-- Contains the URL and expected hash from the on-chain registration.
data PoolMetadataRef = PoolMetadataRef
  { pmrPoolId   :: !ByteString  -- ^ Pool key hash
  , pmrUrl      :: !Text        -- ^ Metadata URL from the registration certificate
  , pmrMetaHash :: !ByteString  -- ^ Expected hash of the metadata content
  }
  deriving stock (Eq, Show)

-- | Reference to a governance voting anchor.
-- Contains the URL and expected hash from the on-chain proposal/vote.
data VotingAnchorRef = VotingAnchorRef
  { varUrl      :: !Text        -- ^ Anchor URL
  , varMetaHash :: !ByteString  -- ^ Expected content hash
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Result types
-- ---------------------------------------------------------------------------

-- | Successfully fetched pool metadata.
data PoolMetadata = PoolMetadata
  { pmName        :: !Text            -- ^ Pool name
  , pmDescription :: !Text            -- ^ Pool description
  , pmTicker      :: !Text            -- ^ Pool ticker symbol
  , pmHomepage    :: !Text            -- ^ Pool homepage URL
  , pmRawJson     :: !ByteString      -- ^ Raw JSON content
  }
  deriving stock (Show)

-- | Successfully fetched governance vote/anchor metadata.
data VoteMetadata = VoteMetadata
  { vmTitle   :: !(Maybe Text)   -- ^ Optional title from the anchor
  , vmAbstract :: !(Maybe Text)  -- ^ Optional abstract/summary
  , vmRawJson :: !ByteString     -- ^ Raw JSON content
  }
  deriving stock (Show)

-- ---------------------------------------------------------------------------
-- * Errors
-- ---------------------------------------------------------------------------

-- | Errors that can occur during off-chain metadata fetching.
data FetchError
  = FetchErrorHttp !Text
      -- ^ HTTP request failure (timeout, DNS, connection refused, etc.)
  | FetchErrorHashMismatch !ByteString !ByteString
      -- ^ Expected hash vs actual hash mismatch
  | FetchErrorDecode !Text
      -- ^ JSON decoding failure
  | FetchErrorTooLarge !Int
      -- ^ Response body exceeded the size limit
  deriving stock (Eq, Show)
