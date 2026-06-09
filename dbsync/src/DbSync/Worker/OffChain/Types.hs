{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : DbSync.Worker.OffChain.Types
Description : Types for off-chain metadata fetching.

Defines the function-record interface for fetching off-chain metadata
(stake pool metadata, governance voting anchors) and the associated
reference and result types.
-}
module DbSync.Worker.OffChain.Types
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
  , renderFetchError
  ) where

import Cardano.Prelude

import qualified Data.Text as T

import DbSync.Db.Types (AnchorType)
import qualified DbSync.Worker.OffChain.Vote.Types as Vote

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
  { varUrl        :: !Text        -- ^ Anchor URL
  , varMetaHash   :: !ByteString  -- ^ Expected content hash
  , varAnchorType :: !AnchorType  -- ^ Where the anchor was attached
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Result types
-- ---------------------------------------------------------------------------

-- | Successfully fetched pool metadata.
--
-- 'pmHash' is the Blake2b_256 digest of 'pmRawBytes' computed by the
-- fetcher; it has already been verified against the expected hash on
-- the on-chain @pool_metadata_ref@ row. 'pmCanonicalJson' is the
-- aeson-roundtripped JSON encoding — PG's @jsonb@ parser is stricter
-- than aeson's, so the round-trip avoids inserts failing on bytes
-- aeson accepted.
data PoolMetadata = PoolMetadata
  { pmTicker        :: !Text          -- ^ Pool ticker symbol
  , pmHash          :: !ByteString    -- ^ Verified content hash
  , pmRawBytes      :: !ByteString    -- ^ Raw response body
  , pmCanonicalJson :: !Text          -- ^ aeson-canonicalised JSON
  }
  deriving stock (Show)

-- | Result of a vote-anchor HTTP fetch.
--
-- The fetcher returns a 'VoteMetadata' on any HTTP success, including
-- responses whose body isn't valid JSON or doesn't conform to a CIP
-- schema — those cases still produce an @off_chain_vote_data@ row,
-- with 'vmIsValidJson' / 'vmCipData' encoding the validation outcome.
-- Network failures and hash mismatches surface as 'Left' 'FetchError'
-- and become @off_chain_vote_fetch_error@ rows instead.
data VoteMetadata = VoteMetadata
  { vmHash          :: !ByteString          -- ^ Verified content hash
  , vmRawBytes      :: !ByteString          -- ^ Raw response body
  , vmCanonicalJson :: !Text                -- ^ aeson-canonicalised JSON (or a
                                            --   JSON-wrapped error message if
                                            --   the body wasn't parseable)
  , vmIsValidJson   :: !Bool                -- ^ Maps to @is_valid = TRUE/FALSE@
                                            --   vs 'Nothing' on the data row
  , vmWarning       :: !(Maybe Text)        -- ^ Non-fatal CIP parse warning
  , vmVoteData      :: !(Maybe Vote.OffChainVoteData)
                                            -- ^ 'Just' when the body matched
                                            --   the CIP schema for its anchor
                                            --   type; drives subtable writes
  }
  deriving stock (Show)

-- ---------------------------------------------------------------------------
-- * Errors
-- ---------------------------------------------------------------------------

-- | Errors that can occur during off-chain metadata fetching.
-- 'renderFetchError' turns one into the @fetch_error@ column text.
data FetchError
  = FetchErrorHttp !Text
      -- ^ HTTP request failure (timeout, DNS, connection refused, etc.)
  | FetchErrorHashMismatch !ByteString !ByteString
      -- ^ Expected hash vs actual hash mismatch
  | FetchErrorDecode !Text
      -- ^ JSON decoding failure
  | FetchErrorTooLarge !Int
      -- ^ Response body exceeded the size limit
  | FetchErrorBadContentType !Text
      -- ^ Server returned an unacceptable Content-Type
  | FetchErrorBadUrl !Text
      -- ^ URL validation rejected the ref (non-http(s), private IP, etc.)
  | FetchErrorTimeout !Text
      -- ^ Connection or response timeout
  | FetchErrorConnectionFailure
      -- ^ TCP-level connection failure
  | FetchErrorIpfsAllGatewaysFailed ![Text]
      -- ^ Every configured gateway returned an error for an @ipfs://@ url
  deriving stock (Eq, Show)

renderFetchError :: FetchError -> Text
renderFetchError = \case
  FetchErrorHttp t                  -> "http: " <> t
  FetchErrorHashMismatch _ _        -> "hash mismatch"
  FetchErrorDecode t                -> "decode: " <> t
  FetchErrorTooLarge n              -> "too large: " <> show n
  FetchErrorBadContentType t        -> "bad content-type: " <> t
  FetchErrorBadUrl t                -> "bad url: " <> t
  FetchErrorTimeout t               -> "timeout: " <> t
  FetchErrorConnectionFailure       -> "connection failed"
  FetchErrorIpfsAllGatewaysFailed e ->
    "ipfs gateways failed: " <> T.intercalate "; " e
