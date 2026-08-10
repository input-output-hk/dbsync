{-# LANGUAGE OverloadedStrings #-}

-- | Function-record interface for fetching off-chain metadata (stake
-- pool metadata, governance voting anchors) and its reference and
-- result types.
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
  , ofFetchVoteMetadata :: !(VotingAnchorRef -> IO (Either FetchError VoteMetadata))
  , ofGetPendingPools   :: !(IO [PoolMetadataRef])
  , ofGetPendingVotes   :: !(IO [VotingAnchorRef])
  , ofSavePoolResult    :: !(PoolMetadataRef -> Either FetchError PoolMetadata -> IO ())
  , ofSaveVoteResult    :: !(VotingAnchorRef -> Either FetchError VoteMetadata -> IO ())
  }

-- ---------------------------------------------------------------------------
-- * Reference types
-- ---------------------------------------------------------------------------

-- | Reference to off-chain pool metadata, from the on-chain
-- registration certificate.
data PoolMetadataRef = PoolMetadataRef
  { pmrPoolId   :: !ByteString  -- ^ Pool key hash
  , pmrUrl      :: !Text        -- ^ Metadata URL from the registration certificate
  , pmrMetaHash :: !ByteString  -- ^ Expected hash of the metadata content
  }
  deriving stock (Eq, Show)

-- | Reference to a governance voting anchor, from the on-chain
-- proposal or vote.
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
-- 'pmCanonicalJson' is the aeson round-tripped encoding. PG's @jsonb@
-- parser is stricter than aeson's, so the round trip stops inserts
-- that fail on bytes aeson accepted.
data PoolMetadata = PoolMetadata
  { pmTicker        :: !Text          -- ^ Pool ticker symbol
  , pmHash          :: !ByteString    -- ^ Verified content hash
  , pmRawBytes      :: !ByteString    -- ^ Raw response body
  , pmCanonicalJson :: !Text          -- ^ aeson-canonicalised JSON
  }
  deriving stock (Show)

-- | Result of a vote-anchor HTTP fetch.
--
-- The fetcher returns a 'VoteMetadata' on any HTTP success, even when
-- the body is not valid JSON or does not match a CIP schema. Those
-- cases still write an @off_chain_vote_data@ row. Network failures
-- and hash mismatches return 'Left' and become
-- @off_chain_vote_fetch_error@ rows instead.
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
  | FetchErrorHashMismatch !ByteString !ByteString
      -- ^ Expected hash, then actual hash.
  | FetchErrorDecode !Text
  | FetchErrorTooLarge !Int
      -- ^ Body size that exceeded the limit.
  | FetchErrorBadContentType !Text
  | FetchErrorBadUrl !Text
      -- ^ URL validation rejected the ref: non-http(s), private IP, etc.
  | FetchErrorTimeout !Text
  | FetchErrorConnectionFailure
  | FetchErrorIpfsAllGatewaysFailed ![Text]
      -- ^ Every configured gateway failed for an @ipfs:\/\/@ url.
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
