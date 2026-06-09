{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @off_chain_votes@ extractor:
-- inserts for the seven result tables and the work-queue lookups the
-- vote worker uses to find pending or due-for-retry voting anchors.
module DbSync.Db.Statement.OffChainVote
  ( -- * Inserts
    insertOffChainVoteDataRowStmt
  , insertOffChainVoteDataReturningIdStmt
  , insertOffChainVoteGovActionDataRowStmt
  , insertOffChainVoteDrepDataRowStmt
  , insertOffChainVoteAuthorRowStmt
  , insertOffChainVoteReferenceRowStmt
  , insertOffChainVoteExternalUpdateRowStmt
  , insertOffChainVoteFetchErrorRowStmt

    -- * Work-queue lookups
  , PendingVoteFetch (..)
  , queryNewPendingVoteFetchesStmt
  , queryRetryPendingVoteFetchesStmt

    -- * Retry-count lookup
  , selectMaxVoteRetryCountStmt
  ) where

import Cardano.Prelude

import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (localTimeToUTC, utc)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids
  ( OffChainVoteDataId (..)
  , VotingAnchorId (..)
  , idDecoder
  , idEncoder
  )
import DbSync.Db.Schema.OffChainVote
  ( OffChainVoteAuthor
  , OffChainVoteData
  , OffChainVoteDrepData
  , OffChainVoteExternalUpdate
  , OffChainVoteFetchError
  , OffChainVoteGovActionData
  , OffChainVoteReference
  , offChainVoteAuthorEncoder
  , offChainVoteAuthorTableDef
  , offChainVoteDataEncoder
  , offChainVoteDataTableDef
  , offChainVoteDrepDataEncoder
  , offChainVoteDrepDataTableDef
  , offChainVoteExternalUpdateEncoder
  , offChainVoteExternalUpdateTableDef
  , offChainVoteFetchErrorEncoder
  , offChainVoteFetchErrorTableDef
  , offChainVoteGovActionDataEncoder
  , offChainVoteGovActionDataTableDef
  , offChainVoteReferenceEncoder
  , offChainVoteReferenceTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)
import DbSync.Db.Types (AnchorType, VoteUrl, anchorTypeDecoder, voteUrlDecoder)

-- ---------------------------------------------------------------------------
-- * Inserts
-- ---------------------------------------------------------------------------

insertOffChainVoteDataRowStmt :: Stmt.Statement OffChainVoteData ()
insertOffChainVoteDataRowStmt =
  Stmt.preparable (insertRowSql offChainVoteDataTableDef) offChainVoteDataEncoder D.noResult

-- | Insert a row and return the identity-allocated @id@. Needed by
-- the off-chain vote worker so subtable rows (gov_action / drep /
-- author / reference / external_update) can carry the FK.
insertOffChainVoteDataReturningIdStmt
  :: Stmt.Statement OffChainVoteData OffChainVoteDataId
insertOffChainVoteDataReturningIdStmt =
  Stmt.preparable sql offChainVoteDataEncoder (D.singleRow (idDecoder OffChainVoteDataId))
  where
    sql =
      "INSERT INTO off_chain_vote_data \
      \(voting_anchor_id, hash, json, bytes, warning, language, comment, is_valid) \
      \VALUES ($1, $2, $3, $4, $5, $6, $7, $8) \
      \RETURNING id"

insertOffChainVoteGovActionDataRowStmt :: Stmt.Statement OffChainVoteGovActionData ()
insertOffChainVoteGovActionDataRowStmt =
  Stmt.preparable
    (insertRowSql offChainVoteGovActionDataTableDef)
    offChainVoteGovActionDataEncoder
    D.noResult

insertOffChainVoteDrepDataRowStmt :: Stmt.Statement OffChainVoteDrepData ()
insertOffChainVoteDrepDataRowStmt =
  Stmt.preparable
    (insertRowSql offChainVoteDrepDataTableDef)
    offChainVoteDrepDataEncoder
    D.noResult

insertOffChainVoteAuthorRowStmt :: Stmt.Statement OffChainVoteAuthor ()
insertOffChainVoteAuthorRowStmt =
  Stmt.preparable
    (insertRowSql offChainVoteAuthorTableDef)
    offChainVoteAuthorEncoder
    D.noResult

insertOffChainVoteReferenceRowStmt :: Stmt.Statement OffChainVoteReference ()
insertOffChainVoteReferenceRowStmt =
  Stmt.preparable
    (insertRowSql offChainVoteReferenceTableDef)
    offChainVoteReferenceEncoder
    D.noResult

insertOffChainVoteExternalUpdateRowStmt :: Stmt.Statement OffChainVoteExternalUpdate ()
insertOffChainVoteExternalUpdateRowStmt =
  Stmt.preparable
    (insertRowSql offChainVoteExternalUpdateTableDef)
    offChainVoteExternalUpdateEncoder
    D.noResult

insertOffChainVoteFetchErrorRowStmt :: Stmt.Statement OffChainVoteFetchError ()
insertOffChainVoteFetchErrorRowStmt =
  Stmt.preparable
    (insertRowSql offChainVoteFetchErrorTableDef)
    offChainVoteFetchErrorEncoder
    D.noResult

-- ---------------------------------------------------------------------------
-- * Work-queue lookups
-- ---------------------------------------------------------------------------

-- | One anchor in the off-chain vote fetch work queue.
--
-- 'pvfPrevFetchTime' is 'Nothing' for anchors never attempted and
-- 'Just t' (with matching retry count) for anchors whose last attempt
-- failed and is due to retry.
data PendingVoteFetch = PendingVoteFetch
  { pvfVotingAnchorId :: !VotingAnchorId
  , pvfUrl            :: !VoteUrl
  , pvfHash           :: !ByteString
  , pvfAnchorType     :: !AnchorType
  , pvfPrevFetchTime  :: !(Maybe UTCTime)
  , pvfPrevRetryCount :: !Word64
  }
  deriving stock (Eq, Show)

-- | Voting anchors with neither a successful fetch nor a recorded
-- error yet. Constitution anchors are excluded — they go through a
-- distinct review process and are not fetched by this worker.
queryNewPendingVoteFetchesStmt :: Stmt.Statement Int32 [PendingVoteFetch]
queryNewPendingVoteFetchesStmt =
  Stmt.preparable sql encoder decoder
  where
    sql = T.concat
      [ "SELECT va.id, va.url, va.data_hash, va.type"
      , " FROM voting_anchor va"
      , " WHERE va.type != 'constitution'"
      , "   AND NOT EXISTS ("
      , "     SELECT 1 FROM off_chain_vote_data ocvd"
      , "     WHERE ocvd.voting_anchor_id = va.id"
      , "   )"
      , "   AND NOT EXISTS ("
      , "     SELECT 1 FROM off_chain_vote_fetch_error ocvfe"
      , "     WHERE ocvfe.voting_anchor_id = va.id"
      , "   )"
      , " ORDER BY va.id ASC"
      , " LIMIT $1"
      ]

    encoder = E.param (E.nonNullable E.int4)

    decoder = D.rowList $
      (\vaId url h ty -> PendingVoteFetch vaId url h ty Nothing 0)
        <$> idDecoder VotingAnchorId
        <*> D.column (D.nonNullable voteUrlDecoder)
        <*> D.column (D.nonNullable D.bytea)
        <*> D.column (D.nonNullable anchorTypeDecoder)

-- | Voting anchors whose most recent attempt was a recorded failure
-- with no later success. Returns the prior fetch time and retry count
-- so the caller can apply the exponential-backoff schedule.
queryRetryPendingVoteFetchesStmt :: Stmt.Statement Int32 [PendingVoteFetch]
queryRetryPendingVoteFetchesStmt =
  Stmt.preparable sql encoder decoder
  where
    sql = T.concat
      [ "WITH latest_errors AS ("
      , " SELECT MAX(id) AS max_id"
      , " FROM off_chain_vote_fetch_error"
      , " WHERE NOT EXISTS ("
      , "   SELECT 1 FROM off_chain_vote_data ocvd"
      , "   WHERE ocvd.voting_anchor_id = off_chain_vote_fetch_error.voting_anchor_id"
      , " )"
      , " GROUP BY voting_anchor_id"
      , ")"
      , "SELECT va.id, va.url, va.data_hash, va.type,"
      , "       ocvfe.fetch_time, ocvfe.retry_count"
      , " FROM voting_anchor va"
      , " INNER JOIN off_chain_vote_fetch_error ocvfe ON ocvfe.voting_anchor_id = va.id"
      , " WHERE ocvfe.id IN (SELECT max_id FROM latest_errors)"
      , "   AND va.type != 'constitution'"
      , " ORDER BY ocvfe.id ASC"
      , " LIMIT $1"
      ]

    encoder = E.param (E.nonNullable E.int4)

    decoder = D.rowList $
      (\vaId url h ty t r -> PendingVoteFetch vaId url h ty (Just t) r)
        <$> idDecoder VotingAnchorId
        <*> D.column (D.nonNullable voteUrlDecoder)
        <*> D.column (D.nonNullable D.bytea)
        <*> D.column (D.nonNullable anchorTypeDecoder)
        <*> D.column (D.nonNullable (localTimeToUTC utc <$> D.timestamp))
        <*> D.column (D.nonNullable (fromIntegral <$> D.int8))

-- ---------------------------------------------------------------------------
-- * Retry-count lookup
-- ---------------------------------------------------------------------------

-- | The highest @retry_count@ already recorded for a voting anchor,
-- or 'Nothing' if no error row exists. The worker bumps the returned
-- value by one when inserting the next failure row.
selectMaxVoteRetryCountStmt :: Stmt.Statement VotingAnchorId (Maybe Word64)
selectMaxVoteRetryCountStmt =
  Stmt.preparable sql encoder decoder
  where
    sql =
      "SELECT MAX(retry_count) FROM off_chain_vote_fetch_error \
      \WHERE voting_anchor_id = $1"

    encoder = idEncoder getVotingAnchorId

    decoder = D.singleRow $
      D.column (D.nullable (fromIntegral <$> D.int8))
