{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @off_chain_votes@ extractor:
-- inserts for the seven result tables and the work-queue lookups the
-- vote worker uses to find pending or due-for-retry voting anchors.
module DbSync.Db.Statement.Worker.OffChainVote
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

import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (localTimeToUTC, utc)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Governance
  ( VotingAnchorCols (..)
  , votingAnchorCols
  , votingAnchorTableDef
  )
import DbSync.Db.Schema.Ids
  ( OffChainVoteDataId (..)
  , VotingAnchorId (..)
  , idDecoder
  , idEncoder
  )
import DbSync.Db.Schema.OffChainVote
  ( OffChainVoteAuthor
  , OffChainVoteData
  , OffChainVoteDataCols (..)
  , OffChainVoteDrepData
  , OffChainVoteExternalUpdate
  , OffChainVoteFetchError
  , OffChainVoteFetchErrorCols (..)
  , OffChainVoteGovActionData
  , OffChainVoteReference
  , offChainVoteAuthorEncoder
  , offChainVoteAuthorTableDef
  , offChainVoteDataCols
  , offChainVoteDataEncoder
  , offChainVoteDataTableDef
  , offChainVoteDrepDataEncoder
  , offChainVoteDrepDataTableDef
  , offChainVoteExternalUpdateEncoder
  , offChainVoteExternalUpdateTableDef
  , offChainVoteFetchErrorCols
  , offChainVoteFetchErrorEncoder
  , offChainVoteFetchErrorTableDef
  , offChainVoteGovActionDataEncoder
  , offChainVoteGovActionDataTableDef
  , offChainVoteReferenceEncoder
  , offChainVoteReferenceTableDef
  )
import DbSync.Db.Sql.Refs (col, qcol, table)
import DbSync.Db.Statement.Common (insertReturningIdSql, insertRowSql)
import DbSync.Db.Types (AnchorType, VoteUrl, anchorTypeDecoder, voteUrlDecoder)

-- ---------------------------------------------------------------------------
-- * Inserts
-- ---------------------------------------------------------------------------

insertOffChainVoteDataRowStmt :: Stmt.Statement OffChainVoteData ()
insertOffChainVoteDataRowStmt =
  Stmt.preparable (insertRowSql offChainVoteDataTableDef) offChainVoteDataEncoder D.noResult

-- | Returns the identity-allocated @id@ so subtable rows
-- (gov_action / drep / author / reference / external_update) can
-- carry the FK.
insertOffChainVoteDataReturningIdStmt
  :: Stmt.Statement OffChainVoteData OffChainVoteDataId
insertOffChainVoteDataReturningIdStmt =
  Stmt.preparable
    (insertReturningIdSql offChainVoteDataTableDef)
    offChainVoteDataEncoder
    (D.singleRow (idDecoder OffChainVoteDataId))

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
-- 'pvfPrevFetchTime' is 'Nothing' for never-attempted anchors and
-- @Just t@ (with matching retry count) for anchors due to retry.
data PendingVoteFetch = PendingVoteFetch
  { pvfVotingAnchorId :: !VotingAnchorId
  , pvfUrl            :: !VoteUrl
  , pvfHash           :: !ByteString
  , pvfAnchorType     :: !AnchorType
  , pvfPrevFetchTime  :: !(Maybe UTCTime)
  , pvfPrevRetryCount :: !Word64
  }
  deriving stock (Eq, Show)

-- | Anchors with neither a successful fetch nor a recorded error
-- yet. Constitution anchors are excluded — they go through a
-- distinct review process and are not fetched by this worker.
queryNewPendingVoteFetchesStmt :: Stmt.Statement Int32 [PendingVoteFetch]
queryNewPendingVoteFetchesStmt =
  Stmt.preparable sql encoder decoder
  where
    sql = mconcat
      [ "SELECT ", qcol "va" votingAnchorCols.vacId
      , ", ", qcol "va" votingAnchorCols.vacUrl
      , ", ", qcol "va" votingAnchorCols.vacDataHash
      , ", ", qcol "va" votingAnchorCols.vacType
      , " FROM ", table votingAnchorTableDef, " va"
      , " WHERE ", qcol "va" votingAnchorCols.vacType, " != 'constitution'"
      , "   AND NOT EXISTS ("
      , "     SELECT 1 FROM ", table offChainVoteDataTableDef, " ocvd"
      , "     WHERE ", qcol "ocvd" offChainVoteDataCols.ocvdcVotingAnchorId
      ,     " = ", qcol "va" votingAnchorCols.vacId
      , "   )"
      , "   AND NOT EXISTS ("
      , "     SELECT 1 FROM ", table offChainVoteFetchErrorTableDef, " ocvfe"
      , "     WHERE ", qcol "ocvfe" offChainVoteFetchErrorCols.ocvfecVotingAnchorId
      ,     " = ", qcol "va" votingAnchorCols.vacId
      , "   )"
      , " ORDER BY ", qcol "va" votingAnchorCols.vacId, " ASC"
      , " LIMIT $1"
      ]

    encoder = E.param (E.nonNullable E.int4)

    decoder = D.rowList $
      (\vaId url h ty -> PendingVoteFetch vaId url h ty Nothing 0)
        <$> idDecoder VotingAnchorId
        <*> D.column (D.nonNullable voteUrlDecoder)
        <*> D.column (D.nonNullable D.bytea)
        <*> D.column (D.nonNullable anchorTypeDecoder)

-- | Anchors whose most recent attempt was a recorded failure with no
-- later success. Returns the prior fetch time and retry count so the
-- caller can apply exponential backoff.
queryRetryPendingVoteFetchesStmt :: Stmt.Statement Int32 [PendingVoteFetch]
queryRetryPendingVoteFetchesStmt =
  Stmt.preparable sql encoder decoder
  where
    sql = mconcat
      [ "WITH latest_errors AS ("
      , " SELECT MAX(", col offChainVoteFetchErrorCols.ocvfecId, ") AS max_id"
      , " FROM ", table offChainVoteFetchErrorTableDef
      , " WHERE NOT EXISTS ("
      , "   SELECT 1 FROM ", table offChainVoteDataTableDef, " ocvd"
      , "   WHERE ", qcol "ocvd" offChainVoteDataCols.ocvdcVotingAnchorId
      ,   " = ", qcol (table offChainVoteFetchErrorTableDef) offChainVoteFetchErrorCols.ocvfecVotingAnchorId
      , " )"
      , " GROUP BY ", col offChainVoteFetchErrorCols.ocvfecVotingAnchorId
      , ")"
      , "SELECT ", qcol "va" votingAnchorCols.vacId
      , ", ", qcol "va" votingAnchorCols.vacUrl
      , ", ", qcol "va" votingAnchorCols.vacDataHash
      , ", ", qcol "va" votingAnchorCols.vacType
      , ", ", qcol "ocvfe" offChainVoteFetchErrorCols.ocvfecFetchTime
      , ", ", qcol "ocvfe" offChainVoteFetchErrorCols.ocvfecRetryCount
      , " FROM ", table votingAnchorTableDef, " va"
      , " INNER JOIN ", table offChainVoteFetchErrorTableDef, " ocvfe"
      ,   " ON ", qcol "ocvfe" offChainVoteFetchErrorCols.ocvfecVotingAnchorId
      ,   " = ", qcol "va" votingAnchorCols.vacId
      , " WHERE ", qcol "ocvfe" offChainVoteFetchErrorCols.ocvfecId, " IN (SELECT max_id FROM latest_errors)"
      , "   AND ", qcol "va" votingAnchorCols.vacType, " != 'constitution'"
      , " ORDER BY ", qcol "ocvfe" offChainVoteFetchErrorCols.ocvfecId, " ASC"
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

-- | Highest @retry_count@ recorded for a voting anchor, or 'Nothing'
-- if no error row exists. The worker bumps the value by one when
-- inserting the next failure row.
selectMaxVoteRetryCountStmt :: Stmt.Statement VotingAnchorId (Maybe Word64)
selectMaxVoteRetryCountStmt =
  Stmt.preparable sql encoder decoder
  where
    sql = mconcat
      [ "SELECT MAX(", col offChainVoteFetchErrorCols.ocvfecRetryCount, ")"
      , " FROM ", table offChainVoteFetchErrorTableDef
      , " WHERE ", col offChainVoteFetchErrorCols.ocvfecVotingAnchorId, " = $1"
      ]

    encoder = idEncoder getVotingAnchorId

    decoder = D.singleRow $
      D.column (D.nullable (fromIntegral <$> D.int8))
