-- | Hasql writers for tables owned by the @off_chain_votes@ extractor.
module DbSync.Phase.Following.Writer.OffChainVotes
  ( writeOffChainVoteDataConn
  , writeOffChainVoteDataBuf
  , writeOffChainVoteGovActionDataConn
  , writeOffChainVoteGovActionDataBuf
  , writeOffChainVoteDrepDataConn
  , writeOffChainVoteDrepDataBuf
  , writeOffChainVoteAuthorConn
  , writeOffChainVoteAuthorBuf
  , writeOffChainVoteReferenceConn
  , writeOffChainVoteReferenceBuf
  , writeOffChainVoteExternalUpdateConn
  , writeOffChainVoteExternalUpdateBuf
  , writeOffChainVoteFetchErrorConn
  , writeOffChainVoteFetchErrorBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.OffChainVote
  ( OffChainVoteAuthor
  , OffChainVoteData
  , OffChainVoteDrepData
  , OffChainVoteExternalUpdate
  , OffChainVoteFetchError
  , OffChainVoteGovActionData
  , OffChainVoteReference
  )
import DbSync.Db.Statement.OffChainVote
  ( insertOffChainVoteAuthorRowStmt
  , insertOffChainVoteDataRowStmt
  , insertOffChainVoteDrepDataRowStmt
  , insertOffChainVoteExternalUpdateRowStmt
  , insertOffChainVoteFetchErrorRowStmt
  , insertOffChainVoteGovActionDataRowStmt
  , insertOffChainVoteReferenceRowStmt
  )
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

writeOffChainVoteDataConn :: Conn.Connection -> OffChainVoteData -> IO ()
writeOffChainVoteDataConn conn d = runConn conn d insertOffChainVoteDataRowStmt

writeOffChainVoteDataBuf :: WriteBuffer -> OffChainVoteData -> IO ()
writeOffChainVoteDataBuf buf d = queueBuf buf d insertOffChainVoteDataRowStmt

writeOffChainVoteGovActionDataConn :: Conn.Connection -> OffChainVoteGovActionData -> IO ()
writeOffChainVoteGovActionDataConn conn g = runConn conn g insertOffChainVoteGovActionDataRowStmt

writeOffChainVoteGovActionDataBuf :: WriteBuffer -> OffChainVoteGovActionData -> IO ()
writeOffChainVoteGovActionDataBuf buf g = queueBuf buf g insertOffChainVoteGovActionDataRowStmt

writeOffChainVoteDrepDataConn :: Conn.Connection -> OffChainVoteDrepData -> IO ()
writeOffChainVoteDrepDataConn conn d = runConn conn d insertOffChainVoteDrepDataRowStmt

writeOffChainVoteDrepDataBuf :: WriteBuffer -> OffChainVoteDrepData -> IO ()
writeOffChainVoteDrepDataBuf buf d = queueBuf buf d insertOffChainVoteDrepDataRowStmt

writeOffChainVoteAuthorConn :: Conn.Connection -> OffChainVoteAuthor -> IO ()
writeOffChainVoteAuthorConn conn a = runConn conn a insertOffChainVoteAuthorRowStmt

writeOffChainVoteAuthorBuf :: WriteBuffer -> OffChainVoteAuthor -> IO ()
writeOffChainVoteAuthorBuf buf a = queueBuf buf a insertOffChainVoteAuthorRowStmt

writeOffChainVoteReferenceConn :: Conn.Connection -> OffChainVoteReference -> IO ()
writeOffChainVoteReferenceConn conn r = runConn conn r insertOffChainVoteReferenceRowStmt

writeOffChainVoteReferenceBuf :: WriteBuffer -> OffChainVoteReference -> IO ()
writeOffChainVoteReferenceBuf buf r = queueBuf buf r insertOffChainVoteReferenceRowStmt

writeOffChainVoteExternalUpdateConn :: Conn.Connection -> OffChainVoteExternalUpdate -> IO ()
writeOffChainVoteExternalUpdateConn conn u = runConn conn u insertOffChainVoteExternalUpdateRowStmt

writeOffChainVoteExternalUpdateBuf :: WriteBuffer -> OffChainVoteExternalUpdate -> IO ()
writeOffChainVoteExternalUpdateBuf buf u = queueBuf buf u insertOffChainVoteExternalUpdateRowStmt

writeOffChainVoteFetchErrorConn :: Conn.Connection -> OffChainVoteFetchError -> IO ()
writeOffChainVoteFetchErrorConn conn e = runConn conn e insertOffChainVoteFetchErrorRowStmt

writeOffChainVoteFetchErrorBuf :: WriteBuffer -> OffChainVoteFetchError -> IO ()
writeOffChainVoteFetchErrorBuf buf e = queueBuf buf e insertOffChainVoteFetchErrorRowStmt
