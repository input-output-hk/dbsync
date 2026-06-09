-- | COPY writers for tables owned by the @off_chain_votes@ extractor.
module DbSync.Phase.Ingest.Writer.OffChainVotes
  ( writeOffChainVoteDataCopy
  , writeOffChainVoteGovActionDataCopy
  , writeOffChainVoteDrepDataCopy
  , writeOffChainVoteAuthorCopy
  , writeOffChainVoteReferenceCopy
  , writeOffChainVoteExternalUpdateCopy
  , writeOffChainVoteFetchErrorCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.OffChainVote
  ( OffChainVoteAuthor
  , OffChainVoteData
  , OffChainVoteDrepData
  , OffChainVoteExternalUpdate
  , OffChainVoteFetchError
  , OffChainVoteGovActionData
  , OffChainVoteReference
  , encodeOffChainVoteAuthorCopy
  , encodeOffChainVoteDataCopy
  , encodeOffChainVoteDrepDataCopy
  , encodeOffChainVoteExternalUpdateCopy
  , encodeOffChainVoteFetchErrorCopy
  , encodeOffChainVoteGovActionDataCopy
  , encodeOffChainVoteReferenceCopy
  , offChainVoteAuthorTableDef
  , offChainVoteDataTableDef
  , offChainVoteDrepDataTableDef
  , offChainVoteExternalUpdateTableDef
  , offChainVoteFetchErrorTableDef
  , offChainVoteGovActionDataTableDef
  , offChainVoteReferenceTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))

writeOffChainVoteDataCopy :: LoaderStream -> OffChainVoteData -> IO ()
writeOffChainVoteDataCopy ls d =
  lsWriteRow ls (tdName offChainVoteDataTableDef) (encodeOffChainVoteDataCopy d)

writeOffChainVoteGovActionDataCopy :: LoaderStream -> OffChainVoteGovActionData -> IO ()
writeOffChainVoteGovActionDataCopy ls g =
  lsWriteRow ls (tdName offChainVoteGovActionDataTableDef) (encodeOffChainVoteGovActionDataCopy g)

writeOffChainVoteDrepDataCopy :: LoaderStream -> OffChainVoteDrepData -> IO ()
writeOffChainVoteDrepDataCopy ls d =
  lsWriteRow ls (tdName offChainVoteDrepDataTableDef) (encodeOffChainVoteDrepDataCopy d)

writeOffChainVoteAuthorCopy :: LoaderStream -> OffChainVoteAuthor -> IO ()
writeOffChainVoteAuthorCopy ls a =
  lsWriteRow ls (tdName offChainVoteAuthorTableDef) (encodeOffChainVoteAuthorCopy a)

writeOffChainVoteReferenceCopy :: LoaderStream -> OffChainVoteReference -> IO ()
writeOffChainVoteReferenceCopy ls r =
  lsWriteRow ls (tdName offChainVoteReferenceTableDef) (encodeOffChainVoteReferenceCopy r)

writeOffChainVoteExternalUpdateCopy :: LoaderStream -> OffChainVoteExternalUpdate -> IO ()
writeOffChainVoteExternalUpdateCopy ls u =
  lsWriteRow ls (tdName offChainVoteExternalUpdateTableDef) (encodeOffChainVoteExternalUpdateCopy u)

writeOffChainVoteFetchErrorCopy :: LoaderStream -> OffChainVoteFetchError -> IO ()
writeOffChainVoteFetchErrorCopy ls e =
  lsWriteRow ls (tdName offChainVoteFetchErrorTableDef) (encodeOffChainVoteFetchErrorCopy e)
