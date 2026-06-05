{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @voting_anchor@ dedup table.
--
-- The unique constraint is @(data_hash, url, type)@; 'queryVotingAnchorIdStmt'
-- takes the same triple.
module DbSync.Db.Statement.VotingAnchor
  ( insertVotingAnchorRowStmt
  , nextVotingAnchorIdStmt
  , queryVotingAnchorIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Governance
  ( VotingAnchor
  , votingAnchorEncoder
  , votingAnchorTableDef
  )
import DbSync.Db.Schema.Ids (VotingAnchorId (..), idDecoder, idEncoder)
import DbSync.Db.Statement.Common (insertRowSql, nextIdStmt)
import DbSync.Db.Types (AnchorType, anchorTypeEncoder)

insertVotingAnchorRowStmt :: Stmt.Statement (VotingAnchorId, VotingAnchor) ()
insertVotingAnchorRowStmt =
  Stmt.preparable (insertRowSql votingAnchorTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getVotingAnchorId)
           <> (snd >$< votingAnchorEncoder)

nextVotingAnchorIdStmt :: Stmt.Statement () VotingAnchorId
nextVotingAnchorIdStmt = nextIdStmt votingAnchorTableDef VotingAnchorId

-- | Look up @voting_anchor.id@ by the @(url, data_hash, type)@ triple
-- that backs the table's unique constraint.
queryVotingAnchorIdStmt
  :: Stmt.Statement (Text, ByteString, AnchorType) (Maybe VotingAnchorId)
queryVotingAnchorIdStmt =
  Stmt.preparable sql encoder (D.rowMaybe (idDecoder VotingAnchorId))
  where
    sql =
      "SELECT id FROM voting_anchor \
      \WHERE url = $1 AND data_hash = $2 AND type = $3"
    encoder =
         ((\(u, _, _) -> u) >$< E.param (E.nonNullable E.text))
      <> ((\(_, h, _) -> h) >$< E.param (E.nonNullable E.bytea))
      <> ((\(_, _, t) -> t) >$< E.param (E.nonNullable anchorTypeEncoder))
