{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @committee_hash@ dedup table.
--
-- The unique constraint is @(raw, has_script)@. Both columns are
-- non-nullable so 'queryCommitteeHashIdStmt' uses plain @=@.
module DbSync.Db.Statement.CommitteeHash
  ( insertCommitteeHashRowStmt
  , nextCommitteeHashIdStmt
  , queryCommitteeHashIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Governance
  ( CommitteeHash
  , committeeHashEncoder
  , committeeHashTableDef
  )
import DbSync.Db.Schema.Ids (CommitteeHashId (..), idDecoder, idEncoder)
import DbSync.Db.Statement.Common (insertRowSql, nextIdStmt)

insertCommitteeHashRowStmt :: Stmt.Statement (CommitteeHashId, CommitteeHash) ()
insertCommitteeHashRowStmt =
  Stmt.preparable (insertRowSql committeeHashTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getCommitteeHashId)
           <> (snd >$< committeeHashEncoder)

nextCommitteeHashIdStmt :: Stmt.Statement () CommitteeHashId
nextCommitteeHashIdStmt = nextIdStmt committeeHashTableDef CommitteeHashId

queryCommitteeHashIdStmt :: Stmt.Statement (ByteString, Bool) (Maybe CommitteeHashId)
queryCommitteeHashIdStmt =
  Stmt.preparable sql encoder (D.rowMaybe (idDecoder CommitteeHashId))
  where
    sql = "SELECT id FROM committee_hash WHERE raw = $1 AND has_script = $2"
    encoder = (fst >$< E.param (E.nonNullable E.bytea))
           <> (snd >$< E.param (E.nonNullable E.bool))
