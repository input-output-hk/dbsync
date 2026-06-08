{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @drep_hash@ dedup table.
module DbSync.Db.Statement.DrepHash
  ( insertDrepHashRowStmt
  , nextDrepHashIdStmt
  , queryDrepHashIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Governance (DrepHash, drepHashEncoder, drepHashTableDef)
import DbSync.Db.Schema.Ids (DrepHashId (..), idDecoder, idEncoder)
import DbSync.Db.Statement.Common (insertRowSql, nextIdStmt)

insertDrepHashRowStmt :: Stmt.Statement (DrepHashId, DrepHash) ()
insertDrepHashRowStmt =
  Stmt.preparable (insertRowSql drepHashTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getDrepHashId)
           <> (snd >$< drepHashEncoder)

nextDrepHashIdStmt :: Stmt.Statement () DrepHashId
nextDrepHashIdStmt = nextIdStmt drepHashTableDef DrepHashId

-- | Look up @drep_hash.id@ by @(raw, has_script, view)@. @view@ is
-- required to disambiguate the two abstract DReps, which both have
-- @raw=NULL@ and @has_script=FALSE@.
queryDrepHashIdStmt :: Stmt.Statement (Maybe ByteString, Bool, Text) (Maybe DrepHashId)
queryDrepHashIdStmt =
  Stmt.preparable sql encoder (D.rowMaybe (idDecoder DrepHashId))
  where
    sql =
      "SELECT id FROM drep_hash \
      \WHERE raw IS NOT DISTINCT FROM $1 AND has_script = $2 AND view = $3"
    encoder = ((\(r, _, _) -> r) >$< E.param (E.nullable E.bytea))
           <> ((\(_, s, _) -> s) >$< E.param (E.nonNullable E.bool))
           <> ((\(_, _, v) -> v) >$< E.param (E.nonNullable E.text))
