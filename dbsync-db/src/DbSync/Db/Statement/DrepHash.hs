{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @drep_hash@ dedup table.
--
-- The unique constraint is @(raw, has_script)@. @raw@ is NULL for
-- the two abstract DReps; @raw IS NOT DISTINCT FROM $1@ handles the
-- NULL case alongside concrete-hash matches.
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

-- | Look up @drep_hash.id@ by @(raw, has_script)@. @raw@ may be NULL
-- for the abstract DReps; @IS NOT DISTINCT FROM@ handles that.
queryDrepHashIdStmt :: Stmt.Statement (Maybe ByteString, Bool) (Maybe DrepHashId)
queryDrepHashIdStmt =
  Stmt.preparable sql encoder (D.rowMaybe (idDecoder DrepHashId))
  where
    sql =
      "SELECT id FROM drep_hash \
      \WHERE raw IS NOT DISTINCT FROM $1 AND has_script = $2"
    encoder = (fst >$< E.param (E.nullable E.bytea))
           <> (snd >$< E.param (E.nonNullable E.bool))
