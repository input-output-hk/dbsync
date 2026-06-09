{-# LANGUAGE OverloadedStrings #-}

-- | Bulk ID allocation: @SELECT nextval('<table>_id_seq') FROM
-- generate_series(1, $1)@ returns @$1@ ids in one libpq pipeline
-- round-trip. Used by 'FollowingChainTip' to pre-allocate every
-- assignable id a block needs.
module DbSync.Db.Statement.IdAllocator
  ( bulkNextvalStmt
  , bulkNextvalSql
  ) where

import Cardano.Prelude

import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Types (TableDef (..))

bulkNextvalStmt
  :: TableDef
  -> (Int64 -> a)
  -> Stmt.Statement Int32 [a]
bulkNextvalStmt td ctor =
  Stmt.preparable
    (bulkNextvalSql td)
    (E.param (E.nonNullable E.int4))
    (D.rowList (ctor <$> D.column (D.nonNullable D.int8)))

-- | Exported separately so callers that want a different codec
-- (e.g. an untyped 'Int64' list) can reuse the SQL.
bulkNextvalSql :: TableDef -> Text
bulkNextvalSql td =
  "SELECT nextval('" <> tdName td <> "_id_seq') \
  \FROM generate_series(1, $1)"
