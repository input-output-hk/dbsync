{-# LANGUAGE OverloadedStrings #-}

-- | Bulk ID allocation via @SELECT nextval@.
--
-- 'FollowingChainTip' pre-allocates every assignable ID a block
-- will need in one libpq pipeline round-trip. The per-sequence step
-- is one @SELECT nextval(seq) FROM generate_series(1, $1)@ that
-- returns @$1@ freshly allocated ids as a typed list.
--
-- The sequence name is derived from the table by appending
-- @_id_seq@, matching the convention created during the
-- 'PreparingForVolatileTail' schema-mode flip.
module DbSync.Db.Statement.IdAllocator
  ( bulkNextvalStmt
  , bulkNextvalSql
  ) where

import Cardano.Prelude

import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Types (TableDef (..))

-- | @SELECT nextval('<table>_id_seq') FROM generate_series(1, $1)@
-- returning @$1@ freshly allocated ids wrapped through @ctor@.
bulkNextvalStmt
  :: TableDef
  -> (Int64 -> a)       -- ^ ID constructor (newtype wrapper).
  -> Stmt.Statement Int32 [a]
bulkNextvalStmt td ctor =
  Stmt.preparable
    (bulkNextvalSql td)
    (E.param (E.nonNullable E.int4))
    (D.rowList (ctor <$> D.column (D.nonNullable D.int8)))

-- | The bare @SELECT nextval('<table>_id_seq') FROM
-- generate_series(1, $1)@ SQL, exported separately so callers that
-- want a different codec (e.g. an untyped 'Int64' list) can reuse
-- the SQL.
bulkNextvalSql :: TableDef -> Text
bulkNextvalSql td =
  "SELECT nextval('" <> tdName td <> "_id_seq') \
  \FROM generate_series(1, $1)"
