-- | Hasql 'Statement' bindings for the @ada_pots@ table.
module DbSync.Db.Statement.AdaPots
  ( insertAdaPotsRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.AdaPots
  ( AdaPots
  , adaPotsEncoder
  , adaPotsTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertAdaPotsRowStmt :: Stmt.Statement AdaPots ()
insertAdaPotsRowStmt =
  Stmt.preparable (insertRowSql adaPotsTableDef) adaPotsEncoder D.noResult
