-- | Hasql 'Statement' binding for the @reserve@ table.
module DbSync.Db.Statement.Reserve
  ( insertReserveRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.EpochBoundary
  ( Reserve
  , reserveEncoder
  , reserveTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertReserveRowStmt :: Stmt.Statement Reserve ()
insertReserveRowStmt =
  Stmt.preparable (insertRowSql reserveTableDef) reserveEncoder D.noResult
