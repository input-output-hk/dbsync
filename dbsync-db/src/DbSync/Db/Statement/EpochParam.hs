-- | Hasql 'Statement' bindings for the @epoch_param@ table.
module DbSync.Db.Statement.EpochParam
  ( insertEpochParamRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.EpochBoundary
  ( EpochParam
  , epochParamEncoder
  , epochParamTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertEpochParamRowStmt :: Stmt.Statement EpochParam ()
insertEpochParamRowStmt =
  Stmt.preparable (insertRowSql epochParamTableDef) epochParamEncoder D.noResult
