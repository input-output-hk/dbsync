-- | Hasql 'Statement' bindings for the @epoch_state@ table.
module DbSync.Db.Statement.EpochState
  ( insertEpochStateRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.EpochBoundary
  ( EpochState
  , epochStateEncoder
  , epochStateTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertEpochStateRowStmt :: Stmt.Statement EpochState ()
insertEpochStateRowStmt =
  Stmt.preparable (insertRowSql epochStateTableDef) epochStateEncoder D.noResult
