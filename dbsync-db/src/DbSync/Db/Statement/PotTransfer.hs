-- | Hasql 'Statement' binding for the @pot_transfer@ table.
module DbSync.Db.Statement.PotTransfer
  ( insertPotTransferRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.EpochBoundary
  ( PotTransfer
  , potTransferEncoder
  , potTransferTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertPotTransferRowStmt :: Stmt.Statement PotTransfer ()
insertPotTransferRowStmt =
  Stmt.preparable (insertRowSql potTransferTableDef) potTransferEncoder D.noResult
