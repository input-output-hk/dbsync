-- | Hasql 'Statement' binding for the @treasury@ table.
module DbSync.Db.Statement.Treasury
  ( insertTreasuryRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.EpochBoundary
  ( Treasury
  , treasuryEncoder
  , treasuryTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertTreasuryRowStmt :: Stmt.Statement Treasury ()
insertTreasuryRowStmt =
  Stmt.preparable (insertRowSql treasuryTableDef) treasuryEncoder D.noResult
