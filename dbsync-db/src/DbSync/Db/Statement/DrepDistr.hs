-- | Hasql 'Statement' bindings for the @drep_distr@ leaf table.
--
-- Written by 'runGovernanceBoundary' from the pulsing snapshot at
-- each epoch boundary.
module DbSync.Db.Statement.DrepDistr
  ( insertDrepDistrRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Governance
  ( DrepDistr
  , drepDistrEncoder
  , drepDistrTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertDrepDistrRowStmt :: Stmt.Statement DrepDistr ()
insertDrepDistrRowStmt =
  Stmt.preparable
    (insertRowSql drepDistrTableDef)
    drepDistrEncoder
    D.noResult
