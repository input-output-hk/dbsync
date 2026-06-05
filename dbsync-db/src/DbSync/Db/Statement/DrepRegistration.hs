-- | Hasql 'Statement' bindings for the @drep_registration@ leaf table.
module DbSync.Db.Statement.DrepRegistration
  ( insertDrepRegistrationRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Governance
  ( DrepRegistration
  , drepRegistrationEncoder
  , drepRegistrationTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertDrepRegistrationRowStmt :: Stmt.Statement DrepRegistration ()
insertDrepRegistrationRowStmt =
  Stmt.preparable
    (insertRowSql drepRegistrationTableDef)
    drepRegistrationEncoder
    D.noResult
