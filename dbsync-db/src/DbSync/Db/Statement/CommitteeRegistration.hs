-- | Hasql 'Statement' bindings for the @committee_registration@ leaf table.
module DbSync.Db.Statement.CommitteeRegistration
  ( insertCommitteeRegistrationRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Governance
  ( CommitteeRegistration
  , committeeRegistrationEncoder
  , committeeRegistrationTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertCommitteeRegistrationRowStmt :: Stmt.Statement CommitteeRegistration ()
insertCommitteeRegistrationRowStmt =
  Stmt.preparable
    (insertRowSql committeeRegistrationTableDef)
    committeeRegistrationEncoder
    D.noResult
