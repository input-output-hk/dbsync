-- | Hasql 'Statement' bindings for the @committee_de_registration@ leaf table.
module DbSync.Db.Statement.CommitteeDeRegistration
  ( insertCommitteeDeRegistrationRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Governance
  ( CommitteeDeRegistration
  , committeeDeRegistrationEncoder
  , committeeDeRegistrationTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertCommitteeDeRegistrationRowStmt :: Stmt.Statement CommitteeDeRegistration ()
insertCommitteeDeRegistrationRowStmt =
  Stmt.preparable
    (insertRowSql committeeDeRegistrationTableDef)
    committeeDeRegistrationEncoder
    D.noResult
