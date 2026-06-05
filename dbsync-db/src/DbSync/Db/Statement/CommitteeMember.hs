-- | Hasql 'Statement' bindings for the @committee_member@ leaf table.
module DbSync.Db.Statement.CommitteeMember
  ( insertCommitteeMemberRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Governance
  ( CommitteeMember
  , committeeMemberEncoder
  , committeeMemberTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertCommitteeMemberRowStmt :: Stmt.Statement CommitteeMember ()
insertCommitteeMemberRowStmt =
  Stmt.preparable
    (insertRowSql committeeMemberTableDef)
    committeeMemberEncoder
    D.noResult
