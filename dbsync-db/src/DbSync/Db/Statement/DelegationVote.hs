-- | Hasql 'Statement' bindings for the @delegation_vote@ leaf table.
module DbSync.Db.Statement.DelegationVote
  ( insertDelegationVoteRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Governance
  ( DelegationVote
  , delegationVoteEncoder
  , delegationVoteTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertDelegationVoteRowStmt :: Stmt.Statement DelegationVote ()
insertDelegationVoteRowStmt =
  Stmt.preparable
    (insertRowSql delegationVoteTableDef)
    delegationVoteEncoder
    D.noResult
