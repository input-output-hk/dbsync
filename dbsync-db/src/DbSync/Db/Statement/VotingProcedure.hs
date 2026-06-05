-- | Hasql 'Statement' bindings for the @voting_procedure@ leaf table.
module DbSync.Db.Statement.VotingProcedure
  ( insertVotingProcedureRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Governance
  ( VotingProcedure
  , votingProcedureEncoder
  , votingProcedureTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertVotingProcedureRowStmt :: Stmt.Statement VotingProcedure ()
insertVotingProcedureRowStmt =
  Stmt.preparable
    (insertRowSql votingProcedureTableDef)
    votingProcedureEncoder
    D.noResult
