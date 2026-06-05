-- | Hasql 'Statement' bindings for the @treasury_withdrawal@ leaf table.
module DbSync.Db.Statement.TreasuryWithdrawal
  ( insertTreasuryWithdrawalRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Governance
  ( TreasuryWithdrawal
  , treasuryWithdrawalEncoder
  , treasuryWithdrawalTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertTreasuryWithdrawalRowStmt :: Stmt.Statement TreasuryWithdrawal ()
insertTreasuryWithdrawalRowStmt =
  Stmt.preparable
    (insertRowSql treasuryWithdrawalTableDef)
    treasuryWithdrawalEncoder
    D.noResult
