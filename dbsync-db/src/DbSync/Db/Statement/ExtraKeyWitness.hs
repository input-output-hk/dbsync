-- | Hasql 'Statement' bindings for the @extra_key_witness@ leaf table.
--
-- @extra_key_witness@ is an IDENTITY leaf: PostgreSQL fills the @id@
-- column, so the insert takes just the row payload.
module DbSync.Db.Statement.ExtraKeyWitness
  ( insertExtraKeyWitnessRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.ScriptsDatums
  ( ExtraKeyWitness
  , extraKeyWitnessEncoder
  , extraKeyWitnessTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertExtraKeyWitnessRowStmt :: Stmt.Statement ExtraKeyWitness ()
insertExtraKeyWitnessRowStmt =
  Stmt.preparable
    (insertRowSql extraKeyWitnessTableDef)
    extraKeyWitnessEncoder
    D.noResult
