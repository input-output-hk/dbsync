-- | Hasql 'Statement' bindings for the @constitution@ table.
--
-- Counter-managed FK target: @epoch_state.constitution_id@ references it.
module DbSync.Db.Statement.Constitution
  ( insertConstitutionRowStmt
  , nextConstitutionIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Governance
  ( Constitution
  , constitutionEncoder
  , constitutionTableDef
  )
import DbSync.Db.Schema.Ids (ConstitutionId (..), idEncoder)
import DbSync.Db.Statement.Common (insertRowSql, nextIdStmt)

insertConstitutionRowStmt :: Stmt.Statement (ConstitutionId, Constitution) ()
insertConstitutionRowStmt =
  Stmt.preparable (insertRowSql constitutionTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getConstitutionId)
           <> (snd >$< constitutionEncoder)

nextConstitutionIdStmt :: Stmt.Statement () ConstitutionId
nextConstitutionIdStmt = nextIdStmt constitutionTableDef ConstitutionId
