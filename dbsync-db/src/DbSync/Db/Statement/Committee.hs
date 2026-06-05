-- | Hasql 'Statement' bindings for the @committee@ table.
--
-- Counter-managed FK target: @committee_member.committee_id@ and
-- @epoch_state.committee_id@ reference it.
module DbSync.Db.Statement.Committee
  ( insertCommitteeRowStmt
  , nextCommitteeIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Governance
  ( Committee
  , committeeEncoder
  , committeeTableDef
  )
import DbSync.Db.Schema.Ids (CommitteeId (..), idEncoder)
import DbSync.Db.Statement.Common (insertRowSql, nextIdStmt)

insertCommitteeRowStmt :: Stmt.Statement (CommitteeId, Committee) ()
insertCommitteeRowStmt =
  Stmt.preparable (insertRowSql committeeTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getCommitteeId)
           <> (snd >$< committeeEncoder)

nextCommitteeIdStmt :: Stmt.Statement () CommitteeId
nextCommitteeIdStmt = nextIdStmt committeeTableDef CommitteeId
