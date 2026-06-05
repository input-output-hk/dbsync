-- | Hasql 'Statement' bindings for the @event_info@ table.
--
-- Counter-managed FK target: @voting_procedure.invalid@ references it.
-- Currently rows are never written (matches the original cardano-db-sync
-- behaviour); the statements are present so the Follow Writer can flip
-- on when a population path lands.
module DbSync.Db.Statement.EventInfo
  ( insertEventInfoRowStmt
  , nextEventInfoIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Governance
  ( EventInfo
  , eventInfoEncoder
  , eventInfoTableDef
  )
import DbSync.Db.Schema.Ids (EventInfoId (..), idEncoder)
import DbSync.Db.Statement.Common (insertRowSql, nextIdStmt)

insertEventInfoRowStmt :: Stmt.Statement (EventInfoId, EventInfo) ()
insertEventInfoRowStmt =
  Stmt.preparable (insertRowSql eventInfoTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getEventInfoId)
           <> (snd >$< eventInfoEncoder)

nextEventInfoIdStmt :: Stmt.Statement () EventInfoId
nextEventInfoIdStmt = nextIdStmt eventInfoTableDef EventInfoId
