-- | Hasql 'Statement' bindings for the @datum@ dedup table.
--
-- Follow resolves a datum by its 32-byte hash via 'queryDatumIdStmt'.
-- On a miss it allocates a new id from 'nextDatumIdStmt' and the
-- writer inserts via 'insertDatumRowStmt'.
module DbSync.Db.Statement.Datum
  ( -- * Inserts
    insertDatumRowStmt

    -- * ID allocation
  , nextDatumIdStmt

    -- * Lookups
  , queryDatumIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (DatumId (..), idEncoder)
import DbSync.Db.Schema.ScriptsDatums (Datum, datumEncoder, datumTableDef)
import DbSync.Db.Statement.Common
  ( LookupColumn (..)
  , insertRowSql
  , nextIdStmt
  , queryIdByColumnStmt
  )

insertDatumRowStmt :: Stmt.Statement (DatumId, Datum) ()
insertDatumRowStmt =
  Stmt.preparable (insertRowSql datumTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getDatumId)
           <> (snd >$< datumEncoder)

nextDatumIdStmt :: Stmt.Statement () DatumId
nextDatumIdStmt = nextIdStmt datumTableDef DatumId

queryDatumIdStmt :: Stmt.Statement ByteString (Maybe DatumId)
queryDatumIdStmt = queryIdByColumnStmt datumTableDef ByHash DatumId
