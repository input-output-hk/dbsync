-- | Hasql 'Statement' bindings for the @script@ dedup table.
--
-- Follow resolves a script by its hash via 'queryScriptIdStmt'. On a
-- miss it allocates a new id from 'nextScriptIdStmt' and the writer
-- inserts via 'insertScriptRowStmt'.
module DbSync.Db.Statement.Script
  ( -- * Inserts
    insertScriptRowStmt

    -- * ID allocation
  , nextScriptIdStmt

    -- * Lookups
  , queryScriptIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (ScriptId (..), idEncoder)
import DbSync.Db.Schema.ScriptsDatums (Script, scriptEncoder, scriptTableDef)
import DbSync.Db.Statement.Common
  ( LookupColumn (..)
  , insertRowSql
  , nextIdStmt
  , queryIdByColumnStmt
  )

insertScriptRowStmt :: Stmt.Statement (ScriptId, Script) ()
insertScriptRowStmt =
  Stmt.preparable (insertRowSql scriptTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getScriptId)
           <> (snd >$< scriptEncoder)

nextScriptIdStmt :: Stmt.Statement () ScriptId
nextScriptIdStmt = nextIdStmt scriptTableDef ScriptId

queryScriptIdStmt :: Stmt.Statement ByteString (Maybe ScriptId)
queryScriptIdStmt = queryIdByColumnStmt scriptTableDef ByHash ScriptId
