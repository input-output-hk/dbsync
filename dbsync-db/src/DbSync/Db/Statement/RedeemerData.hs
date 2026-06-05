-- | Hasql 'Statement' bindings for the @redeemer_data@ dedup table.
--
-- Follow resolves a redeemer-data payload by its 32-byte hash via
-- 'queryRedeemerDataIdStmt'. On a miss it allocates a new id from
-- 'nextRedeemerDataIdStmt' and the writer inserts via
-- 'insertRedeemerDataRowStmt'.
module DbSync.Db.Statement.RedeemerData
  ( -- * Inserts
    insertRedeemerDataRowStmt

    -- * ID allocation
  , nextRedeemerDataIdStmt

    -- * Lookups
  , queryRedeemerDataIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (RedeemerDataId (..), idEncoder)
import DbSync.Db.Schema.ScriptsDatums
  ( RedeemerData
  , redeemerDataEncoder
  , redeemerDataTableDef
  )
import DbSync.Db.Statement.Common
  ( LookupColumn (..)
  , insertRowSql
  , nextIdStmt
  , queryIdByColumnStmt
  )

insertRedeemerDataRowStmt :: Stmt.Statement (RedeemerDataId, RedeemerData) ()
insertRedeemerDataRowStmt =
  Stmt.preparable (insertRowSql redeemerDataTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getRedeemerDataId)
           <> (snd >$< redeemerDataEncoder)

nextRedeemerDataIdStmt :: Stmt.Statement () RedeemerDataId
nextRedeemerDataIdStmt = nextIdStmt redeemerDataTableDef RedeemerDataId

queryRedeemerDataIdStmt :: Stmt.Statement ByteString (Maybe RedeemerDataId)
queryRedeemerDataIdStmt =
  queryIdByColumnStmt redeemerDataTableDef ByHash RedeemerDataId
