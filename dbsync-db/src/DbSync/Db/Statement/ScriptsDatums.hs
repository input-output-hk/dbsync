-- | Hasql 'Statement' bindings for the @scripts_datums@ extractor
-- tables: @datum@, @script@, @redeemer@, @redeemer_data@,
-- @extra_key_witness@.
--
-- @datum@, @script@ and @redeemer_data@ are dedup-keyed on a 32-byte
-- @hash@: Follow runs the @query…IdStmt@ first; on a miss it
-- allocates a fresh id from the matching @next…IdStmt@. @redeemer@ is
-- counter-managed (no dedup, allocator per row). @extra_key_witness@
-- is an IDENTITY leaf.
module DbSync.Db.Statement.ScriptsDatums
  ( -- * datum
    insertDatumRowStmt
  , nextDatumIdStmt
  , queryDatumIdStmt

    -- * script
  , insertScriptRowStmt
  , nextScriptIdStmt
  , queryScriptIdStmt

    -- * redeemer
  , insertRedeemerRowStmt
  , nextRedeemerIdStmt

    -- * redeemer_data
  , insertRedeemerDataRowStmt
  , nextRedeemerDataIdStmt
  , queryRedeemerDataIdStmt

    -- * extra_key_witness
  , insertExtraKeyWitnessRowStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids
  ( DatumId (..)
  , RedeemerDataId (..)
  , RedeemerId (..)
  , ScriptId (..)
  , idEncoder
  )
import DbSync.Db.Schema.ScriptsDatums
  ( Datum
  , ExtraKeyWitness
  , Redeemer
  , RedeemerData
  , Script
  , datumEncoder
  , datumTableDef
  , extraKeyWitnessEncoder
  , extraKeyWitnessTableDef
  , redeemerDataEncoder
  , redeemerDataTableDef
  , redeemerEncoder
  , redeemerTableDef
  , scriptEncoder
  , scriptTableDef
  )
import DbSync.Db.Statement.Common
  ( LookupColumn (..)
  , insertRowSql
  , nextIdStmt
  , queryIdByColumnStmt
  )

-- ---------------------------------------------------------------------------
-- * datum
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- * script
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- * redeemer
-- ---------------------------------------------------------------------------

insertRedeemerRowStmt :: Stmt.Statement (RedeemerId, Redeemer) ()
insertRedeemerRowStmt =
  Stmt.preparable (insertRowSql redeemerTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getRedeemerId)
           <> (snd >$< redeemerEncoder)

nextRedeemerIdStmt :: Stmt.Statement () RedeemerId
nextRedeemerIdStmt = nextIdStmt redeemerTableDef RedeemerId

-- ---------------------------------------------------------------------------
-- * redeemer_data
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- * extra_key_witness
-- ---------------------------------------------------------------------------

insertExtraKeyWitnessRowStmt :: Stmt.Statement ExtraKeyWitness ()
insertExtraKeyWitnessRowStmt =
  Stmt.preparable
    (insertRowSql extraKeyWitnessTableDef)
    extraKeyWitnessEncoder
    D.noResult
