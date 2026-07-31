{-# LANGUAGE OverloadedStrings #-}

-- | Fill @redeemer.script_hash@ for spend redeemers.
--
-- Every other purpose resolves its hash from the tx body while
-- parsing. A spend redeemer points at a 'TxIn', so its hash is the
-- payment credential of the /spent/ output — reachable only through
-- @tx_in.tx_out_id@, which Ingest leaves unresolved.
--
-- Both variants share one UPDATE body. Prep runs the unscoped form
-- once over the ingested range; Follow appends the id-scoped form to
-- each block's pipeline, after the block's @tx_in@ and @redeemer@
-- INSERTs are queued.
module DbSync.Db.Statement.Worker.RedeemerScriptHash
  ( -- * Prepared 'Stmt.Statement' values
    backfillSpendScriptHashStmt
  , fillSpendScriptHashesStmt

    -- * Raw SQL strings
    --
    -- Exported so tests can feed them to @EXPLAIN@ and assert on the
    -- plan shape.
  , backfillSpendScriptHashSql
  , fillSpendScriptHashesSql
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Address (AddressCols (..), addressCols, addressTableDef)
import DbSync.Db.Schema.Ids (RedeemerId (..))
import DbSync.Db.Schema.ScriptsDatums
  ( RedeemerCols (..)
  , redeemerCols
  , redeemerTableDef
  )
import DbSync.Db.Schema.UTxO
  ( TxInCols (..)
  , TxOutCols (..)
  , txInCols
  , txInTableDef
  , txOutCols
  , txOutTableDef
  )
import DbSync.Db.Sql.Refs (col, qcol, table)
import DbSync.Db.Statement.Common (arrayParam)

-- | Every spend redeemer in the database whose hash is still NULL.
-- Requires @tx_in.tx_out_id@ (the CTAS resolve) and
-- @tx_out.address_id@ (the per-epoch address worker) to be populated.
backfillSpendScriptHashStmt :: Stmt.Statement () Int64
backfillSpendScriptHashStmt =
  Stmt.preparable backfillSpendScriptHashSql E.noParams D.rowsAffected

backfillSpendScriptHashSql :: Text
backfillSpendScriptHashSql = spendScriptHashSql ""

-- | The block-scoped form: the redeemer ids the pipeline assigned for
-- one block. Driving off the @redeemer@ primary key keeps the plan an
-- index lookup per id rather than a scan of the whole table.
fillSpendScriptHashesStmt :: Stmt.Statement [RedeemerId] ()
fillSpendScriptHashesStmt =
  Stmt.preparable fillSpendScriptHashesSql encoder D.noResult
  where
    encoder = map getRedeemerId >$< arrayParam E.int8

fillSpendScriptHashesSql :: Text
fillSpendScriptHashesSql =
  spendScriptHashSql $
    T.unwords ["AND", qcol (table redeemerTableDef) redeemerCols.rdcId, "= ANY($1)"]

-- | @address.payment_cred@ of the spent output, guarded on
-- @has_script@: a key-locked output never carries a redeemer, so a
-- match there would mean a malformed input row.
spendScriptHashSql :: Text -> Text
spendScriptHashSql scopeClause = T.unwords
  [ "UPDATE", table redeemerTableDef
  , "SET", col redeemerCols.rdcScriptHash, "=", qcol "a" addressCols.acPaymentCred
  , "FROM", table txInTableDef, "ti"
  , "JOIN", table txOutTableDef, "o"
  , "  ON", qcol "o" txOutCols.tocTxId, "=", qcol "ti" txInCols.ticTxOutId
  , " AND", qcol "o" txOutCols.tocIndex, "=", qcol "ti" txInCols.ticTxOutIndex
  , "JOIN", table addressTableDef, "a"
  , "  ON", qcol "a" addressCols.acId, "=", qcol "o" txOutCols.tocAddressId
  , "WHERE", qcol "ti" txInCols.ticRedeemerId
  ,     "=", qcol (table redeemerTableDef) redeemerCols.rdcId
  , "  AND", qcol (table redeemerTableDef) redeemerCols.rdcPurpose, "= 'spend'"
  , "  AND", qcol (table redeemerTableDef) redeemerCols.rdcScriptHash, "IS NULL"
  , "  AND", qcol "a" addressCols.acHasScript
  , scopeClause
  ]
