{-# LANGUAGE OverloadedStrings #-}

-- | Fill @redeemer.script_hash@ for spend redeemers.
--
-- Every other purpose resolves its hash from the tx body while parsing. A
-- spend redeemer points at a 'TxIn', so its hash is the payment credential
-- of the /spent/ output, which is reachable only through
-- @tx_in.tx_out_id@. Ingest leaves that column unresolved.
module DbSync.Db.Statement.Worker.RedeemerScriptHash
  ( -- * Prep rebuild
    rebuildSpendScriptHashScript

    -- * Follow statement
  , fillSpendScriptHashesStmt
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
import DbSync.Db.Schema.Types (TableColumn (..))
import DbSync.Db.Schema.UTxO
  ( TxInCols (..)
  , TxOutCols (..)
  , txInCols
  , txInTableDef
  , txOutCols
  , txOutTableDef
  )
import DbSync.Db.Sql.Refs (col, qcol, table)
import DbSync.Db.Statement.Common (arrayParam, rebuildTableScript)

-- ---------------------------------------------------------------------------
-- * Prep rebuild
-- ---------------------------------------------------------------------------

-- | Rebuild @redeemer@ with every spend hash the ingested range resolves.
-- Requires the CTAS resolve to populate @tx_in.tx_out_id@ and the
-- per-epoch address worker to populate @tx_out.address_id@.
--
-- Prep rebuilds instead of updating, because nearly every spend redeemer
-- needs a hash. An UPDATE at that fraction leaves one dead tuple per row,
-- writes in join order rather than heap order, and cannot go parallel.
rebuildSpendScriptHashScript :: Text
rebuildSpendScriptHashScript =
  rebuildTableScript
    redeemerTableDef
    [(redeemerCols.rdcScriptHash.tcName, resolvedHash)]
    fromSql
  where
    -- A hash already on the row wins; the purpose guard keeps a
    -- non-spend redeemer untouched even if an input pointed at it.
    resolvedHash = T.unwords
      [ "COALESCE(", qcol "src" redeemerCols.rdcScriptHash, ","
      , "CASE WHEN", qcol "src" redeemerCols.rdcPurpose, "= 'spend'"
      , "THEN", qcol "spend" redeemerCols.rdcScriptHash, "END )"
      ]

    fromSql = T.unwords
      [ table redeemerTableDef, "src"
      , "LEFT JOIN (", spendHashSubquery, ") spend"
      , "ON", qcol "spend" txInCols.ticRedeemerId, "=", qcol "src" redeemerCols.rdcId
      ]

-- | @(redeemer_id, script_hash)@ for every input that unlocks a
-- script-locked output. It yields at most one row per @redeemer_id@:
-- @tx_out.(tx_id, index)@ is unique and @address.id@ is a primary key, so
-- neither join fans out.
spendHashSubquery :: Text
spendHashSubquery = T.unwords
  [ "SELECT", qcol "ti" txInCols.ticRedeemerId
  ,     "AS", col txInCols.ticRedeemerId, ","
  ,          qcol "a" addressCols.acPaymentCred
  ,     "AS", col redeemerCols.rdcScriptHash
  , "FROM", table txInTableDef, "ti"
  , "JOIN", table txOutTableDef, "o"
  , "  ON", qcol "o" txOutCols.tocTxId, "=", qcol "ti" txInCols.ticTxOutId
  , " AND", qcol "o" txOutCols.tocIndex, "=", qcol "ti" txInCols.ticTxOutIndex
  , "JOIN", table addressTableDef, "a"
  , "  ON", qcol "a" addressCols.acId, "=", qcol "o" txOutCols.tocAddressId
  , "WHERE", qcol "ti" txInCols.ticRedeemerId, "IS NOT NULL"
  , "  AND", qcol "a" addressCols.acHasScript
  ]

-- ---------------------------------------------------------------------------
-- * Follow statement
-- ---------------------------------------------------------------------------

-- | The redeemer ids the pipeline assigned for one block. Driving off
-- the @redeemer@ primary key keeps the plan an index lookup per id
-- rather than a scan of the whole table.
fillSpendScriptHashesStmt :: Stmt.Statement [RedeemerId] ()
fillSpendScriptHashesStmt =
  Stmt.preparable fillSpendScriptHashesSql encoder D.noResult
  where
    encoder = map getRedeemerId >$< arrayParam E.int8

-- | Exported so tests can feed it to @EXPLAIN@ and assert on the plan
-- shape.
fillSpendScriptHashesSql :: Text
fillSpendScriptHashesSql = T.unwords
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
  , "  AND", qcol (table redeemerTableDef) redeemerCols.rdcId, "= ANY($1)"
  ]
