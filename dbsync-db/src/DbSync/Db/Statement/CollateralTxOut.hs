{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @collateral_tx_out@ table.
--
-- Holds the optional collateral-return output of a Babbage+ phase-2
-- failed transaction.
module DbSync.Db.Statement.CollateralTxOut
  ( -- * Inserts
    insertCollateralTxOutRowStmt

    -- * Updates
  , updateCollateralTxOutAddressIdStmt
  , bulkUpdateCollateralTxOutAddressIdsStmt

    -- * ID allocation
  , nextCollateralTxOutIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (AddressId (..), CollateralTxOutId (..), idEncoder)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO
  ( CollateralTxOut
  , collateralTxOutEncoder
  , collateralTxOutTableDef
  )
import DbSync.Db.Statement.Common (arrayParam, nextIdStmt)

table :: Text
table = tdName collateralTxOutTableDef

insertCollateralTxOutRowStmt :: Stmt.Statement (CollateralTxOutId, CollateralTxOut) ()
insertCollateralTxOutRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getCollateralTxOutId)
           <> (snd >$< collateralTxOutEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " ( id, tx_id, index, address_id, stake_address_id, value"
      , " , data_hash, multi_assets_descr, inline_datum_id, reference_script_id)"
      , " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)"
      ]

-- | Fill in @collateral_tx_out.address_id@ for an existing row.
updateCollateralTxOutAddressIdStmt :: Stmt.Statement (AddressId, CollateralTxOutId) ()
updateCollateralTxOutAddressIdStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getAddressId)
           <> (snd >$< idEncoder getCollateralTxOutId)
    sql = "UPDATE " <> table <> " SET address_id = $1 WHERE id = $2"

-- | Bulk-update @collateral_tx_out.address_id@. Same shape as
-- 'DbSync.Db.Statement.TxOut.bulkUpdateTxOutAddressIdsStmt'; one
-- round-trip regardless of input size.
bulkUpdateCollateralTxOutAddressIdsStmt :: Stmt.Statement ([Int64], [Int64]) ()
bulkUpdateCollateralTxOutAddressIdsStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< arrayParam E.int8)    -- collateral_tx_out ids
           <> (snd >$< arrayParam E.int8)    -- address ids
    sql = T.concat
      [ "UPDATE ", table, " SET address_id = u.aid"
      , " FROM unnest($1, $2) AS u(out_id, aid)"
      , " WHERE ", table, ".id = u.out_id"
      ]

nextCollateralTxOutIdStmt :: Stmt.Statement () CollateralTxOutId
nextCollateralTxOutIdStmt = nextIdStmt collateralTxOutTableDef CollateralTxOutId
