{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @collateral_tx_out@ table.
--
-- Holds the optional collateral-return output of a Babbage+ phase-2
-- failed transaction.
module DbSync.Db.Statement.CollateralTxOut
  ( -- * Inserts
    insertCollateralTxOutRowStmt

    -- * ID allocation
  , nextCollateralTxOutIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (CollateralTxOutId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO
  ( CollateralTxOut
  , collateralTxOutEncoder
  , collateralTxOutTableDef
  )

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

nextCollateralTxOutIdStmt :: Stmt.Statement () CollateralTxOutId
nextCollateralTxOutIdStmt =
  Stmt.preparable sql E.noParams (D.singleRow $ idDecoder CollateralTxOutId)
  where
    sql = "SELECT nextval('" <> table <> "_id_seq')"
