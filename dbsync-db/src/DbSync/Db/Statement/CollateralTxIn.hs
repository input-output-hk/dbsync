{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @collateral_tx_in@ table.
--
-- Same shape as 'DbSync.Db.Statement.TxIn' but no @redeemer_id@ column.
-- Collateral inputs are written when a Plutus script in the spending
-- transaction fails phase-2 validation (Alonzo+).
module DbSync.Db.Statement.CollateralTxIn
  ( -- * Inserts
    insertCollateralTxInRowStmt

    -- * ID allocation
  , nextCollateralTxInIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (CollateralTxInId (..), idEncoder)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO
  ( CollateralTxIn
  , collateralTxInEncoder
  , collateralTxInTableDef
  )
import DbSync.Db.Statement.Common (nextIdStmt)

table :: Text
table = tdName collateralTxInTableDef

-- | Insert a 'CollateralTxIn' with a caller-chosen id.
insertCollateralTxInRowStmt :: Stmt.Statement (CollateralTxInId, CollateralTxIn) ()
insertCollateralTxInRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getCollateralTxInId)
           <> (snd >$< collateralTxInEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " ( id, tx_in_id, tx_out_id, tx_out_index, tx_out_hash)"
      , " VALUES ($1, $2, $3, $4, $5)"
      ]

-- | Allocate a new id from the @collateral_tx_in_id_seq@ sequence.
nextCollateralTxInIdStmt :: Stmt.Statement () CollateralTxInId
nextCollateralTxInIdStmt = nextIdStmt collateralTxInTableDef CollateralTxInId
