{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @withdrawal@ table.
module DbSync.Db.Statement.Withdrawal
  ( -- * Inserts
    insertWithdrawalRowStmt

    -- * ID allocation
  , nextWithdrawalIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (WithdrawalId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.StakeDelegation
  ( Withdrawal
  , withdrawalEncoder
  , withdrawalTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName withdrawalTableDef

insertWithdrawalRowStmt :: Stmt.Statement (WithdrawalId, Withdrawal) ()
insertWithdrawalRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getWithdrawalId)
           <> (snd >$< withdrawalEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, addr_id, tx_id, amount, redeemer_id)"
      , " VALUES ($1, $2, $3, $4, $5)"
      ]

nextWithdrawalIdStmt :: Stmt.Statement () WithdrawalId
nextWithdrawalIdStmt =
  Stmt.preparable sql E.noParams (D.singleRow $ idDecoder WithdrawalId)
  where
    sql = "SELECT nextval('" <> table <> "_id_seq')"
