{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @ma_tx_mint@ table (per-tx
-- minting / burning events).
module DbSync.Db.Statement.MaTxMint
  ( -- * Inserts
    insertMaTxMintRowStmt

    -- * ID allocation
  , nextMaTxMintIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (MaTxMintId (..), idEncoder)
import DbSync.Db.Schema.MultiAsset (MaTxMint, maTxMintEncoder, maTxMintTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.Common (nextIdStmt)

table :: Text
table = tdName maTxMintTableDef

-- | Insert an 'MaTxMint' with a caller-chosen id.
insertMaTxMintRowStmt :: Stmt.Statement (MaTxMintId, MaTxMint) ()
insertMaTxMintRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getMaTxMintId)
           <> (snd >$< maTxMintEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, quantity, tx_id, ident) VALUES ($1, $2, $3, $4)"
      ]

-- | Allocate a new id from the @ma_tx_mint_id_seq@ sequence.
nextMaTxMintIdStmt :: Stmt.Statement () MaTxMintId
nextMaTxMintIdStmt = nextIdStmt maTxMintTableDef MaTxMintId
