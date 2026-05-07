{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @address@ dedup table.
--
-- 'FollowingChainTip' resolves an address by @raw@ via
-- 'queryAddressIdStmt'. On a miss it allocates a new id from
-- 'nextAddressIdStmt' and the writer inserts via
-- 'insertAddressRowStmt'.
module DbSync.Db.Statement.Address
  ( -- * Inserts
    insertAddressRowStmt

    -- * ID allocation
  , nextAddressIdStmt

    -- * Lookups
  , queryAddressIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Address
  ( Address
  , addressEncoder
  , addressTableDef
  )
import DbSync.Db.Schema.Ids (AddressId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName addressTableDef

insertAddressRowStmt :: Stmt.Statement (AddressId, Address) ()
insertAddressRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getAddressId)
           <> (snd >$< addressEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, address, raw, has_script, payment_cred, stake_address_id)"
      , " VALUES ($1, $2, $3, $4, $5, $6)"
      ]

nextAddressIdStmt :: Stmt.Statement () AddressId
nextAddressIdStmt =
  Stmt.preparable sql E.noParams (D.singleRow $ idDecoder AddressId)
  where
    sql = "SELECT nextval('" <> table <> "_id_seq')"

-- | Look up an existing 'AddressId' by raw address bytes.
queryAddressIdStmt :: Stmt.Statement ByteString (Maybe AddressId)
queryAddressIdStmt =
  Stmt.preparable sql
    (E.param (E.nonNullable E.bytea))
    (D.rowMaybe (idDecoder AddressId))
  where
    sql = "SELECT id FROM " <> table <> " WHERE raw = $1"
