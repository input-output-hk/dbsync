{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @stake_address@ dedup table.
--
-- 'FollowingChainTip' resolves a stake address by @hash_raw@ via
-- 'queryStakeAddressIdStmt'. On a miss it allocates a new id from
-- 'nextStakeAddressIdStmt' and the writer inserts via
-- 'insertStakeAddressRowStmt'.
module DbSync.Db.Statement.StakeAddress
  ( -- * Inserts
    insertStakeAddressRowStmt

    -- * ID allocation
  , nextStakeAddressIdStmt

    -- * Lookups
  , queryStakeAddressIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (StakeAddressId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.StakeDelegation
  ( StakeAddress
  , stakeAddressEncoder
  , stakeAddressTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName stakeAddressTableDef

insertStakeAddressRowStmt :: Stmt.Statement (StakeAddressId, StakeAddress) ()
insertStakeAddressRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getStakeAddressId)
           <> (snd >$< stakeAddressEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, hash_raw, view, script_hash) VALUES ($1, $2, $3, $4)"
      ]

nextStakeAddressIdStmt :: Stmt.Statement () StakeAddressId
nextStakeAddressIdStmt =
  Stmt.preparable sql E.noParams (D.singleRow $ idDecoder StakeAddressId)
  where
    sql = "SELECT nextval('" <> table <> "_id_seq')"

-- | Look up an existing 'StakeAddressId' by 28-byte credential hash.
queryStakeAddressIdStmt :: Stmt.Statement ByteString (Maybe StakeAddressId)
queryStakeAddressIdStmt =
  Stmt.preparable sql
    (E.param (E.nonNullable E.bytea))
    (D.rowMaybe (idDecoder StakeAddressId))
  where
    sql = "SELECT id FROM " <> table <> " WHERE hash_raw = $1"
