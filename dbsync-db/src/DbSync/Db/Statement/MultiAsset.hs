{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @multi_asset@ table.
--
-- @multi_asset@ is dedup-keyed by @(policy, name)@. The
-- 'FollowingChainTip' resolver runs 'queryMultiAssetIdStmt' first;
-- on a miss it allocates a fresh id from 'nextMultiAssetIdStmt'.
-- The companion @ma_tx_mint@ and @ma_tx_out@ tables have their own
-- 'DbSync.Db.Statement.MaTxMint' / 'DbSync.Db.Statement.MaTxOut'
-- modules.
module DbSync.Db.Statement.MultiAsset
  ( -- * Inserts
    insertMultiAssetRowStmt

    -- * ID allocation
  , nextMultiAssetIdStmt

    -- * Lookups
  , queryMultiAssetIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (MultiAssetId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.MultiAsset (MultiAsset, multiAssetEncoder, multiAssetTableDef)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName multiAssetTableDef

-- | Insert a 'MultiAsset' with a caller-chosen id.
insertMultiAssetRowStmt :: Stmt.Statement (MultiAssetId, MultiAsset) ()
insertMultiAssetRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getMultiAssetId)
           <> (snd >$< multiAssetEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, policy, name, fingerprint) VALUES ($1, $2, $3, $4)"
      ]

-- | Allocate a new id from the @multi_asset_id_seq@ sequence.
nextMultiAssetIdStmt :: Stmt.Statement () MultiAssetId
nextMultiAssetIdStmt =
  Stmt.preparable sql E.noParams (D.singleRow $ idDecoder MultiAssetId)
  where
    sql = "SELECT nextval('" <> table <> "_id_seq')"

-- | Look up a 'MultiAssetId' by @(policy, name)@. The dedup key on
-- the resolver side is the concatenation of policy + name as
-- 'ShortByteString', but we query the columns directly here.
queryMultiAssetIdStmt :: Stmt.Statement (ByteString, ByteString) (Maybe MultiAssetId)
queryMultiAssetIdStmt =
  Stmt.preparable sql encoder (D.rowMaybe (idDecoder MultiAssetId))
  where
    encoder = (fst >$< E.param (E.nonNullable E.bytea))
           <> (snd >$< E.param (E.nonNullable E.bytea))
    sql = "SELECT id FROM " <> table <> " WHERE policy = $1 AND name = $2"
