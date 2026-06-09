{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @multi_asset@ extractor tables:
-- @multi_asset@, @ma_tx_mint@, @ma_tx_out@.
--
-- @multi_asset@ is dedup-keyed by @(policy, name)@: the
-- 'FollowingChainTip' resolver runs 'queryMultiAssetIdStmt' first;
-- on a miss it allocates a fresh id from 'nextMultiAssetIdStmt'.
module DbSync.Db.Statement.MultiAsset
  ( -- * multi_asset
    insertMultiAssetRowStmt
  , nextMultiAssetIdStmt
  , queryMultiAssetIdStmt

    -- * ma_tx_mint
  , insertMaTxMintRowStmt

    -- * ma_tx_out
  , insertMaTxOutRowStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (MultiAssetId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.MultiAsset
  ( MaTxMint
  , MaTxOut
  , MultiAsset
  , MultiAssetCols (..)
  , maTxMintEncoder
  , maTxMintTableDef
  , maTxOutEncoder
  , maTxOutTableDef
  , multiAssetCols
  , multiAssetEncoder
  , multiAssetTableDef
  )
import DbSync.Db.Schema.Types (TableColumn (..), TableDef (..))
import DbSync.Db.Statement.Common (insertRowSql, nextIdStmt)

-- ---------------------------------------------------------------------------
-- * multi_asset
-- ---------------------------------------------------------------------------

insertMultiAssetRowStmt :: Stmt.Statement (MultiAssetId, MultiAsset) ()
insertMultiAssetRowStmt =
  Stmt.preparable (insertRowSql multiAssetTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getMultiAssetId)
           <> (snd >$< multiAssetEncoder)

nextMultiAssetIdStmt :: Stmt.Statement () MultiAssetId
nextMultiAssetIdStmt = nextIdStmt multiAssetTableDef MultiAssetId

-- | Look up a 'MultiAssetId' by @(policy, name)@. The resolver's
-- in-memory dedup key is the concatenation of policy + name as a
-- 'ShortByteString'; this statement queries the columns directly.
queryMultiAssetIdStmt :: Stmt.Statement (ByteString, ByteString) (Maybe MultiAssetId)
queryMultiAssetIdStmt =
  Stmt.preparable sql encoder (D.rowMaybe (idDecoder MultiAssetId))
  where
    encoder = (fst >$< E.param (E.nonNullable E.bytea))
           <> (snd >$< E.param (E.nonNullable E.bytea))
    sql = mconcat
      [ "SELECT ", multiAssetCols.macId.tcName
      , " FROM ", tdName multiAssetTableDef
      , " WHERE ", multiAssetCols.macPolicy.tcName, " = $1"
      , " AND ", multiAssetCols.macName.tcName, " = $2"
      ]

-- ---------------------------------------------------------------------------
-- * ma_tx_mint
-- ---------------------------------------------------------------------------

insertMaTxMintRowStmt :: Stmt.Statement MaTxMint ()
insertMaTxMintRowStmt =
  Stmt.preparable (insertRowSql maTxMintTableDef) maTxMintEncoder D.noResult

-- ---------------------------------------------------------------------------
-- * ma_tx_out
-- ---------------------------------------------------------------------------

insertMaTxOutRowStmt :: Stmt.Statement MaTxOut ()
insertMaTxOutRowStmt =
  Stmt.preparable (insertRowSql maTxOutTableDef) maTxOutEncoder D.noResult
