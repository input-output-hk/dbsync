-- | Follow 'IdResolver' fragments for the @multi_asset@ extractor.
--
-- @multi_asset@ deduplicates by (policy, name). The dedup key handed
-- in by the extractor (a 'ShortByteString' formed from policy ++ name)
-- is ignored here; the structured fields drive the SELECT.
module DbSync.Phase.Following.Resolver.MultiAsset
  ( -- * Direct flavour
    resolveMultiAssetConn
  , assignMaTxMintIdConn
  , assignMaTxOutIdConn

    -- * Buffered flavour
  , resolveMultiAssetBuf
  , assignMaTxMintIdBuf
  , assignMaTxOutIdBuf
  ) where

import Cardano.Prelude

import Data.ByteString.Short (ShortByteString)
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Ids (MaTxMintId, MaTxOutId, MultiAssetId)
import DbSync.Db.Schema.MultiAsset (MultiAsset, multiAssetName, multiAssetPolicy)
import DbSync.Db.Statement.MaTxMint (nextMaTxMintIdStmt)
import DbSync.Db.Statement.MaTxOut (nextMaTxOutIdStmt)
import DbSync.Db.Statement.MultiAsset (nextMultiAssetIdStmt, queryMultiAssetIdStmt)
import DbSync.Phase.Following.IdAllocator (PreAllocatedIds (..), popHead)
import DbSync.Phase.Following.Resolver.Internal
  ( BlockDedupCache (..)
  , cacheInsert
  , runStmt
  )

-- ---------------------------------------------------------------------------
-- * Direct flavour
-- ---------------------------------------------------------------------------

resolveMultiAssetConn
  :: Conn.Connection -> ShortByteString -> MultiAsset -> IO (MultiAssetId, Bool)
resolveMultiAssetConn conn _key ma = do
  mId <- runStmt conn (multiAssetPolicy ma, multiAssetName ma) queryMultiAssetIdStmt
  case mId of
    Just maId -> pure (maId, False)
    Nothing   -> do
      maId <- runStmt conn () nextMultiAssetIdStmt
      pure (maId, True)

assignMaTxMintIdConn :: Conn.Connection -> IO MaTxMintId
assignMaTxMintIdConn conn = runStmt conn () nextMaTxMintIdStmt

assignMaTxOutIdConn :: Conn.Connection -> IO MaTxOutId
assignMaTxOutIdConn conn = runStmt conn () nextMaTxOutIdStmt

-- ---------------------------------------------------------------------------
-- * Buffered flavour
-- ---------------------------------------------------------------------------

resolveMultiAssetBuf
  :: Conn.Connection
  -> BlockDedupCache
  -> ShortByteString
  -> MultiAsset
  -> IO (MultiAssetId, Bool)
resolveMultiAssetBuf conn cache _key ma = do
  let policy = multiAssetPolicy ma
      name   = multiAssetName ma
      key    = (policy, name)
      mapRef = bdcMultiAsset cache
  m <- readIORef mapRef
  case Map.lookup key m of
    Just maId -> pure (maId, False)
    Nothing -> do
      mId <- runStmt conn key queryMultiAssetIdStmt
      case mId of
        Just maId -> do
          cacheInsert mapRef key maId
          pure (maId, False)
        Nothing -> do
          maId <- runStmt conn () nextMultiAssetIdStmt
          cacheInsert mapRef key maId
          pure (maId, True)

assignMaTxMintIdBuf :: PreAllocatedIds -> IO MaTxMintId
assignMaTxMintIdBuf preAlloc = popHead "assignMaTxMintId" (paiMaTxMintIds preAlloc)

assignMaTxOutIdBuf :: PreAllocatedIds -> IO MaTxOutId
assignMaTxOutIdBuf preAlloc = popHead "assignMaTxOutId" (paiMaTxOutIds preAlloc)
