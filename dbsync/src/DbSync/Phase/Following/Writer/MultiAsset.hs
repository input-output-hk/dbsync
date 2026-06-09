-- | hasql writers for tables owned by the @multi_asset@ extractor.
module DbSync.Phase.Following.Writer.MultiAsset
  ( writeMultiAssetConn
  , writeMultiAssetBuf
  , writeMaTxMintConn
  , writeMaTxMintBuf
  , writeMaTxOutConn
  , writeMaTxOutBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Ids (MultiAssetId)
import DbSync.Db.Schema.MultiAsset (MaTxMint, MaTxOut, MultiAsset)
import DbSync.Db.Statement.MultiAsset (insertMaTxMintRowStmt)
import DbSync.Db.Statement.MultiAsset (insertMaTxOutRowStmt)
import DbSync.Db.Statement.MultiAsset (insertMultiAssetRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

writeMultiAssetConn :: Conn.Connection -> MultiAssetId -> MultiAsset -> IO ()
writeMultiAssetConn conn mid ma = runConn conn (mid, ma) insertMultiAssetRowStmt

writeMultiAssetBuf :: WriteBuffer -> MultiAssetId -> MultiAsset -> IO ()
writeMultiAssetBuf buf mid ma = queueBuf buf (mid, ma) insertMultiAssetRowStmt

writeMaTxMintConn :: Conn.Connection -> MaTxMint -> IO ()
writeMaTxMintConn conn m = runConn conn m insertMaTxMintRowStmt

writeMaTxMintBuf :: WriteBuffer -> MaTxMint -> IO ()
writeMaTxMintBuf buf m = queueBuf buf m insertMaTxMintRowStmt

writeMaTxOutConn :: Conn.Connection -> MaTxOut -> IO ()
writeMaTxOutConn conn m = runConn conn m insertMaTxOutRowStmt

writeMaTxOutBuf :: WriteBuffer -> MaTxOut -> IO ()
writeMaTxOutBuf buf m = queueBuf buf m insertMaTxOutRowStmt
