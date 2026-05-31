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

import DbSync.Db.Schema.Ids (MaTxMintId, MaTxOutId, MultiAssetId)
import DbSync.Db.Schema.MultiAsset (MaTxMint, MaTxOut, MultiAsset)
import DbSync.Db.Statement.MaTxMint (insertMaTxMintRowStmt)
import DbSync.Db.Statement.MaTxOut (insertMaTxOutRowStmt)
import DbSync.Db.Statement.MultiAsset (insertMultiAssetRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

writeMultiAssetConn :: Conn.Connection -> MultiAssetId -> MultiAsset -> IO ()
writeMultiAssetConn conn mid ma = runConn conn (mid, ma) insertMultiAssetRowStmt

writeMultiAssetBuf :: WriteBuffer -> MultiAssetId -> MultiAsset -> IO ()
writeMultiAssetBuf buf mid ma = queueBuf buf (mid, ma) insertMultiAssetRowStmt

writeMaTxMintConn :: Conn.Connection -> MaTxMintId -> MaTxMint -> IO ()
writeMaTxMintConn conn mid m = runConn conn (mid, m) insertMaTxMintRowStmt

writeMaTxMintBuf :: WriteBuffer -> MaTxMintId -> MaTxMint -> IO ()
writeMaTxMintBuf buf mid m = queueBuf buf (mid, m) insertMaTxMintRowStmt

writeMaTxOutConn :: Conn.Connection -> MaTxOutId -> MaTxOut -> IO ()
writeMaTxOutConn conn mid m = runConn conn (mid, m) insertMaTxOutRowStmt

writeMaTxOutBuf :: WriteBuffer -> MaTxOutId -> MaTxOut -> IO ()
writeMaTxOutBuf buf mid m = queueBuf buf (mid, m) insertMaTxOutRowStmt
