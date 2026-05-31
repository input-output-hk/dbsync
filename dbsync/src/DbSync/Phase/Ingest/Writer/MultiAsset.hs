-- | COPY writers for tables owned by the @multi_asset@ extractor.
module DbSync.Phase.Ingest.Writer.MultiAsset
  ( writeMultiAssetCopy
  , writeMaTxMintCopy
  , writeMaTxOutCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.Ids (MaTxMintId, MaTxOutId, MultiAssetId)
import DbSync.Db.Schema.MultiAsset
  ( MaTxMint
  , MaTxOut
  , MultiAsset
  , encodeMaTxMintCopy
  , encodeMaTxOutCopy
  , encodeMultiAssetCopy
  , maTxMintTableDef
  , maTxOutTableDef
  , multiAssetTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))

writeMultiAssetCopy :: LoaderStream -> MultiAssetId -> MultiAsset -> IO ()
writeMultiAssetCopy ls mid ma = lsWriteRow ls (tdName multiAssetTableDef) (encodeMultiAssetCopy mid ma)

writeMaTxMintCopy :: LoaderStream -> MaTxMintId -> MaTxMint -> IO ()
writeMaTxMintCopy ls mid m = lsWriteRow ls (tdName maTxMintTableDef) (encodeMaTxMintCopy mid m)

writeMaTxOutCopy :: LoaderStream -> MaTxOutId -> MaTxOut -> IO ()
writeMaTxOutCopy ls mid m = lsWriteRow ls (tdName maTxOutTableDef) (encodeMaTxOutCopy mid m)
