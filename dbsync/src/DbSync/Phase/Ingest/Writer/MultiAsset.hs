-- | COPY writers for tables owned by the @multi_asset@ extractor.
module DbSync.Phase.Ingest.Writer.MultiAsset
  ( writeMultiAssetCopy
  , writeMaTxMintCopy
  , writeMaTxOutCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.Ids (MultiAssetId)
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

writeMaTxMintCopy :: LoaderStream -> MaTxMint -> IO ()
writeMaTxMintCopy ls m = lsWriteRow ls (tdName maTxMintTableDef) (encodeMaTxMintCopy m)

writeMaTxOutCopy :: LoaderStream -> MaTxOut -> IO ()
writeMaTxOutCopy ls m = lsWriteRow ls (tdName maTxOutTableDef) (encodeMaTxOutCopy m)
