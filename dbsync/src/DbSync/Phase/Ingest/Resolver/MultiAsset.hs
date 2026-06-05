-- | Ingest 'IdResolver' fragment for the @multi_asset@ extractor.
module DbSync.Phase.Ingest.Resolver.MultiAsset
  ( resolveMultiAssetIngest
  ) where

import Cardano.Prelude

import Data.ByteString.Short (ShortByteString)

import DbSync.Db.Schema.Ids (MultiAssetId (..))
import DbSync.Db.Schema.MultiAsset (MultiAsset)
import DbSync.Phase.Ingest.DedupStore (DedupStores (..), lookupOrInsert)

-- | Key arrives as 'ShortByteString' (already unpinned) from the
-- extractor; matches the dedup store's key type.
resolveMultiAssetIngest
  :: DedupStores -> ShortByteString -> MultiAsset -> IO (MultiAssetId, Bool)
resolveMultiAssetIngest dedupStores skey _ma = do
  (maId, isNew) <- lookupOrInsert skey (dstMultiAsset dedupStores)
  pure (MultiAssetId maId, isNew)
