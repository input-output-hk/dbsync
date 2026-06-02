-- | Ingest 'IdResolver' fragments for the @multi_asset@ extractor.
module DbSync.Phase.Ingest.Resolver.MultiAsset
  ( resolveMultiAssetIngest
  , assignMaTxMintIdIngest
  , assignMaTxOutIdIngest
  ) where

import Cardano.Prelude

import Data.ByteString.Short (ShortByteString)
import Data.IORef (IORef)

import DbSync.Db.Schema.Ids (MaTxMintId (..), MaTxOutId (..), MultiAssetId (..))
import DbSync.Db.Schema.MultiAsset (MultiAsset)
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounters (..))
import DbSync.Phase.Ingest.DedupStore (DedupStores (..), lookupOrInsert)
import DbSync.Phase.Ingest.Resolver.Internal (allocateNextId)

-- | Key arrives as 'ShortByteString' (already unpinned) from the
-- extractor; matches the dedup store's key type.
resolveMultiAssetIngest
  :: DedupStores -> ShortByteString -> MultiAsset -> IO (MultiAssetId, Bool)
resolveMultiAssetIngest dedupStores skey _ma = do
  (maId, isNew) <- lookupOrInsert skey (dstMultiAsset dedupStores)
  pure (MultiAssetId maId, isNew)

assignMaTxMintIdIngest :: IORef ExtractState -> IO MaTxMintId
assignMaTxMintIdIngest extractStateRef =
  allocateNextId extractStateRef icMaTxMintId (\cs c -> cs { icMaTxMintId = c }) MaTxMintId

assignMaTxOutIdIngest :: IORef ExtractState -> IO MaTxOutId
assignMaTxOutIdIngest extractStateRef =
  allocateNextId extractStateRef icMaTxOutId (\cs c -> cs { icMaTxOutId = c }) MaTxOutId
