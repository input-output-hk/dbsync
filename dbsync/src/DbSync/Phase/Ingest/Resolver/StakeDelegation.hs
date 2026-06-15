-- | Ingest 'IdResolver' fragments for the @stake_delegation@ extractor.
module DbSync.Phase.Ingest.Resolver.StakeDelegation
  ( resolveStakeAddressIngest
  ) where

import Cardano.Prelude

import qualified Data.ByteString.Short as SBS

import DbSync.Db.Schema.Ids (StakeAddressId (..))
import DbSync.Db.Schema.Core (StakeAddress)
import DbSync.Phase.Ingest.DedupStore (DedupStores (..), lookupOrInsert)

-- | Dedup lookup against the LSM-backed stake_address table.
resolveStakeAddressIngest
  :: DedupStores -> ByteString -> StakeAddress -> IO (StakeAddressId, Bool)
resolveStakeAddressIngest dedupStores hash _sa = do
  let !key = SBS.toShort hash
  (saId, isNew) <- lookupOrInsert key (dstStakeAddress dedupStores)
  pure (StakeAddressId saId, isNew)
