-- | Ingest 'IdResolver' fragments for the @scripts_datums@ extractor.
module DbSync.Phase.Ingest.Resolver.ScriptsDatums
  ( resolveDatumIngest
  , resolveScriptIngest
  , resolveRedeemerDataIngest
  , assignRedeemerIdIngest
  ) where

import Cardano.Prelude

import qualified Data.ByteString.Short as SBS
import Data.IORef (IORef)

import DbSync.Db.Schema.Ids
  ( DatumId (..)
  , RedeemerDataId (..)
  , RedeemerId (..)
  , ScriptId (..)
  )
import DbSync.Db.Schema.ScriptsDatums (Datum, RedeemerData, Script)
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounters (..))
import DbSync.Phase.Ingest.DedupStore (DedupStores (..), lookupOrInsert)
import DbSync.Phase.Ingest.Resolver.Internal (allocateNextId)

resolveDatumIngest
  :: DedupStores -> ByteString -> Datum -> IO (DatumId, Bool)
resolveDatumIngest stores hash _row = do
  (rawId, isNew) <- lookupOrInsert (SBS.toShort hash) (dstDatum stores)
  pure (DatumId rawId, isNew)

resolveScriptIngest
  :: DedupStores -> ByteString -> Script -> IO (ScriptId, Bool)
resolveScriptIngest stores hash _row = do
  (rawId, isNew) <- lookupOrInsert (SBS.toShort hash) (dstScriptHash stores)
  pure (ScriptId rawId, isNew)

resolveRedeemerDataIngest
  :: DedupStores -> ByteString -> RedeemerData -> IO (RedeemerDataId, Bool)
resolveRedeemerDataIngest stores hash _row = do
  (rawId, isNew) <- lookupOrInsert (SBS.toShort hash) (dstRedeemerData stores)
  pure (RedeemerDataId rawId, isNew)

assignRedeemerIdIngest :: IORef ExtractState -> IO RedeemerId
assignRedeemerIdIngest extractStateRef =
  allocateNextId extractStateRef icRedeemerId
    (\cs c -> cs { icRedeemerId = c }) RedeemerId
