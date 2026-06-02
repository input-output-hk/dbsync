-- | Ingest 'IdResolver' fragment for the @cbor@ extractor.
module DbSync.Phase.Ingest.Resolver.Cbor
  ( assignTxCborIdIngest
  ) where

import Cardano.Prelude

import Data.IORef (IORef)

import DbSync.Db.Schema.Ids (TxCborId (..))
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounters (..))
import DbSync.Phase.Ingest.Resolver.Internal (allocateNextId)

assignTxCborIdIngest :: IORef ExtractState -> IO TxCborId
assignTxCborIdIngest extractStateRef =
  allocateNextId extractStateRef icTxCborId (\cs c -> cs { icTxCborId = c }) TxCborId
