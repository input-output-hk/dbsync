-- | Ingest 'IdResolver' fragment for the @metadata@ extractor.
module DbSync.Phase.Ingest.Resolver.Metadata
  ( assignTxMetadataIdIngest
  ) where

import Cardano.Prelude

import Data.IORef (IORef)

import DbSync.Db.Schema.Ids (TxMetadataId (..))
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounters (..))
import DbSync.Phase.Ingest.Resolver.Internal (allocateNextId)

assignTxMetadataIdIngest :: IORef ExtractState -> IO TxMetadataId
assignTxMetadataIdIngest extractStateRef =
  allocateNextId extractStateRef icTxMetadataId (\cs c -> cs { icTxMetadataId = c }) TxMetadataId
