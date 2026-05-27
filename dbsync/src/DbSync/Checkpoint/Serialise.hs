-- | Checkpoint serialisation.
--
-- Binary serialisation and deserialisation of 'DedupStores' for
-- checkpoint persistence. Uses a compact binary format for fast
-- save/restore.
module DbSync.Checkpoint.Serialise
  ( serialiseDedupMaps
  , deserialiseDedupMaps
  ) where

import Cardano.Prelude

import DbSync.Phase.Ingest.DedupStore (DedupStores)

-- | Serialise dedup stores to a compact binary format.
serialiseDedupMaps :: DedupStores -> ByteString
serialiseDedupMaps = panic "TODO: not implemented"

-- | Deserialise dedup stores from a checkpoint file.
deserialiseDedupMaps :: ByteString -> Either Text DedupStores
deserialiseDedupMaps = panic "TODO: not implemented"
