-- | Checkpoint serialisation.
--
-- Binary serialisation and deserialisation of 'DedupMaps' for checkpoint
-- persistence. Uses a compact binary format for fast save/restore.
module DbSync.Checkpoint.Serialise
  ( serialiseDedupMaps
  , deserialiseDedupMaps
  ) where

import Cardano.Prelude

import DbSync.Phase.Ingest.DedupMap (DedupMaps)

-- | Serialise dedup maps to a compact binary format.
serialiseDedupMaps :: DedupMaps -> ByteString
serialiseDedupMaps = panic "TODO: not implemented"

-- | Deserialise dedup maps from a checkpoint file.
deserialiseDedupMaps :: ByteString -> Either Text DedupMaps
deserialiseDedupMaps = panic "TODO: not implemented"
