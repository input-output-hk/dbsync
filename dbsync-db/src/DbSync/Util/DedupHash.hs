-- | Fixed-size key derivation for in-memory dedup maps.
--
-- A dedup map ('DbSync.Id.DedupMap.DedupMap') stores one entry per
-- unique entity ever seen, and never evicts. For maps whose natural
-- key is already a 28-byte cryptographic hash (pool key, stake
-- credential, script hash) the raw key is fine. For maps whose
-- natural key is wider or variable-length, we hash it down to a
-- uniform 28-byte digest via 'hashDedupKey'.
--
-- Any callsite that derives a key MUST go through this module so the
-- ingest path and the boot-time rebuild path produce byte-identical
-- keys for the same input.
module DbSync.Util.DedupHash
  ( hashDedupKey
  ) where

import Cardano.Prelude

import qualified Cardano.Crypto.Hash.Blake2b as Blake2b
import qualified Data.ByteString.Short as SBS

-- | 28-byte Blake2b-224 digest of @bs@, packed as an unpinned
-- 'SBS.ShortByteString'.
--
-- The bang forces the digest before packing so the input 'ByteString'
-- cannot be retained through a thunk by the caller.
hashDedupKey :: ByteString -> SBS.ShortByteString
hashDedupKey bs =
  let !digest = Blake2b.blake2b_libsodium 28 bs
  in SBS.toShort digest
