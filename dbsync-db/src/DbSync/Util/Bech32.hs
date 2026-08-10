{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Bech32 + CIP-14 fingerprint encoders.
--
-- Lives in @dbsync-db@ so row constructors can call it without
-- depending on the @dbsync@ engine.
module DbSync.Util.Bech32
  ( -- * Generic encoders
    serialiseToBech32

    -- * Fixed-HRP encoders
  , serialiseVrfVkToBech32
  , serialisePoolKeyHashToBech32
  , serialiseDrepToBech32

    -- * Address encoders
  , serialiseShelleyAddrToBech32
  , serialiseStakeKeyHashToBech32
  , serialiseStakeScriptHashToBech32

    -- * CIP-14 asset fingerprint
  , mkAssetFingerprint
  ) where

import Cardano.Prelude

import qualified Cardano.Crypto.Hash.Blake2b as Blake2b
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS

-- ---------------------------------------------------------------------------
-- * Generic encoders
-- ---------------------------------------------------------------------------

-- | Bech32-encode @bytes@ with the given human-readable prefix.
--
-- An invalid HRP — Bech32 wants lower-case ASCII, length 1-83 — panics
-- instead of returning a bad encoding. Every HRP here is a static literal
-- that the unit tests check, so the panic only fires on a code mistake.
serialiseToBech32 :: Text -> ByteString -> Text
serialiseToBech32 prefix bytes =
  Bech32.encodeLenient hrp (Bech32.dataPartFromBytes bytes)
  where
    hrp = case Bech32.humanReadablePartFromText prefix of
      Right p  -> p
      Left err -> panic ("DbSync.Util.Bech32: invalid HRP " <> show prefix <> ": " <> show err)

-- ---------------------------------------------------------------------------
-- * Fixed-HRP encoders
-- ---------------------------------------------------------------------------

serialiseVrfVkToBech32 :: ByteString -> Text
serialiseVrfVkToBech32 = serialiseToBech32 "vrf_vk"

-- | Takes the 28-byte pool key hash and yields @pool1…@.
serialisePoolKeyHashToBech32 :: ByteString -> Text
serialisePoolKeyHashToBech32 = serialiseToBech32 "pool"

-- | Takes the 28-byte DRep credential hash. The result is the @view@
-- string on a @drep_hash@ row.
serialiseDrepToBech32 :: ByteString -> Text
serialiseDrepToBech32 = serialiseToBech32 "drep"

-- ---------------------------------------------------------------------------
-- * Address encoders
-- ---------------------------------------------------------------------------

-- | The low bit of the header byte selects the network: @1@ gives mainnet
-- (HRP @addr@), @0@ gives testnet (HRP @addr_test@). The caller must pass
-- a Shelley address. A Byron bootstrap address (header @0x80@) round-trips
-- through Base58, not Bech32.
serialiseShelleyAddrToBech32 :: ByteString -> Text
serialiseShelleyAddrToBech32 bs
  | BS.null bs = panic "serialiseShelleyAddrToBech32: empty bytes"
  | otherwise  = serialiseToBech32 (addrHrp (BS.head bs)) bs

-- | Builds the 29-byte reward address — header @0xE0 .|. net@ plus the
-- 28-byte credential — then encodes it with HRP @stake@ or @stake_test@.
serialiseStakeKeyHashToBech32 :: Bool -> ByteString -> Text
serialiseStakeKeyHashToBech32 mainnet credHash =
  serialiseToBech32 (rewardHrp mainnet) (BS.cons header credHash)
  where
    header = 0xE0 .|. networkBit mainnet

-- | Same as 'serialiseStakeKeyHashToBech32' but for script-hash
-- reward credentials (header @0xF0 .|. net@).
serialiseStakeScriptHashToBech32 :: Bool -> ByteString -> Text
serialiseStakeScriptHashToBech32 mainnet credHash =
  serialiseToBech32 (rewardHrp mainnet) (BS.cons header credHash)
  where
    header = 0xF0 .|. networkBit mainnet

-- ---------------------------------------------------------------------------
-- * CIP-14 asset fingerprint
-- ---------------------------------------------------------------------------

-- | CIP-14 asset fingerprint: Bech32 of @blake2b-160 (policy ++ name)@
-- with HRP @asset@.
mkAssetFingerprint :: ByteString -> ByteString -> Text
mkAssetFingerprint policy assetName =
  serialiseToBech32 "asset" (Blake2b.blake2b_libsodium 20 (policy <> assetName))

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | Reward and base addresses share this network-bit encoding.
addrHrp :: Word8 -> Text
addrHrp header
  | header .&. 0x01 == 0x01 = "addr"
  | otherwise               = "addr_test"

rewardHrp :: Bool -> Text
rewardHrp True  = "stake"
rewardHrp False = "stake_test"

networkBit :: Bool -> Word8
networkBit True  = 0x01
networkBit False = 0x00
