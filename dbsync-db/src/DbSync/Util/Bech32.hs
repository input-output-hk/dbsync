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
-- The HRP must be valid Bech32 (lower-case ASCII, length 1-83); if it
-- isn't, we 'panic' rather than return a bad encoding silently. All
-- HRPs used in this project are static literals checked by the unit
-- tests, so a panic here only fires on a programming mistake.
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

-- | VRF verification key — HRP @vrf_vk@.
serialiseVrfVkToBech32 :: ByteString -> Text
serialiseVrfVkToBech32 = serialiseToBech32 "vrf_vk"

-- | Stake-pool key hash (28 bytes) — HRP @pool@. Yields @pool1…@.
serialisePoolKeyHashToBech32 :: ByteString -> Text
serialisePoolKeyHashToBech32 = serialiseToBech32 "pool"

-- ---------------------------------------------------------------------------
-- * Address encoders
-- ---------------------------------------------------------------------------

-- | Encode a Shelley payment address from its raw bytes.
--
-- The header byte's low bit selects the network: @1@ → mainnet
-- (HRP @addr@), @0@ → testnet (HRP @addr_test@). Caller must ensure
-- @bs@ is a Shelley address — Byron bootstrap addresses (header
-- @0x80@) round-trip via Base58, not Bech32.
serialiseShelleyAddrToBech32 :: ByteString -> Text
serialiseShelleyAddrToBech32 bs
  | BS.null bs = panic "serialiseShelleyAddrToBech32: empty bytes"
  | otherwise  = serialiseToBech32 (addrHrp (BS.head bs)) bs

-- | Encode a stake-key reward address from a 28-byte credential and
-- the network. Builds the full 29-byte serialised reward address
-- (header @0xE0 .|. net@ + credential) and Bech32-encodes it with
-- HRP @stake@ / @stake_test@.
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

-- | HRP for a Shelley address: derived from the network bit (low bit
-- of the header byte). Reward and base addresses share the same
-- network-bit encoding.
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
