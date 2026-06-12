-- | Small generic helpers with no natural home in a feature module.
-- Feature-specific conversions stay co-located with their types.
module DbSync.Util
  ( -- * Strict-Maybe interop
    maybeToStrictMaybe
  , strictMaybeToMaybe

    -- * Coin conversions
  , coinToDbLovelace
  , coinToWord64
  , coinToInt64

    -- * Nonce / UnitInterval conversions
  , nonceToBytes
  , unitIntervalToDouble

    -- * Reward address
  , rewardAddrCred
  ) where

import Cardano.Prelude

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..))

import qualified Data.ByteString as BS
import qualified Data.Strict.Maybe as Strict

import DbSync.Db.Types (DbLovelace (..))

-- ---------------------------------------------------------------------------
-- * Strict-Maybe interop
-- ---------------------------------------------------------------------------

-- | The @data-strict@ variant, /not/ @Cardano.Ledger.BaseTypes.StrictMaybe@.
maybeToStrictMaybe :: Maybe a -> Strict.Maybe a
maybeToStrictMaybe Nothing  = Strict.Nothing
maybeToStrictMaybe (Just a) = Strict.Just a

strictMaybeToMaybe :: Strict.Maybe a -> Maybe a
strictMaybeToMaybe Strict.Nothing  = Nothing
strictMaybeToMaybe (Strict.Just a) = Just a

-- ---------------------------------------------------------------------------
-- * Coin conversions
-- ---------------------------------------------------------------------------

coinToDbLovelace :: Coin -> DbLovelace
coinToDbLovelace (Coin n) = DbLovelace (fromInteger n)

coinToWord64 :: Coin -> Word64
coinToWord64 (Coin n) = fromInteger n

-- | Used for fields that may legitimately hold a negative delta
-- (e.g. tx deposit refunds).
coinToInt64 :: Coin -> Int64
coinToInt64 (Coin n) = fromInteger n

-- ---------------------------------------------------------------------------
-- * Nonce / UnitInterval conversions
-- ---------------------------------------------------------------------------

-- | A neutral nonce maps to @NULL@ in @nonce@ \/ @extra_entropy@
-- columns; an actual epoch nonce maps to its 32-byte digest.
nonceToBytes :: Ledger.Nonce -> Maybe ByteString
nonceToBytes = \case
  Ledger.NeutralNonce -> Nothing
  Ledger.Nonce hash   -> Just (Crypto.hashToBytes hash)

-- | A 'UnitInterval' is a 'Rational' in [0, 1]; database columns
-- store it as a text-encoded 'Double'.
unitIntervalToDouble :: Ledger.UnitInterval -> Double
unitIntervalToDouble = fromRational . Ledger.unboundRational

-- ---------------------------------------------------------------------------
-- * Reward address
-- ---------------------------------------------------------------------------

-- | Strip the 1-byte network header from a reward address
-- (@network_id || credential_hash@) and return the 28-byte
-- credential. Short inputs (length \<= 1) pass through unchanged
-- rather than panicking on malformed data.
rewardAddrCred :: ByteString -> ByteString
rewardAddrCred bs
  | BS.length bs > 1 = BS.drop 1 bs
  | otherwise        = bs
