{- |
Module      : DbSync.Util
Description : Small generic helpers shared across db-sync.

Keep this module lean: only prelude-style conversions and other
utilities that have no natural home in a feature module belong
here. Ledger-specific helpers stay co-located with their types
(e.g. 'DbSync.Era.Shelley.Rewards' owns reward-type
conversions). Helpers are added on demand rather than up-front.
-}
module DbSync.Util
  ( -- * Strict-Maybe interop
    maybeToStrictMaybe
  , strictMaybeToMaybe

    -- * Coin conversions
  , coinToDbLovelace
  , coinToWord64
  , coinToInt64

    -- * Reward address
  , rewardAddrCred
  ) where

import Cardano.Prelude

import Cardano.Ledger.Coin (Coin (..))

import qualified Data.ByteString as BS
import qualified Data.Strict.Maybe as Strict

import DbSync.Db.Types (DbLovelace (..))

-- ---------------------------------------------------------------------------
-- * Strict-Maybe interop
-- ---------------------------------------------------------------------------

-- | Convert a lazy prelude 'Maybe' into a strict 'Strict.Maybe'.
--
-- Used wherever we hand values to @cardano-ledger@ \/ @ouroboros-consensus@
-- structures that insist on the strict variant (most @NewEpochState@
-- derivatives, for instance).
--
-- This is __not__ the same type as @cardano-ledger@\'s @StrictMaybe@ —
-- that is @Cardano.Ledger.BaseTypes.StrictMaybe@. This helper covers
-- the @data-strict@ flavour used by 'DbSync.Era.Shelley'.
maybeToStrictMaybe :: Maybe a -> Strict.Maybe a
maybeToStrictMaybe Nothing  = Strict.Nothing
maybeToStrictMaybe (Just a) = Strict.Just a

-- | Inverse of 'maybeToStrictMaybe'.
strictMaybeToMaybe :: Strict.Maybe a -> Maybe a
strictMaybeToMaybe Strict.Nothing  = Nothing
strictMaybeToMaybe (Strict.Just a) = Just a

-- ---------------------------------------------------------------------------
-- * Coin conversions
-- ---------------------------------------------------------------------------

-- | Project a ledger 'Coin' to its database lovelace representation.
coinToDbLovelace :: Coin -> DbLovelace
coinToDbLovelace (Coin n) = DbLovelace (fromInteger n)

-- | Project a ledger 'Coin' to a 'Word64'.
coinToWord64 :: Coin -> Word64
coinToWord64 (Coin n) = fromInteger n

-- | Project a ledger 'Coin' to an 'Int64'. Used for fields that may
-- legitimately hold a negative delta (e.g. tx deposit refunds).
coinToInt64 :: Coin -> Int64
coinToInt64 (Coin n) = fromInteger n

-- ---------------------------------------------------------------------------
-- * Reward address
-- ---------------------------------------------------------------------------

-- | Strip the 1-byte network header from a reward address, yielding
-- the underlying 28-byte credential hash. Reward addresses serialise
-- as @network_id || credential_hash@; the credential is what we
-- store and dedup on.
--
-- Short inputs (length \<= 1) are returned unchanged on the
-- assumption the caller has handed us malformed data and we'd rather
-- propagate the bytes than panic.
rewardAddrCred :: ByteString -> ByteString
rewardAddrCred bs
  | BS.length bs > 1 = BS.drop 1 bs
  | otherwise        = bs
