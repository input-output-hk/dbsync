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
  , unitIntervalToRational

    -- * JSON / PostgreSQL interop
  , jsonValueContainsNul
  ) where

import Cardano.Prelude

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..))

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text as Text

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

unitIntervalToRational :: Ledger.UnitInterval -> Rational
unitIntervalToRational = Ledger.unboundRational

-- ---------------------------------------------------------------------------
-- * JSON / PostgreSQL interop
-- ---------------------------------------------------------------------------

-- | Whether any string in the value — object keys included — contains
-- a Unicode NUL (@U+0000@). PostgreSQL rejects @\\u0000@ in @jsonb@
-- values (and NUL bytes in @text@), so such a value cannot be stored
-- as-is: callers store SQL @NULL@ or a placeholder object instead and
-- rely on the raw-bytes column as ground truth.
jsonValueContainsNul :: Aeson.Value -> Bool
jsonValueContainsNul = go
  where
    go :: Aeson.Value -> Bool
    go = \case
      Aeson.String t -> hasNul t
      Aeson.Array xs -> any go xs
      Aeson.Object o ->
        KeyMap.foldrWithKey
          (\k v acc -> hasNul (Aeson.Key.toText k) || go v || acc)
          False
          o
      _ -> False

    hasNul :: Text -> Bool
    hasNul = Text.any (== '\NUL')
