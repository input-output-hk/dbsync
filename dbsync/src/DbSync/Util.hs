{- |
Module      : DbSync.Util
Description : Small generic helpers shared across db-sync.

Keep this module lean: only prelude-style conversions and other
utilities that have no natural home in a feature module belong
here. Ledger-specific helpers stay co-located with their types
(e.g. 'DbSync.Era.Shelley.Generic.Rewards' owns reward-type
conversions). Helpers are added on demand rather than up-front.
-}
module DbSync.Util
  ( maybeToStrictMaybe
  , strictMaybeToMaybe
  ) where

import Cardano.Prelude

import qualified Data.Strict.Maybe as Strict

-- | Convert a lazy prelude 'Maybe' into a strict 'Strict.Maybe'.
--
-- Used wherever we hand values to @cardano-ledger@ \/ @ouroboros-consensus@
-- structures that insist on the strict variant (most @NewEpochState@
-- derivatives, for instance).
maybeToStrictMaybe :: Maybe a -> Strict.Maybe a
maybeToStrictMaybe Nothing  = Strict.Nothing
maybeToStrictMaybe (Just a) = Strict.Just a

-- | Inverse of 'maybeToStrictMaybe'.
strictMaybeToMaybe :: Strict.Maybe a -> Maybe a
strictMaybeToMaybe Strict.Nothing  = Nothing
strictMaybeToMaybe (Strict.Just a) = Just a
