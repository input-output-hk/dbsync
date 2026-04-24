{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Domain-specific newtypes for database column types.
--
-- These newtypes ensure type safety for Lovelace values and large
-- Word64 values that must be stored as PostgreSQL @numeric@.
--
-- Hasql encoders\/decoders are added later when 'FollowingChainTip'
-- INSERT support is implemented.
module DbSync.Db.Types
  ( -- * Types
    DbLovelace (..)
  , DbWord64 (..)
  ) where

import Cardano.Prelude

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Lovelace values stored as PostgreSQL @numeric(20,0)@.
--
-- Uses a newtype rather than raw 'Word64' so that:
--
--   * The column type is unambiguous at the Haskell level.
--   * Encoders\/decoders can be swapped in later without changing call sites.
--   * Values that exceed @Int64@ range are handled correctly via @numeric@.
newtype DbLovelace = DbLovelace { unDbLovelace :: Word64 }
  deriving stock (Eq, Ord)
  deriving newtype (Show, Read)

-- | Large unsigned integers stored as PostgreSQL @numeric@.
--
-- Same motivation as 'DbLovelace' but for non-monetary Word64 columns
-- (e.g. @invalid_before@, @invalid_hereafter@).
newtype DbWord64 = DbWord64 { unDbWord64 :: Word64 }
  deriving stock (Eq, Ord)
  deriving newtype (Show, Read, Num)
