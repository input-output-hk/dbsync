{- |
Module      : DbSync.Era.Shelley
Description : Umbrella re-export for the era-generic projections.

Callers usually want \"the era-collapsed view of everything\" in one
import. This module re-exports the sub-modules with the @Generic@
prefix kept implicit.

Current members:

  * "DbSync.Era.Shelley.Rewards"

  * "DbSync.Era.Shelley.ProtoParams"

  * "DbSync.Era.Shelley.EpochUpdate"

  * "DbSync.Era.Shelley.StakeDist" (types only)
-}
module DbSync.Era.Shelley
  ( module X
  ) where

import DbSync.Era.Shelley.EpochUpdate as X
import DbSync.Era.Shelley.ProtoParams as X
import DbSync.Era.Shelley.Rewards    as X
import DbSync.Era.Shelley.StakeDist  as X
