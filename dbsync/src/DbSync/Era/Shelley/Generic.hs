{- |
Module      : DbSync.Era.Shelley.Generic
Description : Umbrella re-export for the era-generic projections.

Callers usually want \"the era-collapsed view of everything\" in one
import. This module re-exports the sub-modules with the @Generic@
prefix kept implicit.

Current members:

  * "DbSync.Era.Shelley.Generic.Rewards"
  * "DbSync.Era.Shelley.Generic.ProtoParams"
  * "DbSync.Era.Shelley.Generic.EpochUpdate"
  * "DbSync.Era.Shelley.Generic.StakeDist" (types only)
-}
module DbSync.Era.Shelley.Generic
  ( module X
  ) where

import DbSync.Era.Shelley.Generic.EpochUpdate as X
import DbSync.Era.Shelley.Generic.ProtoParams as X
import DbSync.Era.Shelley.Generic.Rewards    as X
import DbSync.Era.Shelley.Generic.StakeDist  as X
