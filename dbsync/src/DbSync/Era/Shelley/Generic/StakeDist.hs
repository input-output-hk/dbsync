{- |
Module      : DbSync.Era.Shelley.Generic.StakeDist
Description : Era-agnostic stake-distribution slice types.

Scope: __data types only__.

'StakeSlice' and 'StakeSliceRes' are the era-collapsed shape in which
we incrementally insert stake-distribution rows across the blocks of
an epoch. A single slice is a list of @(stake-credential, (amount,
pool))@ triples; 'NoSlices' signals that this block has no
distribution entries to emit.

The /functions/ that compute slices from an 'ExtLedgerState'
— @getStakeSlice@, @fullEpochStake@, @getPoolDistr@, and friends —
will live in 'DbSync.Ledger.State' where they're actually called.
-}
module DbSync.Era.Shelley.Generic.StakeDist
  ( StakeSliceRes (..)
  , StakeSlice (..)
  ) where

import Cardano.Prelude

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Slotting.Slot (EpochNo)

import DbSync.Ledger.Keys (PoolKeyHash, StakeCred)

-- | Per-block result of the stake-distribution extraction.
--
--   * 'Slice' — an actual slice of entries. The 'Bool' is 'True' for
--     the last slice of this epoch and is used only for logging.
--   * 'NoSlices' — Byron blocks, or blocks where the index is past the
--     end of the delegation vector.
data StakeSliceRes
  = Slice !StakeSlice !Bool
  | NoSlices

-- | One slice of the stake distribution — a list of
-- @(credential, (amount, pool))@ triples tagged with the epoch whose
-- stake it describes.
data StakeSlice = StakeSlice
  { sliceEpochNo :: !EpochNo
  , sliceDistr   :: ![(StakeCred, (Coin, PoolKeyHash))]
  }
  deriving stock (Eq)
