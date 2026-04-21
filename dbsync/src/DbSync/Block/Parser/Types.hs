-- | Types for the block parser.
--
-- Contains auxiliary types used by the era-specific converters,
-- including 'EpochSlotInfo' for computing epoch\/slot\/time from
-- slot numbers.
module DbSync.Block.Parser.Types
  ( -- * Epoch\/Slot computation
    EpochSlotInfo (..)
  , stubEpochSlotInfo
  ) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

-- ---------------------------------------------------------------------------
-- * Epoch\/Slot computation
-- ---------------------------------------------------------------------------

-- | Encapsulates the logic for computing epoch number, epoch slot, and
-- UTC time from a 'SlotNo'. Built from 'GenesisConfig' at startup.
--
-- This replaces the original @SlotDetails@ lookup — we compute these
-- values during parsing rather than as a separate step.
--
-- Implementation note: functions are not strict-annotated since thunks
-- are fine for function fields that are always applied.
data EpochSlotInfo = EpochSlotInfo
  { esiSlotToEpochNo   :: SlotNo -> EpochNo
      -- ^ Compute the epoch number for a given slot
  , esiSlotToEpochSlot :: SlotNo -> Word64
      -- ^ Compute the slot offset within its epoch
  , esiSlotToUTCTime   :: SlotNo -> UTCTime
      -- ^ Compute the wall-clock time for a given slot
  }

-- | Placeholder 'EpochSlotInfo' that returns zeroes.
-- Used until the real implementation is wired in (Step 5).
stubEpochSlotInfo :: EpochSlotInfo
stubEpochSlotInfo = EpochSlotInfo
  { esiSlotToEpochNo   = \_ -> EpochNo 0
  , esiSlotToEpochSlot = \_ -> 0
  , esiSlotToUTCTime   = \_ -> UTCTime (fromGregorian 2017 9 23) (secondsToDiffTime 0)
  }
