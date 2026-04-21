{- |
Module      : DbSync.Checkpoint.Types
Description : Checkpoint types for resumable sync progress.

A 'Checkpoint' captures the complete sync state at an epoch boundary,
allowing the application to resume from a known-good position after
a restart. Checkpoints include ID counters, dedup map paths, and
the block hash needed to verify chain continuity.
-}
module DbSync.Checkpoint.Types
  ( -- * Types
    Checkpoint (..)
  ) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo)
import Cardano.Slotting.Slot (EpochNo, SlotNo)

import DbSync.Id.Counter (IdCounters)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | A serialisable snapshot of sync progress at an epoch boundary.
--
-- Written to disk at each epoch commit during 'IngestChainHistory'.
-- On restart, the latest valid checkpoint is loaded and sync resumes
-- from the recorded block/slot.
data Checkpoint = Checkpoint
  { cpEpoch          :: !EpochNo
      -- ^ Epoch at which this checkpoint was taken
  , cpBlockNo        :: !BlockNo
      -- ^ Last block number processed in this epoch
  , cpSlotNo         :: !SlotNo
      -- ^ Slot of the last processed block
  , cpIdCounters     :: !IdCounters
      -- ^ Monotonic ID counter state at checkpoint time
  , cpDedupMapsPath  :: !FilePath
      -- ^ Path to the serialised dedup maps on disk
  , cpLedgerPath     :: !FilePath
      -- ^ Path to the serialised ledger state snapshot
  , cpExtractors    :: ![Text]
      -- ^ Names of enabled extractors at checkpoint time
  , cpLastBlockHash  :: !ByteString
      -- ^ Hash of the last processed block (for chain continuity verification)
  , cpDbSyncVersion  :: !Text
      -- ^ Version of db-sync that created this checkpoint
  }
  deriving stock (Show)
