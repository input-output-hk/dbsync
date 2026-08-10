-- | Shared types for the LocalStateQuery integration. They sit below
-- the @Env -> StateQuery@ dependency arrow, so 'DbSync.App.Env' can
-- use the handle without importing 'DbSync.StateQuery'.
module DbSync.StateQuery.Types
  ( -- * Types
    SlotDetails (..)
  , CardanoInterpreter
  , StateQueryVar (..)

    -- * Accessor classes
  , HasStateQueryVar (..)
  , HasSystemStart (..)
  ) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo, EpochSize, SlotNo)

import Control.Concurrent.STM (TMVar, TVar)

import Data.Time.Clock (UTCTime)

import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart)
import Ouroboros.Consensus.Cardano.Block
  ( CardanoBlock
  , CardanoEras
  , StandardCrypto
  )
import Ouroboros.Consensus.HardFork.History.Qry (Interpreter)
import Ouroboros.Consensus.Ledger.Query (Query)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)

import DbSync.StateQuery.ObservedSummary (ObservedSummary)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Slot details computed by the HardFork Interpreter.
data SlotDetails = SlotDetails
  { sdSlotTime    :: !UTCTime
  , sdCurrentTime :: !UTCTime
  , sdEpochNo     :: !EpochNo
  , sdSlotNo      :: !SlotNo
  , sdEpochSlot   :: !Word64
  , sdEpochSize   :: !EpochSize
  }
  deriving stock (Eq, Show)

type CardanoInterpreter = Interpreter (CardanoEras StandardCrypto)

-- | LocalStateQuery request channel plus the two interpreter sources.
data StateQueryVar = StateQueryVar
  { sqvRequestVar     :: !(TMVar ( Query (CardanoBlock StandardCrypto) CardanoInterpreter
                                 , TMVar (Either AcquireFailure CardanoInterpreter)
                                 ))
    -- ^ Query paired with the reply slot the requester blocks on.
    --   'DbSync.StateQuery.localStateQueryHandler' drains it.
  , sqvInterpreterVar :: !(TVar (Maybe CardanoInterpreter))
    -- ^ Cached authoritative interpreter. 'Nothing' until a snapshot
    --   or the node supplies one.
  , sqvObservedVar    :: !(TVar ObservedSummary)
    -- ^ Fallback summary, updated per block by
    --   'DbSync.StateQuery.observeBlockSTM'.
  }

class HasStateQueryVar env where
  getStateQueryVar :: env -> StateQueryVar

-- | Needed alongside 'HasStateQueryVar' to turn relative slot times
-- into UTC.
class HasSystemStart env where
  getSystemStart :: env -> SystemStart
