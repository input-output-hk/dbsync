{- |
Module      : DbSync.StateQuery.Types
Description : Shared types for the LocalStateQuery integration.

Lives in its own module so 'DbSync.Env' (and any other consumer that
just needs the shape of the handle) can depend on these definitions
without pulling in 'DbSync.StateQuery'\'s monadic helpers — the latter
import 'DbSync.Env' to read environment-bound fields, so the type
definitions must sit below the @Env -> StateQuery@ dependency arrow.
-}
module DbSync.StateQuery.Types
  ( -- * Types
    SlotDetails (..)
  , CardanoInterpreter
  , StateQueryVar (..)
  ) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo, EpochSize, SlotNo)

import Control.Concurrent.STM (TMVar, TVar)

import Data.Time.Clock (UTCTime)

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

-- | The HardFork Interpreter, correctly handles all era transitions.
type CardanoInterpreter = Interpreter (CardanoEras StandardCrypto)

-- | Channel for LocalStateQuery request\/response communication and a
-- locally-observed fallback summary.
--
-- Three slots:
--
-- * 'sqvRequestVar' — used by 'DbSync.StateQuery.getHistoryInterpreter'
--   (the slow, blocking path) to request an interpreter from the node
--   via 'DbSync.StateQuery.localStateQueryHandler'.
-- * 'sqvInterpreterVar' — caches the node's authoritative interpreter
--   once acquired. 'Just' means we have it; 'Nothing' means we don't.
-- * 'sqvObservedVar' — locally-observed summary, updated by
--   'DbSync.StateQuery.observeBlockSTM' as ChainSync delivers blocks.
data StateQueryVar = StateQueryVar
  { sqvRequestVar     :: !(TMVar ( Query (CardanoBlock StandardCrypto) CardanoInterpreter
                                 , TMVar (Either AcquireFailure CardanoInterpreter)
                                 ))
  , sqvInterpreterVar :: !(TVar (Maybe CardanoInterpreter))
  , sqvObservedVar    :: !(TVar ObservedSummary)
  }
