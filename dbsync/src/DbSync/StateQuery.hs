{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Local state query integration for epoch\/slot computation.
--
-- Queries the cardano-node for a 'CardanoInterpreter' via the
-- LocalStateQuery mini-protocol. The interpreter correctly maps
-- any slot to its epoch, time, and position across all era
-- transitions (Byron → Shelley → Allegra → ...).
--
-- Ported from @Cardano.DbSync.LocalStateQuery@ and
-- @Cardano.DbSync.StateQuery@ in the original cardano-db-sync.
module DbSync.StateQuery
  ( -- * Types
    SlotDetails (..)
  , CardanoInterpreter
  , StateQueryVar (..)

    -- * Construction
  , newStateQueryVar

    -- * Querying
  , getSlotDetails

    -- * Protocol handler
  , localStateQueryHandler
  ) where

import Cardano.Prelude hiding (atomically)

import Cardano.Slotting.Slot (EpochNo, EpochSize, SlotNo (..))

import Control.Concurrent.STM
  ( TMVar
  , atomically
  , newEmptyTMVarIO
  , newTVarIO
  , putTMVar
  , readTVar
  , takeTMVar
  , writeTVar
  )
import Control.Concurrent.STM.TVar (TVar)
import Control.Tracer (Tracer, traceWith)

import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)

import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , SystemStart (..)
  )
import Ouroboros.Consensus.Cardano.Block
  ( BlockQuery (QueryHardFork)
  , CardanoBlock
  , CardanoEras
  , StandardCrypto
  )
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
  ( QueryHardFork (GetInterpreter)
  )
import Ouroboros.Consensus.HardFork.History.Qry
  ( Expr (..)
  , Interpreter
  , PastHorizonException
  , Qry
  , interpretQuery
  , qryFromExpr
  , slotToEpoch'
  )
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Network.Block (Point)
import Ouroboros.Network.Protocol.LocalStateQuery.Client
  ( ClientStAcquired (..)
  , ClientStAcquiring (..)
  , ClientStIdle (..)
  , ClientStQuerying (..)
  , LocalStateQueryClient (..)
  )
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure, Target (..))

import DbSync.Error (AppError (..), throwAppError)
import DbSync.Trace.Types (LogMsg (..), Severity (..))

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Slot details computed by the HardFork Interpreter.
-- Replaces the manual epoch\/slot math in 'EpochSlotInfo'.
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

-- | Channel for LocalStateQuery request\/response communication.
data StateQueryVar = StateQueryVar
  { sqvRequestVar    :: !(TMVar ( Query (CardanoBlock StandardCrypto) CardanoInterpreter
                                , TMVar (Either AcquireFailure CardanoInterpreter)
                                ))
  , sqvInterpreterVar :: !(TVar (Maybe CardanoInterpreter))
  }

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

-- | Create a new 'StateQueryVar' with an empty interpreter cache.
newStateQueryVar :: IO StateQueryVar
newStateQueryVar = StateQueryVar <$> newEmptyTMVarIO <*> newTVarIO Nothing

-- ---------------------------------------------------------------------------
-- * Querying
-- ---------------------------------------------------------------------------

-- | Get 'SlotDetails' for a given 'SlotNo'.
--
-- On first call, queries the node for a 'CardanoInterpreter' via
-- LocalStateQuery and caches it. Subsequent calls use the cache.
-- If the cached interpreter fails (e.g. past horizon), re-queries.
getSlotDetails
  :: HasCallStack
  => StateQueryVar
  -> SystemStart
  -> SlotNo
  -> IO SlotDetails
getSlotDetails sqv systemStart slot = do
  -- Try cached interpreter first
  mInterp <- atomically $ readTVar (sqvInterpreterVar sqv)
  case mInterp of
    Just interp -> case evalSlotDetails interp of
      Right sd -> insertCurrentTime sd
      Left _ -> do
        -- Cached interpreter failed, get a new one
        interp' <- getHistoryInterpreter sqv
        case evalSlotDetails interp' of
          Left err -> throwAppError AppBlockError $
            "getSlotDetails: " <> show err
          Right sd -> insertCurrentTime sd
    Nothing -> do
      interp <- getHistoryInterpreter sqv
      case evalSlotDetails interp of
        Left err -> throwAppError AppBlockError $
          "getSlotDetails: " <> show err
        Right sd -> insertCurrentTime sd
  where
    evalSlotDetails :: CardanoInterpreter -> Either PastHorizonException SlotDetails
    evalSlotDetails interp = interpretQuery interp (querySlotDetails systemStart slot)

    insertCurrentTime :: SlotDetails -> IO SlotDetails
    insertCurrentTime sd = do
      now <- getCurrentTime
      pure sd { sdCurrentTime = now }

-- | Query the node for a 'CardanoInterpreter'.
--
-- Sends a @GetInterpreter@ request via the LocalStateQuery TMVar channel
-- and waits for the response. Caches the interpreter for future use.
getHistoryInterpreter :: HasCallStack => StateQueryVar -> IO CardanoInterpreter
getHistoryInterpreter sqv = do
  respVar <- newEmptyTMVarIO
  atomically $ putTMVar (sqvRequestVar sqv) (BlockQuery $ QueryHardFork GetInterpreter, respVar)
  res <- atomically $ takeTMVar respVar
  case res of
    Left err -> throwAppError AppBlockError $
      "getHistoryInterpreter: " <> show err
    Right interp -> do
      atomically $ writeTVar (sqvInterpreterVar sqv) (Just interp)
      pure interp

-- ---------------------------------------------------------------------------
-- * Query expression
-- ---------------------------------------------------------------------------

-- | Build a 'Qry' that computes 'SlotDetails' for a given slot.
-- Uses the HardFork Interpreter's built-in epoch\/slot\/time calculation.
-- Ported from @Cardano.DbSync.StateQuery.querySlotDetails@.
querySlotDetails :: SystemStart -> SlotNo -> Qry SlotDetails
querySlotDetails start absSlot = do
  absTime <- qryFromExpr $
    ELet (EAbsToRelSlot (ELit absSlot)) $ \relSlot ->
      ELet (ERelSlotToTime (EVar relSlot)) $ \relTime ->
        ELet (ERelToAbsTime (EVar relTime)) $ \absTime ->
          EVar absTime
  (absEpoch, slotInEpoch) <- slotToEpoch' absSlot
  epochSize <- qryFromExpr $ EEpochSize (ELit absEpoch)
  let time = relToUTCTime start absTime
  pure SlotDetails
    { sdSlotTime    = time
    , sdCurrentTime = time  -- corrected later in insertCurrentTime
    , sdEpochNo     = absEpoch
    , sdSlotNo      = absSlot
    , sdEpochSlot   = slotInEpoch
    , sdEpochSize   = epochSize
    }

-- | Convert a 'RelativeTime' to 'UTCTime' given a 'SystemStart'.
relToUTCTime :: SystemStart -> RelativeTime -> UTCTime
relToUTCTime (SystemStart start) (RelativeTime rel) = addUTCTime rel start

-- ---------------------------------------------------------------------------
-- * Protocol handler
-- ---------------------------------------------------------------------------

-- | LocalStateQuery protocol client that handles interpreter requests.
--
-- Loops forever, reading requests from the 'StateQueryVar' TMVar,
-- sending them to the node via Acquire → Query → Release, and
-- writing responses back to the response TMVar.
localStateQueryHandler
  :: StateQueryVar
  -> LocalStateQueryClient
       (CardanoBlock StandardCrypto)
       (Point (CardanoBlock StandardCrypto))
       (Query (CardanoBlock StandardCrypto))
       IO
       a
localStateQueryHandler sqv =
  LocalStateQueryClient idleState
  where
    idleState :: IO (ClientStIdle (CardanoBlock StandardCrypto) (Point (CardanoBlock StandardCrypto)) (Query (CardanoBlock StandardCrypto)) IO a)
    idleState = do
      (query, respVar) <- atomically $ takeTMVar (sqvRequestVar sqv)
      pure
        . SendMsgAcquire VolatileTip
        $ ClientStAcquiring
          { recvMsgAcquired =
              pure . SendMsgQuery query $
                ClientStQuerying
                  { recvMsgResult = \result -> do
                      atomically $ putTMVar respVar (Right result)
                      pure $ SendMsgRelease idleState
                  }
          , recvMsgFailure = \failure -> do
              atomically $ putTMVar respVar (Left failure)
              idleState
          }
