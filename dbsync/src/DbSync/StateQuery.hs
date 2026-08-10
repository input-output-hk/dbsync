{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Slot-details service.
--
-- Computes 'SlotDetails' (epoch, time, slot-within-epoch, epoch size)
-- through a hard-fork 'History.Interpreter'. 'getSlotDetailsIOWith'
-- documents the three sources it tries.
module DbSync.StateQuery
  ( -- * Types (re-exports from .Types)
    SlotDetails (..)
  , CardanoInterpreter
  , StateQueryVar (..)

    -- * Construction + retry policy (re-exports from .Var)
  , newStateQueryVar
  , RetryConfig (..)
  , defaultRetryConfig

    -- * Querying
  , getSlotDetails
  , getSlotDetailsIO
  , getSlotDetailsIOWith

    -- * Local observation (re-exports from .Observe / .ObservedSummary)
  , observeBlockSTM
  , ObservationResult (..)
  , ObservedTransition (..)
  , EraIdx (..)
  , renderEraIdx

    -- * Snapshot-derived interpreter seeding (re-exports from .Seed)
  , seedInterpreterFromLedgerState
  , isInterpreterCached

    -- * Protocol handler (re-exports from .Handler)
  , localStateQueryHandler
  ) where

import Cardano.Prelude hiding (atomically)

import Cardano.Slotting.Slot (SlotNo (..))

import Control.Concurrent.STM
  ( atomically
  , newEmptyTMVarIO
  , putTMVar
  , readTVar
  , takeTMVar
  , writeTVar
  )
import Control.Tracer (traceWith)

import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)

import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , SystemStart (..)
  )
import Ouroboros.Consensus.Cardano.Block (BlockQuery (QueryHardFork))
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
  ( QueryHardFork (GetInterpreter)
  )
import Ouroboros.Consensus.HardFork.History.Qry
  ( Expr (..)
  , PastHorizonException
  , Qry
  , interpretQuery
  , qryFromExpr
  , slotToEpoch'
  )
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..))

import DbSync.Error (throwBlock)
import DbSync.StateQuery.Handler (localStateQueryHandler)
import DbSync.StateQuery.Observe (observeBlockSTM)
import DbSync.StateQuery.ObservedSummary
  ( EraIdx (..)
  , renderEraIdx
  , ObservationResult (..)
  , ObservedTransition (..)
  , currentInterpreter
  , isObservationBroken
  )
import DbSync.StateQuery.Seed (isInterpreterCached, seedInterpreterFromLedgerState)
import DbSync.StateQuery.Types
  ( CardanoInterpreter
  , HasStateQueryVar (..)
  , HasSystemStart
  , SlotDetails (..)
  , StateQueryVar (..)
  )
import qualified DbSync.StateQuery.Types as SQT
import DbSync.StateQuery.Var (RetryConfig (..), defaultRetryConfig, newStateQueryVar)
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- ---------------------------------------------------------------------------
-- * Querying
-- ---------------------------------------------------------------------------

getSlotDetails
  :: ( HasTracer env
     , HasStateQueryVar env
     , HasSystemStart env
     , MonadReader env m
     , MonadIO m
     )
  => SlotNo -> m SlotDetails
getSlotDetails slot = do
  tracer      <- asks getTracer
  sqv         <- asks getStateQueryVar
  systemStart <- asks SQT.getSystemStart
  liftIO $ getSlotDetailsIO tracer sqv systemStart slot

-- | Raw 'IO' bridge for callers that hold only a 'StateQueryVar' and
-- no phase env, such as the ledger worker hooks. Uses
-- 'defaultRetryConfig' for the node fallback.
getSlotDetailsIO
  :: AppTracer
  -> StateQueryVar
  -> SystemStart
  -> SlotNo
  -> IO SlotDetails
getSlotDetailsIO = getSlotDetailsIOWith defaultRetryConfig

-- | 'getSlotDetailsIO' with a caller-supplied 'RetryConfig'. Tests
-- pass a microsecond-scale config to keep the suite fast.
--
-- Resolution order:
--
-- 1. Cached interpreter ('sqvInterpreterVar').
-- 2. Locally-observed summary ('sqvObservedVar'), unless
--    'isObservationBroken' is set.
-- 3. Node 'GetInterpreter' over the LSQ request channel. The response
--    is validated against the requested slot; a too-narrow horizon is
--    not cached, so a lagging node LedgerDB cannot poison the cache.
--
-- Throws 'AppBlockError' if every attempt in (3) fails, or if the LSQ
-- channel returns an 'AcquireFailure' other than
-- 'AcquireFailurePointTooOld'.
getSlotDetailsIOWith
  :: RetryConfig
  -> AppTracer
  -> StateQueryVar
  -> SystemStart
  -> SlotNo
  -> IO SlotDetails
getSlotDetailsIOWith rc tracer sqv systemStart slot = do
  mLocal <- tryLocalInterpreters sqv evalSlot
  case mLocal of
    Just sd -> insertCurrentTime sd
    Nothing -> fetchFromNodeWithRetry rc tracer sqv systemStart slot
  where
    evalSlot :: CardanoInterpreter -> Either PastHorizonException SlotDetails
    evalSlot interp = interpretQuery interp (querySlotDetails systemStart slot)

    insertCurrentTime :: SlotDetails -> IO SlotDetails
    insertCurrentTime sd = do
      now <- getCurrentTime
      pure sd { sdCurrentTime = now }

-- | Try the cached interpreter, then the observed summary. 'Nothing'
-- means neither source covers the slot.
tryLocalInterpreters
  :: StateQueryVar
  -> (CardanoInterpreter -> Either PastHorizonException SlotDetails)
  -> IO (Maybe SlotDetails)
tryLocalInterpreters sqv eval = do
  mInterp <- atomically $ readTVar (sqvInterpreterVar sqv)
  case mInterp >>= rightToMaybe . eval of
    Just sd -> pure (Just sd)
    Nothing -> do
      observed <- atomically $ readTVar (sqvObservedVar sqv)
      if isObservationBroken observed
        then pure Nothing
        else pure $ rightToMaybe (eval (currentInterpreter observed))

-- | Acquire an interpreter from the node, retrying on too-narrow
-- horizon and on transient 'AcquireFailurePointTooOld' replies.
fetchFromNodeWithRetry
  :: RetryConfig
  -> AppTracer
  -> StateQueryVar
  -> SystemStart
  -> SlotNo
  -> IO SlotDetails
fetchFromNodeWithRetry rc tracer sqv systemStart slot = go (0 :: Int)
  where
    go n = do
      mLocal <- tryLocalInterpreters sqv evalSlot
      case mLocal of
        Just sd -> do
          when (n > 0) $
            traceWith tracer $ LogMsg Info "StateQuery"
              ( "local interpreter caught up while waiting for node; "
                  <> "slot " <> show (unSlotNo slot)
                  <> " resolved after " <> show n <> " backoff(s)"
              )
          insertCurrentTime sd
        Nothing -> queryNode n

    queryNode n = do
      when (n == 0) $
        traceWith tracer $ LogMsg Info "StateQuery"
          ( "Acquiring history interpreter from node for slot "
              <> show (unSlotNo slot) <> "…"
          )
      respVar <- newEmptyTMVarIO
      atomically $ putTMVar (sqvRequestVar sqv)
        (BlockQuery $ QueryHardFork GetInterpreter, respVar)
      res <- atomically $ takeTMVar respVar
      case res of
        Right interp -> case evalSlot interp of
          Right sd -> do
            when (n > 0) $
              traceWith tracer $ LogMsg Info "StateQuery"
                ( "Node ledger caught up; interpreter acquired after "
                    <> show n <> " retry(s)"
                )
            atomically $ writeTVar (sqvInterpreterVar sqv) (Just interp)
            insertCurrentTime sd
          Left _ -> backoff n
            "Node interpreter horizon is behind the requested slot \
            \(cardano-node LedgerDB still catching up to the chain tip)"
        Left AcquireFailurePointTooOld -> backoff n
          "Node ledger still replaying (AcquireFailurePointTooOld)"
        Left err -> throwBlock $
          "getSlotDetails: unexpected LSQ acquire failure: " <> show err

    backoff n reason
      | n + 1 >= rcMaxAttempts rc = throwBlock $
          "getSlotDetails: unable to resolve slot "
            <> show (unSlotNo slot)
            <> " after " <> show (rcMaxAttempts rc)
            <> " attempts; cardano-node LedgerDB appears stuck behind"
            <> " the chain tip (last reason: " <> reason <> ")"
      | otherwise = do
          let micros = rcBackoffMicros rc n
              secs   = micros `div` 1_000_000
          traceWith tracer $ LogMsg Warning "StateQuery"
            ( reason
                <> " (attempt " <> show (n + 1)
                <> "/" <> show (rcMaxAttempts rc)
                <> "); retrying in " <> show secs <> "s"
            )
          threadDelay micros
          go (n + 1)

    evalSlot :: CardanoInterpreter -> Either PastHorizonException SlotDetails
    evalSlot interp = interpretQuery interp (querySlotDetails systemStart slot)

    insertCurrentTime :: SlotDetails -> IO SlotDetails
    insertCurrentTime sd = do
      now <- getCurrentTime
      pure sd { sdCurrentTime = now }

-- ---------------------------------------------------------------------------
-- * Query expression
-- ---------------------------------------------------------------------------

-- | Build the 'Qry' the HardFork Interpreter evaluates for one slot.
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

relToUTCTime :: SystemStart -> RelativeTime -> UTCTime
relToUTCTime (SystemStart start) (RelativeTime rel) = addUTCTime rel start
