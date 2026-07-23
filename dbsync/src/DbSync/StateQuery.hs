{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Slot-details service.
--
-- Computes 'SlotDetails' (epoch, time, slot-within-epoch, epoch size)
-- via a 'History.Interpreter' wrapping a hard-fork 'Summary'.
--
-- == Fallback order
--
-- 'getSlotDetailsIO' tries three sources, in order, before throwing:
--
-- 1. /Cached interpreter/ ('sqvInterpreterVar'). Seeded at boot from a
--    loaded snapshot via 'seedInterpreterFromLedgerState', then
--    re-seeded by the ledger worker after every block apply. The hot
--    path.
--
-- 2. /Locally-observed summary/ ('sqvObservedVar'). Built incrementally
--    by 'observeBlockSTM' as ChainSync delivers blocks. Skipped when
--    'isObservationBroken' is set — a broken summary would still
--    answer (its current era is 'EraUnbounded'), but with the wrong
--    era classification because the past-era list is missing entries.
--
-- 3. /Node 'GetInterpreter'/ via the LSQ protocol (driven by
--    'DbSync.StateQuery.Handler.localStateQueryHandler'). Last resort:
--    round-trips through the node's LedgerDB. Validated against the
--    requested slot before being cached; if the node's LedgerDB is
--    still behind the chain tip, the response cannot answer and we
--    back off and retry instead of poisoning the cache.
--
-- == Module layout
--
-- This module hosts the 'getSlotDetails*' entry points and the
-- helpers that thread them together. The pieces it depends on live
-- in:
--
-- * "DbSync.StateQuery.Types"   — 'SlotDetails', 'StateQueryVar', accessor classes
-- * "DbSync.StateQuery.Var"     — 'newStateQueryVar', 'RetryConfig'
-- * "DbSync.StateQuery.Observe" — 'observeBlockSTM' (called from the ChainSync receiver)
-- * "DbSync.StateQuery.Seed"    — 'seedInterpreterFromLedgerState', 'isInterpreterCached'
-- * "DbSync.StateQuery.Handler" — LSQ mini-protocol driver
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

-- | Get 'SlotDetails' for a given 'SlotNo'.
--
-- Reads the tracer, 'StateQueryVar' and 'SystemStart' from env;
-- delegates to 'getSlotDetailsIO' for the actual resolution.
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

-- | Get 'SlotDetails' for a given 'SlotNo' (raw 'IO' bridge).
--
-- Exposed so that callers without an 'IngestEnv' on hand (notably the
-- 'DbSync.Worker.Ledger.Worker' hooks, which only have 'LedgerEnv' +
-- 'StateQueryVar') can still reach it without spinning up an
-- 'IngestM' action.
--
-- Uses 'defaultRetryConfig' for the node fallback; tests inject a
-- faster schedule via 'getSlotDetailsIOWith'.
getSlotDetailsIO
  :: AppTracer
  -> StateQueryVar
  -> SystemStart
  -> SlotNo
  -> IO SlotDetails
getSlotDetailsIO = getSlotDetailsIOWith defaultRetryConfig

-- | 'getSlotDetailsIO' with a caller-supplied 'RetryConfig'. Production
-- code uses 'getSlotDetailsIO' (= 'defaultRetryConfig'); tests pass a
-- microsecond-scale config to keep the suite fast.
--
-- Resolution order:
--
-- 1. Cached interpreter ('sqvInterpreterVar'). On success, return.
-- 2. Locally-observed summary ('sqvObservedVar'), unless
--    'isObservationBroken' is set.
-- 3. Node 'GetInterpreter' via the LSQ request channel. Validated
--    against the requested slot; if too narrow, do not cache, back off
--    per 'RetryConfig', and retry.
--
-- Throws 'AppBlockError' if all attempts in (3) fail, or if the LSQ
-- channel returns an unexpected 'AcquireFailure' other than
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

-- | Try the cached interpreter and then the observed summary. Returns
-- 'Just sd' the first time either source can answer; 'Nothing' if
-- neither can.
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

-- | Build a 'Qry' that computes 'SlotDetails' for a given slot.
-- Uses the HardFork Interpreter's built-in epoch\/slot\/time calculation.
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
