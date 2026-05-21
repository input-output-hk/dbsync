{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Local state query integration for epoch\/slot computation.
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
--    On a clean genesis sync where every transition is observed it's
--    a free local answer; on a resume from a non-Byron tip it's
--    bypassed and we go straight to (3).
--
-- 3. /Node 'GetInterpreter'/ via the LSQ protocol. Last resort:
--    round-trips through the node's LedgerDB. Validated against the
--    requested slot before being cached; if the node's LedgerDB is
--    still behind the chain tip, the response cannot answer and we
--    back off and retry instead of poisoning the cache.
--
-- The retry on (3) is the safety net for the parallel-sync workflow
-- where dbsync's ChainSync stream runs ahead of cardano-node's
-- LedgerDB replay: the node's interpreter is then a snapshot of an
-- early-replay state and cannot answer slots the consumer is
-- processing. Each retry re-checks the local sources first so the
-- moment the ledger worker lands a fresh seed in 'sqvInterpreterVar'
-- we use it instead of going back to the node.
module DbSync.StateQuery
  ( -- * Types
    SlotDetails (..)
  , CardanoInterpreter
  , StateQueryVar (..)
  , RetryConfig (..)

    -- * Construction
  , newStateQueryVar
  , defaultRetryConfig

    -- * Querying
  , getSlotDetails
  , getSlotDetailsIO
  , getSlotDetailsIOWith
  , isInterpreterCached

    -- * Local observation
  , observeBlockSTM
  , ObservationResult (..)
  , ObservedTransition (..)
  , EraIdx (..)

    -- * Snapshot-derived interpreter seeding
  , seedInterpreterFromLedgerState

    -- * Protocol handler
  , localStateQueryHandler
  ) where

import Cardano.Prelude hiding (atomically)

import Cardano.Slotting.Slot (SlotNo (..))

import Control.Concurrent.STM
  ( atomically
  , newEmptyTMVarIO
  , newTVarIO
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
import Ouroboros.Consensus.Cardano.Block
  ( BlockQuery (QueryHardFork)
  , CardanoBlock
  , StandardCrypto
  )
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
import Ouroboros.Consensus.HardFork.Abstract (hardForkSummary)
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
  ( QueryHardFork (GetInterpreter)
  )
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.HardFork.History.Qry
  ( Expr (..)
  , PastHorizonException
  , Qry
  , interpretQuery
  , qryFromExpr
  , slotToEpoch'
  )
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Network.Block (Point)
import Ouroboros.Network.Protocol.LocalStateQuery.Client
  ( ClientStAcquired (..)
  , ClientStAcquiring (..)
  , ClientStIdle (..)
  , ClientStQuerying (..)
  , LocalStateQueryClient (..)
  )
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..), Target (..))

import DbSync.Error (throwBlock)
import DbSync.StateQuery.ObservedSummary
  ( EraIdx (..)
  , ObservationResult (..)
  , ObservedTransition (..)
  , currentInterpreter
  , initObservedSummary
  , isObservationBroken
  , observeBlock
  )
import qualified DbSync.StateQuery.Types as SQT
import DbSync.StateQuery.Types
  ( CardanoInterpreter
  , HasStateQueryVar (..)
  , HasSystemStart
  , SlotDetails (..)
  , StateQueryVar (..)
  )
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

-- | Create a new 'StateQueryVar' with an empty interpreter cache and a
-- Byron-only initial observed summary derived from the consensus
-- 'TopLevelConfig'.
newStateQueryVar
  :: TopLevelConfig (CardanoBlock StandardCrypto)
  -> IO StateQueryVar
newStateQueryVar topLevelCfg =
  StateQueryVar
    <$> newEmptyTMVarIO
    <*> newTVarIO Nothing
    <*> newTVarIO (initObservedSummary topLevelCfg)

-- ---------------------------------------------------------------------------
-- * Snapshot-derived interpreter seeding
-- ---------------------------------------------------------------------------

-- | Pre-fill 'sqvInterpreterVar' from a loaded ledger state's
-- hard-fork summary.
--
-- Used at boot when resuming from a snapshot: 'hardForkSummary'
-- produces the same 'Interpreter' the node would return via
-- @GetInterpreter@, so per-block 'getSlotDetails' calls serve
-- locally instead of round-tripping to the node. The summary only
-- covers eras up to the snapshot's tip; queries past its horizon
-- fall through to 'getHistoryInterpreterIO' as before.
seedInterpreterFromLedgerState
  :: TopLevelConfig (CardanoBlock StandardCrypto)
  -> ExtLedgerState (CardanoBlock StandardCrypto) mk
  -> StateQueryVar
  -> IO ()
seedInterpreterFromLedgerState topLevelCfg ExtLedgerState{ ledgerState = ls } sqv = do
  let summary = hardForkSummary (configLedger topLevelCfg) ls
      interp  = History.mkInterpreter summary
  atomically $ writeTVar (sqvInterpreterVar sqv) (Just interp)

-- | True when 'sqvInterpreterVar' has been seeded (snapshot or node).
-- Lets callers suppress observed-summary fallback diagnostics that
-- would otherwise mislead.
isInterpreterCached
  :: (HasStateQueryVar env, MonadReader env m, MonadIO m)
  => m Bool
isInterpreterCached = do
  sqv <- asks getStateQueryVar
  liftIO $ isJust <$> atomically (readTVar (sqvInterpreterVar sqv))

-- ---------------------------------------------------------------------------
-- * Local observation
-- ---------------------------------------------------------------------------

-- | Atomically feed a block to the locally-observed summary.
--
-- Returns the 'ObservationResult' so the caller can trace era
-- transitions. Intended to be called once per block by the consumer,
-- /before/ the corresponding 'getSlotDetails' call so that the
-- transition's epoch boundary is in the summary by the time slot
-- details are computed.
observeBlockSTM
  :: StateQueryVar
  -> CardanoBlock StandardCrypto
  -> STM ObservationResult
observeBlockSTM sqv blk = do
  os <- readTVar (sqvObservedVar sqv)
  let (result, os') = observeBlock blk os
  writeTVar (sqvObservedVar sqv) os'
  pure result

-- ---------------------------------------------------------------------------
-- * Retry policy
-- ---------------------------------------------------------------------------

-- | Retry policy for the node-interpreter fallback in 'getSlotDetailsIOWith'.
--
-- The fallback is taken when neither the cached interpreter nor the
-- observed summary can answer for the requested slot. Each attempt
-- queries the node, validates the response against that slot, and (on
-- failure) sleeps for @'rcBackoffMicros' n@ microseconds before
-- attempt @n + 1@.
data RetryConfig = RetryConfig
  { rcMaxAttempts   :: !Int
    -- ^ Total number of node-query attempts. The last attempt does
    -- not back off; if it fails the call throws.
  , rcBackoffMicros :: !(Int -> Int)
    -- ^ Microseconds to wait between attempts. Argument is the
    -- zero-based index of the attempt that just failed (so the wait
    -- before attempt @n + 1@).
  }

-- | Production retry policy: 10 attempts; geometric backoff capped at
-- 300 seconds; the nine backoffs between the ten attempts sum to
-- 1,800 seconds (= 30 minutes).
--
-- Sequence: 20, 40, 80, 160, 300, 300, 300, 300, 300 seconds.
defaultRetryConfig :: RetryConfig
defaultRetryConfig = RetryConfig
  { rcMaxAttempts   = 10
  , rcBackoffMicros = \n -> 1_000_000 * min 300 (20 * (2 ^ min n (4 :: Int)))
  }

-- ---------------------------------------------------------------------------
-- * Querying
-- ---------------------------------------------------------------------------

-- | Get 'SlotDetails' for a given 'SlotNo'.
--
-- Reads the tracer, 'StateQueryVar' and 'SystemStart' from env;
-- delegates to 'getSlotDetailsIO' for the actual resolution.
getSlotDetails
  :: ( HasCallStack
     , HasTracer env
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
-- This is the implementation 'getSlotDetails' calls under the hood.
-- Exposed so that callers without an 'IngestEnv' on hand (notably the
-- 'DbSync.Ledger.Worker' hooks, which only have 'LedgerEnv' +
-- 'StateQueryVar') can still reach it without spinning up an
-- 'IngestM' action.
--
-- Uses 'defaultRetryConfig' for the node fallback; tests inject a
-- faster schedule via 'getSlotDetailsIOWith'.
getSlotDetailsIO
  :: HasCallStack
  => AppTracer
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
--    'isObservationBroken' is set. A broken summary would answer
--    (its current era is unbounded) but with the wrong era
--    classification, so we skip it and go to (3) instead.
-- 3. Node 'GetInterpreter' via the LSQ request channel. Validated
--    against the requested slot; if too narrow, do not cache, back off
--    per 'RetryConfig', and retry. Each retry re-checks the local
--    sources first.
--
-- Throws 'AppBlockError' if all attempts in (3) fail, or if the LSQ
-- channel returns an unexpected 'AcquireFailure' other than
-- 'AcquireFailurePointTooOld'.
getSlotDetailsIOWith
  :: HasCallStack
  => RetryConfig
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
--
-- The observed summary is skipped when 'isObservationBroken' is set:
-- a broken summary still has its current era as 'EraUnbounded' and
-- would happily answer any slot — but with the /wrong/ era
-- classification, since past-era transitions are missing. Returning a
-- wrong 'SlotDetails' is worse than going to the node, so we only
-- trust the observed summary when it has tracked every era boundary
-- since genesis.
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
--
-- The retry loop re-checks the local sources at the start of every
-- iteration: if the ledger worker (or chainsync observer) has
-- advanced state during the previous backoff, we use it instead of
-- going back to the node.
--
-- A response interpreter is only written to 'sqvInterpreterVar' once
-- it has been validated against the requested slot. Caching a too-narrow
-- interpreter would make every subsequent 'getSlotDetailsIO' call fail
-- until the worker re-seeded, which historically manifested as a hard
-- crash mid-ingest against a node still replaying its LedgerDB.
fetchFromNodeWithRetry
  :: HasCallStack
  => RetryConfig
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
              ) Nothing
          insertCurrentTime sd
        Nothing -> queryNode n

    queryNode n = do
      when (n == 0) $
        traceWith tracer $ LogMsg Info "StateQuery"
          ( "Acquiring history interpreter from node for slot "
              <> show (unSlotNo slot) <> "…"
          ) Nothing
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
                ) Nothing
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
            ) Nothing
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
