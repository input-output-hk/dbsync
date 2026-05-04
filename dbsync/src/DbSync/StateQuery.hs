{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Local state query integration for epoch\/slot computation.
--
-- Computes 'SlotDetails' (epoch, time, slot-within-epoch, epoch size)
-- via a 'History.Interpreter' wrapping a hard-fork 'Summary'. Two
-- sources of truth, in priority order:
--
-- 1. /Node-authoritative/. As soon as the node finishes replaying its
--    LedgerDB, we acquire its 'GetInterpreter' result and cache it.
--    Always preferred when available.
-- 2. /Locally-observed/. While the node is still replaying, we build a
--    'History.Summary' incrementally by observing the era of each
--    incoming block via 'observeBlockSTM'. The era boundaries are
--    computed from per-era 'EraParams' (sourced from the consensus
--    library) and the slot of each first-of-era block. Same point of
--    truth as the node, just observed from ChainSync rather than from
--    a replayed LedgerDB.
--
-- If the locally-observed summary cannot answer (e.g. dbsync resumed
-- from a non-Byron tip without observing the preceding transitions),
-- 'getSlotDetails' falls back to the existing retry-with-backoff
-- against the node — matching pre-existing behaviour.
module DbSync.StateQuery
  ( -- * Types
    SlotDetails (..)
  , CardanoInterpreter
  , StateQueryVar (..)

    -- * Construction
  , newStateQueryVar

    -- * Querying
  , getSlotDetails
  , getSlotDetailsIO
  , getHistoryInterpreter
  , getHistoryInterpreterIO
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

import DbSync.AppM (IngestM)
import DbSync.Env (IngestEnv (..))
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
import DbSync.StateQuery.Types
  ( CardanoInterpreter
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
isInterpreterCached :: StateQueryVar -> IO Bool
isInterpreterCached sqv =
  isJust <$> atomically (readTVar (sqvInterpreterVar sqv))

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
-- * Querying
-- ---------------------------------------------------------------------------

-- | Get 'SlotDetails' for a given 'SlotNo' inside 'IngestM'.
--
-- Reads the tracer, 'StateQueryVar' and 'SystemStart' from the
-- 'IngestEnv'; otherwise behaves identically to 'getSlotDetailsIO'.
-- Prefer this in 'IngestM' code paths so the env is not threaded
-- through every helper signature.
getSlotDetails :: HasCallStack => SlotNo -> IngestM SlotDetails
getSlotDetails slot = do
  tracer      <- asks getTracer
  sqv         <- asks ieStateQueryVar
  systemStart <- asks ieSystemStart
  liftIO $ getSlotDetailsIO tracer sqv systemStart slot

-- | Get 'SlotDetails' for a given 'SlotNo' (raw 'IO' bridge).
--
-- This is the implementation 'getSlotDetails' calls under the hood.
-- Exposed so that callers without an 'IngestEnv' on hand (notably the
-- 'DbSync.Ledger.Worker' hooks, which only have 'LedgerEnv' +
-- 'StateQueryVar') can still reach it without spinning up an
-- 'IngestM' action.
--
-- Resolution order:
--
-- 1. If the node's authoritative interpreter has been cached
--    ('sqvInterpreterVar' = 'Just'), use it.
-- 2. Otherwise, use the locally-observed summary
--    ('sqvObservedVar') unless it's marked broken or returns
--    'PastHorizonException' for the requested slot.
-- 3. As a last resort, fall back to 'getHistoryInterpreterIO' which
--    blocks until the node becomes ready (existing retry-with-backoff
--    behaviour).
--
-- The observed-summary path is the hot path during the brief window
-- where the node is still replaying. The cached node path takes over
-- as soon as the node is ready (typically within ~10–30 minutes for
-- mainnet from genesis).
getSlotDetailsIO
  :: HasCallStack
  => AppTracer
  -> StateQueryVar
  -> SystemStart
  -> SlotNo
  -> IO SlotDetails
getSlotDetailsIO tracer sqv systemStart slot = do
  mInterp <- atomically $ readTVar (sqvInterpreterVar sqv)
  case mInterp of
    Just interp ->
      case evalSlotDetails interp of
        Right sd -> insertCurrentTime sd
        Left _ ->
          -- The cached interpreter is stale (e.g. chain progressed
          -- past its summary). Refresh from the node.
          fetchAndEval
    Nothing -> do
      observed <- atomically $ readTVar (sqvObservedVar sqv)
      if isObservationBroken observed
        then fetchAndEval
        else case evalSlotDetails (currentInterpreter observed) of
          Right sd -> insertCurrentTime sd
          -- Observed summary cannot answer (e.g. resume from non-Byron
          -- tip without observing transitions). Fall back to the node.
          Left _ -> fetchAndEval
  where
    evalSlotDetails :: CardanoInterpreter -> Either PastHorizonException SlotDetails
    evalSlotDetails interp = interpretQuery interp (querySlotDetails systemStart slot)

    fetchAndEval :: IO SlotDetails
    fetchAndEval = do
      interp <- getHistoryInterpreterIO tracer sqv
      case evalSlotDetails interp of
        Left err -> throwBlock $
          "getSlotDetails: " <> show err
        Right sd -> insertCurrentTime sd

    insertCurrentTime :: SlotDetails -> IO SlotDetails
    insertCurrentTime sd = do
      now <- getCurrentTime
      pure sd { sdCurrentTime = now }

-- | Query the node for a 'CardanoInterpreter' inside 'IngestM'.
--
-- Reads the tracer and 'StateQueryVar' from the 'IngestEnv'; defers
-- to 'getHistoryInterpreterIO' for the actual retry loop.
getHistoryInterpreter :: HasCallStack => IngestM CardanoInterpreter
getHistoryInterpreter = do
  tracer <- asks getTracer
  sqv    <- asks ieStateQueryVar
  liftIO $ getHistoryInterpreterIO tracer sqv

-- | Query the node for a 'CardanoInterpreter' (raw 'IO' bridge),
-- retrying with capped exponential backoff if the node's LedgerDB is
-- still replaying.
getHistoryInterpreterIO :: HasCallStack => AppTracer -> StateQueryVar -> IO CardanoInterpreter
getHistoryInterpreterIO tracer sqv = go (0 :: Int)
  where
    go n = do
      when (n == 0) $
        traceWith tracer $ LogMsg Info "StateQuery"
          "Acquiring history interpreter from node…" Nothing
      respVar <- newEmptyTMVarIO
      atomically $ putTMVar (sqvRequestVar sqv) (BlockQuery $ QueryHardFork GetInterpreter, respVar)
      res <- atomically $ takeTMVar respVar
      case res of
        Right interp -> do
          when (n > 0) $
            traceWith tracer $ LogMsg Info "StateQuery"
              ("Node ledger ready; interpreter acquired after "
                <> show n <> " retries") Nothing
          atomically $ writeTVar (sqvInterpreterVar sqv) (Just interp)
          pure interp
        -- Treat as "node not ready yet": back off and retry.
        Left AcquireFailurePointTooOld -> do
          let backoffSecs = min 60 (2 * 2 ^ min n 5 :: Int)
          traceWith tracer $ LogMsg Info "StateQuery"
            ("Node ledger still replaying (attempt " <> show (n + 1)
              <> "); retrying in " <> show backoffSecs <> "s") Nothing
          threadDelay (backoffSecs * 1_000_000)
          go (n + 1)
        Left err -> throwBlock $
          "getHistoryInterpreter: " <> show err

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
