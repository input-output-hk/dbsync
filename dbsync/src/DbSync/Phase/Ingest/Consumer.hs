{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Block consumer for 'IngestChainHistory'.
--
-- Drains 'ChainSyncMsg' values from the env's 'TBQueue'. Forward
-- blocks are parsed into 'GenericBlock', extractors run, and rows
-- are written via the 'LoaderStream'. Epoch boundaries are detected
-- on 'sdEpochNo' change and delegate to
-- 'DbSync.Phase.Ingest.Boundary.handleEpochBoundary' for the
-- commit + reopen cascade. Rollback markers are unreachable in this
-- phase — the receiver only enqueues them above @chain_tip − k@ and
-- the consumer exits before drainage; one slipping through panics.
--
-- == Per-epoch progress log
--
-- At each boundary 'handleEpochBoundary' emits one summary line:
--
-- @
-- Epoch 265 | 21,427 blk in 41s (526 blk/s) | utxo HR=99.92% | [63.21%]
-- @
--
-- The bracketed percentage is the current block's position relative
-- to the rollback boundary (@nodeTip − k@), omitted while the chain
-- is shorter than @k@. The @blk in X@ window spans the previous
-- boundary's post-commit reset through this boundary's, so the rate
-- reflects what the operator sees.
--
-- Pipeline-internal diagnostics (queue depths, drain distribution,
-- receiver writes-blocked counter) live on the watchdog at 'Debug'.
module DbSync.Phase.Ingest.Consumer
  ( -- * Running
    runConsumer

    -- * Queue utilities
  , drainTBQueue

    -- * Rollback-boundary predicate (exported for tests)
  , rollbackBoundaryReached

    -- * Boundary-percent rendering (exported for tests)
  , renderBoundaryPercent

    -- * Ingest-time rollback panic (exported for tests)
  , ingestRollbackPanicMessage
  ) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import Control.Concurrent.STM (TBQueue, TVar, readTBQueue, readTVarIO, tryReadTBQueue)
import Control.Tracer (traceWith)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', readIORef, writeIORef)
import Data.Time.Clock (getCurrentTime)

import DbSync.AppM (IngestM)
import DbSync.App.Env (IngestEnv (..))
import DbSync.ChainSync.Msg (ChainSyncMsg (..))
import DbSync.Db.Schema.Ids (BlockId (..))
import DbSync.Extractor (ExtractState (..))
import DbSync.Extractor.EpochBoundary (runEpochBoundary)
import DbSync.Extractor.Governance (runGovernanceBoundary)
import DbSync.Extractor.Pipeline (processBlock)
import DbSync.Parser.Dispatch (parseBlock)
import DbSync.Parser.Types (GenericBlock (..))
import DbSync.Phase.Ingest.Boundary
  ( ConsumerLoopState (..)
  , PendingBoundary (..)
  , handleEpochBoundary
  , newConsumerLoopState
  , readBoundaryApplyResult
  )
import DbSync.Phase.Ingest.PipelineStats (PipelineStats (..))
import DbSync.StateQuery
  ( ObservationResult (..)
  , ObservedTransition (..)
  , SlotDetails (..)
  , getSlotDetails
  , isInterpreterCached
  , observeBlockSTM
  )
import DbSync.SyncState.Manager (mkBoundarySyncStateRow)
import DbSync.SyncState.Row (writeSyncState)
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Pulse (bumpPulse)
import DbSync.Trace.Replay
  ( ReplayAdvance (..)
  , ReplayLog (..)
  , ReplayLogState
  , advanceReplay
  , renderReplayPercent
  )
import DbSync.Trace.Timing (fmtCount, fmtF2)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))
import DbSync.Trace.Watchdog (Watchdog, bumpConsumer, setConsumerNote)
import DbSync.Db.Loader (LoaderStream (..))
import DbSync.App.Config.Types
  ( LedgerConfig (..)
  , SyncConfig (..)
  , SyncOption (..)
  , SyncOptions (..)
  )
import DbSync.App.Env (HasConfig (..))
import DbSync.Worker.Ledger.Types (HasLedgerEnv (..))
import DbSync.Worker.TxOut.AddressBuffer (emptyEpochAddressBuffer)
import qualified DbSync.Worker.TxOut.ConsumedByBuffer as ConsumedByBuffer
import DbSync.Worker.TxOut.Worker
  ( TxOutJob (..)
  , awaitTxOutDrained
  , enqueueTxOutJob
  , readAddressIdCounter
  )

import Ouroboros.Consensus.Block (blockSlot)
import Ouroboros.Consensus.Shelley.HFEras ()                -- per-era HFC instances
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()  -- 'LedgerSupportsProtocol' orphans

-- ---------------------------------------------------------------------------
-- * Running
-- ---------------------------------------------------------------------------

-- | Run the consumer loop in 'IngestM'.
--
-- Drives a 'ConsumerLoopState' across the loop iterations; the inner
-- loop runs in 'IngestM' itself so 'processBlock' stays env-aware.
runConsumer :: IngestM ()
runConsumer = do
  bootSlot      <- asks ieLastCommittedSlotAtBoot
  cls           <- liftIO $ newConsumerLoopState bootSlot
  tracer        <- asks getTracer
  boundaryVar   <- asks ieRollbackBoundary
  loop tracer boundaryVar cls
  where
    batchSize :: Int
    batchSize = 100

    loop :: AppTracer -> TVar (Maybe BlockNo) -> ConsumerLoopState -> IngestM ()
    loop tracer boundaryVar cls = do
      queue <- asks ieBlockQueue
      blocks <- liftIO $ drainTBQueue queue batchSize
      let !drainSize = length blocks

      statsRef <- asks iePipelineStats
      liftIO $ modifyIORef' statsRef $ \ps -> ps
        { psDrainTotal   = psDrainTotal ps + fromIntegral drainSize
        , psDrainCount   = psDrainCount ps + 1
        , psSingleDrains = psSingleDrains ps + if drainSize == 1 then 1 else 0
        , psFullDrains   = psFullDrains ps + if drainSize >= batchSize then 1 else 0
        }

      processBatch cls blocks

      reached <- liftIO $ rollbackBoundaryReached (clsLastBlock cls) boundaryVar
      if reached
        then do
          finalFlushSyncState cls
          mLast <- liftIO $ readIORef (clsLastBlock cls)
          liftIO $ traceWith tracer $ LogMsg Info "Ingest"
            ( "reached rollback boundary at "
                <> renderLastBlock mLast
                <> "; exiting consumer loop"
            ) Nothing
        else
          loop tracer boundaryVar cls

    renderLastBlock :: Maybe (Word64, Word64, ByteString) -> Text
    renderLastBlock = \case
      Nothing                 -> "(no block processed yet)"
      Just (slot, blk, _hash) ->
        "block " <> show blk <> " (slot " <> show slot <> ")"

    -- | Drain the final queued resolve job and advance @sync_state@
    -- to the last fully-completed epoch. During normal operation
    -- @sync_state@ lags by one epoch; at the rollback boundary the
    -- consumer exits mid-epoch so the previous epoch is the last
    -- commit point. No-op when the pipeline never crossed a boundary.
    finalFlushSyncState :: ConsumerLoopState -> IngestM ()
    finalFlushSyncState cls = do
      ie <- ask
      let txOutWorker  = ieTxOutWorker ie
          mConsumedBuf = ieConsumedByBuffer ie
          loaderStream = ieLoaderStream ie
          watchdog     = ieWatchdog ie
          ledgerEnabled = lcEnabled (scLedger (getConfig ie))
          schemaVersion = 1 :: Int
      -- Drain any residual mid-epoch consumed-by pairs via one last
      -- job carrying an empty address buffer. When consumed-by is
      -- off this branch is unreachable.
      mResidualCb <- liftIO $ case mConsumedBuf of
        Just ref -> Just <$> ConsumedByBuffer.takeAndReset ref
        Nothing  -> pure Nothing
      for_ mResidualCb $ \cb ->
        liftIO $ enqueueTxOutJob txOutWorker $ TxOutJob
          { tjEpoch      = EpochNo 0
          , tjAddress    = emptyEpochAddressBuffer
          , tjConsumedBy = Just cb
          }
      liftIO $ setConsumerNote watchdog "consumer: final awaitTxOutDrained"
      liftIO $ awaitTxOutDrained txOutWorker
      addressIdCounter <- liftIO $ readAddressIdCounter txOutWorker
      mPending <- liftIO $ readIORef (clsPendingBoundary cls)
      for_ mPending $ \pb ->
        writeSyncState $
          mkBoundarySyncStateRow
            (pbLastSlot pb) (pbLastBlockNo pb) (pbLastHash pb)
            (pbCounters pb) addressIdCounter
            schemaVersion ledgerEnabled
      liftIO $ setConsumerNote watchdog "consumer: final lsCommit"
      liftIO $ lsCommit loaderStream

    processBatch :: ConsumerLoopState -> [ChainSyncMsg] -> IngestM ()
    processBatch _   []                       = pure ()
    processBatch _   (MsgRollback point : _)  =
      -- Reaching this branch means the node sent a rollback for a
      -- block below the @chain_tip − k@ boundary — a k-safety
      -- violation. Crash loudly so the operator can investigate.
      panic (ingestRollbackPanicMessage point)
    processBatch cls (MsgForward cardanoBlock : rest) = do
      tracer        <- asks getTracer
      sqv           <- asks ieStateQueryVar
      hasLedger     <- asks ieHasLedgerEnv
      extractStRef  <- asks ieExtractState
      bootSlot      <- asks ieLastCommittedSlotAtBoot
      replayStart   <- asks ieReplayStartSlot
      watchdog      <- asks ieWatchdog
      pulse         <- asks iePulse
      let slot     = blockSlot cardanoBlock
          isReplay = case bootSlot of
            Just bs -> slot <= bs
            Nothing -> False

      advanceReplayLog tracer (clsReplay cls) slot bootSlot replayStart

      -- Replayed blocks are already in PG; skip processBlock.
      unless isReplay $ do
        liftIO $ setConsumerNote watchdog "consumer: observeBlock"
        obsResult <- liftIO $ atomically $ observeBlockSTM sqv cardanoBlock
        logObservation tracer obsResult

        liftIO $ setConsumerNote watchdog "consumer: getSlotDetails"
        sd <- getSlotDetails slot
        liftIO $ setConsumerNote watchdog "consumer: parseBlock"
        let !genBlock   = parseBlock sd cardanoBlock
            !blockEpoch = sdEpochNo sd

        prevEpoch <- liftIO $ readIORef (clsPrevEpoch cls)
        -- On the first block (fresh boot or post-replay resume),
        -- restart the epoch timer so socket waits don't bleed into
        -- the first epoch's elapsed-seconds stat.
        when (isNothing prevEpoch) $
          liftIO $ getCurrentTime >>= writeIORef (clsEpochStart cls)

        case prevEpoch of
          Just prev | prev /= blockEpoch ->
            handleEpochBoundary cls prev slot
          _ -> pure ()

        liftIO $ setConsumerNote watchdog "consumer: processBlock"
        processBlock genBlock

        -- Boundary-block extractor: epoch-table writes that depend
        -- on the ledger worker's @apNewEpoch@.
        case prevEpoch of
          Just prev | prev /= blockEpoch ->
            runBoundaryExtractor watchdog hasLedger extractStRef
          _ -> pure ()

        liftIO $ modifyIORef' (clsBlockCount cls) (+ 1)
        liftIO $ writeIORef (clsPrevEpoch cls) (Just blockEpoch)
        liftIO $ writeIORef (clsLastBlock cls) $ Just
          ( unSlotNo (blkSlotNo genBlock)
          , unBlockNo (blkBlockNo genBlock)
          , blkHash genBlock
          )

      -- Bump watchdog + pulse on every iteration, even during replay,
      -- so the watchdog sees forward progress while 'processBlock' is
      -- skipped. 'bumpPulse' is a no-op above 'Debug'.
      liftIO $ bumpConsumer watchdog slot
      liftIO $ bumpPulse pulse

      processBatch cls rest

-- ---------------------------------------------------------------------------
-- * Per-block helpers
-- ---------------------------------------------------------------------------

-- | Step the replay-progress state machine for this block and emit
-- any progress / completion trace lines.
advanceReplayLog
  :: AppTracer
  -> IORef ReplayLogState
  -> SlotNo
  -> Maybe SlotNo
  -> Maybe SlotNo
  -> IngestM ()
advanceReplayLog tracer replayRef slot bootSlot replayStart = do
  now <- liftIO getCurrentTime
  logEvent <- liftIO $ atomicModifyIORef' replayRef $ \prev ->
    let adv = advanceReplay slot bootSlot now prev
    in (raNewState adv, raLog adv)
  let traceReplay msg =
        liftIO $ traceWith tracer $ LogMsg Info "LedgerReplay" msg Nothing
  case logEvent of
    ReplayLogNothing -> pure ()
    ReplayLogProgress n ->
      traceReplay $
        "applied " <> fmtCount n <> " blocks; current slot "
          <> show (unSlotNo slot)
          <> renderReplayPercent replayStart bootSlot slot
    ReplayLogComplete n elapsed ->
      traceReplay $
        "replay complete; applied " <> fmtCount n
          <> " blocks in " <> fmtF2 (realToFrac elapsed :: Double)
          <> "s, resuming loader stream at slot " <> show (unSlotNo slot)

-- | Trace any era transition observed by 'observeBlockSTM'. Skips
-- the "falling back to node" warning when the interpreter is
-- already cached — the observed-summary path isn't used in that case.
logObservation :: AppTracer -> ObservationResult -> IngestM ()
logObservation tracer = \case
  NewTransition t ->
    liftIO $ traceWith tracer $ LogMsg Info "StateQuery"
      ( "Observed era transition "
          <> show (otFromEra t) <> " → " <> show (otToEra t)
          <> " at slot " <> show (unSlotNo (otAtSlot t))
          <> " (epoch " <> show (unEpochNo (otAtEpoch t)) <> ")"
      ) Nothing
  ObservationBroken fromEra toEra -> do
    cached <- isInterpreterCached
    unless cached $
      liftIO $ traceWith tracer $ LogMsg Warning "StateQuery"
        ( "Observed era jump too large ("
            <> show fromEra <> " → " <> show toEra
            <> "); falling back to node interpreter"
        ) Nothing
  Unchanged -> pure ()

-- | Drain the worker's next boundary 'ApplyResult' and run the
-- governance and epoch-boundary extractors against it. No-op when
-- the ledger feature is disabled or no block has yet been
-- extracted.
--
-- 'runGovernanceBoundary' is only invoked when the @governance@
-- extractor is enabled — its writes target tables owned by that
-- extractor (committee, constitution, drep_distr, gov_action_proposal
-- status columns), which don't exist when governance is off. The
-- resolver's enacted-id snapshot then stays at its default
-- @(Nothing, Nothing, Nothing)@ and 'runEpochBoundary' writes NULLs
-- into @epoch_state@'s three governance FK columns.
runBoundaryExtractor
  :: Watchdog
  -> HasLedgerEnv
  -> IORef ExtractState
  -> IngestM ()
runBoundaryExtractor watchdog hasLedger extractStRef = case hasLedger of
  LedgerDisabled _   -> pure ()
  LedgerEnabled lenv -> do
    liftIO $ setConsumerNote watchdog "consumer: readBoundaryApplyResult"
    applyResult  <- liftIO $ readBoundaryApplyResult lenv
    mLastBlockId <- liftIO $ esLastBlockId <$> readIORef extractStRef
    governanceOn <- asks (prEnabled . pcGovernance . scOptions . getConfig)
    for_ mLastBlockId $ \lastBid -> do
      -- Governance runs first when enabled: it refreshes the
      -- enacted-state ids and apGovExpiresAfter on ExtractState that
      -- EpochBoundary then reads when it constructs the epoch_state
      -- row.
      when governanceOn $ do
        liftIO $ setConsumerNote watchdog "consumer: runGovernanceBoundary"
        runGovernanceBoundary applyResult (BlockId lastBid)
      liftIO $ setConsumerNote watchdog "consumer: runEpochBoundary"
      runEpochBoundary applyResult (BlockId lastBid)

-- ---------------------------------------------------------------------------
-- * Queue utilities
-- ---------------------------------------------------------------------------

-- | Drain up to @maxN@ blocks from the queue. Blocks until at least
-- one is available, then takes as many as are immediately available
-- (up to @maxN@) without waiting.
drainTBQueue :: forall a. TBQueue a -> Int -> IO [a]
drainTBQueue q maxN = atomically $ do
  hd <- readTBQueue q
  rest <- go (maxN - 1)
  pure (hd : rest)
  where
    go :: Int -> STM [a]
    go 0 = pure []
    go n = do
      mVal <- tryReadTBQueue q
      case mVal of
        Nothing  -> pure []
        Just val -> (val :) <$> go (n - 1)

-- ---------------------------------------------------------------------------
-- * Rollback-boundary predicate
-- ---------------------------------------------------------------------------

-- | 'True' when the most recently processed block has reached the
-- finalised-tip boundary (@nodeTip − k@). Returns 'False' if either
-- ref is unset — we haven't seen a block yet, or the receiver
-- hasn't observed a tip at or above @k@.
rollbackBoundaryReached
  :: IORef (Maybe (Word64, Word64, ByteString))  -- ^ Last processed (slot, blockNo, hash)
  -> TVar  (Maybe BlockNo)                       -- ^ Latest @nodeTip − k@
  -> IO Bool
rollbackBoundaryReached lastRef boundaryVar = do
  mLast     <- readIORef lastRef
  mBoundary <- readTVarIO boundaryVar
  pure $ case (mLast, mBoundary) of
    (Just (_slot, lastBlock, _hash), Just (BlockNo b)) -> lastBlock >= b
    _                                                  -> False

-- | The panic message issued when 'IngestChainHistory' receives a
-- 'MsgRollback'. Should be unreachable in practice; the receiver
-- only enqueues rollback markers above @chain_tip − k@ and the
-- consumer exits before drainage. Exposed so the test suite can pin
-- the message shape.
ingestRollbackPanicMessage :: Show point => point -> Text
ingestRollbackPanicMessage point =
  "IngestChainHistory: received MsgRollback at "
    <> show point
    <> "; this should be impossible (k-safety violation)."

-- | Render the Ingest progress segment of the form @\" | [87.32%]\"@.
-- The percentage is the current block's position relative to the
-- current node tip (derived from the published rollback boundary
-- plus @k@). Returns @\"\"@ when the boundary is still 'Nothing'
-- (chain shorter than @k@ blocks), when no block has been processed
-- yet, or when the derived tip is zero.
renderBoundaryPercent :: Maybe BlockNo -> Word64 -> Maybe Word64 -> Text
renderBoundaryPercent (Just (BlockNo boundary)) k (Just curBlock)
  | tip > 0 =
      let raw     = (fromIntegral curBlock / fromIntegral tip :: Double) * 100
          clamped = max 0 (min 100 raw)
      in " | [" <> fmtF2 clamped <> "%]"
  where
    tip = boundary + k
renderBoundaryPercent _ _ _ = ""
