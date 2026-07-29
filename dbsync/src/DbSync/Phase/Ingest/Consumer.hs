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
import qualified Control.Exception as Exception
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Tracer (traceWith)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', readIORef, writeIORef)
import Data.Time.Clock (getCurrentTime)
import GHC.Stats (RTSStats (..), GCDetails (..), getRTSStats, getRTSStatsEnabled)
import System.Mem (performMajorGC)

import DbSync.AppM (IngestM)
import DbSync.App.Env (CoreEnv (..), IngestEnv (..))
import DbSync.ChainSync.Msg (ChainSyncMsg (..))
import DbSync.Db.Schema.Ids (BlockId (..))
import DbSync.Error (BlockAnnotation (..))
import DbSync.Extractor (ExtractState (..), cborCaptureEnabled, takeBlockLedgerData)
import DbSync.Extractor.EpochBoundary (runEpochBoundary)
import DbSync.Extractor.Governance (runGovernanceBoundary)
import DbSync.Extractor.PoolStats (runPoolStatsBoundary)
import DbSync.Extractor.StakeDelegationLedger (runStakeDelegationLedgerBoundary)
import DbSync.Extractor.Pipeline (processBlock)
import DbSync.Parser.Dispatch (parseBlock)
import DbSync.Parser.Types (GenericBlock (..))
import DbSync.Phase.Ingest.Boundary
  ( ConsumerLoopState (..)
  , PendingBoundary (..)
  , handleEpochBoundary
  , newConsumerLoopState
  , readBoundaryApplyResult
  , recordMemSample
  , renderBoundaryPercent
  , fmtBytes
  )
import DbSync.StateQuery
  ( ObservationResult (..)
  , ObservedTransition (..)
  , SlotDetails (..)
  , getSlotDetails
  , isInterpreterCached
  , observeBlockSTM
  , renderEraIdx
  )
import DbSync.SyncState.Manager (mkBoundarySyncStateRow)
import DbSync.SyncState.Row (writeSyncState)
import DbSync.Schema.Version (currentSchemaVersion)
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Replay
  ( ReplayAdvance (..)
  , ReplayLog (..)
  , ReplayLogState (..)
  , advanceReplay
  , renderReplayPercent
  )
import DbSync.Trace.Timing (fmtCount, fmtF2)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))
import DbSync.Db.Loader (LoaderStream (..))
import DbSync.App.Config.Types
  ( LedgerConfig (..)
  , SyncConfig (..)
  , OptionFlag (..)
  , DbProfile (..)
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

-- | Block stride (not a duration) between RTS memory samples feeding
-- the per-epoch peak; coarse enough to keep 'getRTSStats' off the
-- every-block path.
memPeakSampleInterval :: Word64
memPeakSampleInterval = 256

-- | Block stride between mid-fill forced-major-GC probes.
-- 'clsBlockCount' resets each boundary, so a probe fires this many
-- blocks into every epoch.
majorGcProbeInterval :: Word64
majorGcProbeInterval = 5000

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
            )
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
          ledgerEnabled = lcEnabled (scLedger (getConfig ie))
          schemaVersion = currentSchemaVersion
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
      liftIO $ awaitTxOutDrained txOutWorker
      addressIdCounter <- liftIO $ readAddressIdCounter txOutWorker
      mPending <- liftIO $ readIORef (clsPendingBoundary cls)
      for_ mPending $ \pb ->
        writeSyncState $
          mkBoundarySyncStateRow
            (pbLastSlot pb) (pbLastBlockNo pb) (pbLastHash pb)
            (pbCounters pb) addressIdCounter
            schemaVersion ledgerEnabled
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
      minSev        <- asks (ceMinSeverity . ieCore)
      sqv           <- asks ieStateQueryVar
      hasLedger     <- asks ieHasLedgerEnv
      extractStRef  <- asks ieExtractState
      bootSlot      <- asks ieLastCommittedSlotAtBoot
      replayStart   <- asks ieReplayStartSlot
      let slot     = blockSlot cardanoBlock
          isReplay = case bootSlot of
            Just bs -> slot <= bs
            Nothing -> False

      advanceReplayLog tracer (clsReplay cls) slot bootSlot replayStart

      -- Replayed blocks bypass processBlock, but the worker still
      -- enqueues a per-block ApplyResult; drain and discard so the
      -- queue doesn't backpressure the worker.
      when isReplay $
        liftIO $ void $ takeBlockLedgerData hasLedger

      -- Replayed blocks are already in PG; skip processBlock.
      unless isReplay $ do
        obsResult <- liftIO $ atomically $ observeBlockSTM sqv cardanoBlock
        logObservation tracer obsResult

        sd <- getSlotDetails slot
        cborEnabled <- asks (cborCaptureEnabled . ceExtractors . ieCore)
        let !genBlock   = parseBlock cborEnabled sd cardanoBlock
            !blockEpoch = sdEpochNo sd
            blockAnn    = BlockAnnotation
                            (unSlotNo  (blkSlotNo  genBlock))
                            (unBlockNo (blkBlockNo genBlock))
                            (blkHash   genBlock)
            -- Tag every exception thrown while processing this block
            -- with its slot/hash for the crash log.
            inBlock :: IngestM a -> IngestM a
            inBlock act = withRunInIO $ \run -> Exception.annotateIO blockAnn (run act)

        prevEpoch <- liftIO $ readIORef (clsPrevEpoch cls)
        -- On the first block (fresh boot or post-replay resume),
        -- restart the epoch timer so socket waits don't bleed into
        -- the first epoch's elapsed-seconds stat.
        when (isNothing prevEpoch) $
          liftIO $ getCurrentTime >>= writeIORef (clsEpochStart cls)

        case prevEpoch of
          Just prev | prev /= blockEpoch ->
            inBlock (handleEpochBoundary cls prev slot)
          _ -> pure ()

        inBlock (processBlock genBlock)

        -- Boundary-block extractor: epoch-table writes that depend
        -- on the ledger worker's @apNewEpoch@.
        --
        -- The first processed block needs the same drain when it
        -- crosses an epoch relative to the pre-boot chain (the
        -- genesis block on a fresh boot; the first non-replay block
        -- on resume): the worker enqueues a 'BoundaryApplyData' for
        -- it too, and an unmatched payload shifts every later drain
        -- onto its predecessor's payload.
        boundaryBlock <- case prevEpoch of
          Just prev -> pure (prev /= blockEpoch)
          Nothing   -> bootBlockCrossesBoundary bootSlot blockEpoch
        when boundaryBlock $
          inBlock (runBoundaryExtractor hasLedger extractStRef)

        liftIO $ modifyIORef' (clsBlockCount cls) (+ 1)
        liftIO $ writeIORef (clsPrevEpoch cls) (Just blockEpoch)
        liftIO $ writeIORef (clsLastBlock cls) $ Just
          ( unSlotNo (blkSlotNo genBlock)
          , unBlockNo (blkBlockNo genBlock)
          , blkHash genBlock
          )

        -- Sample RTS memory for the per-epoch peak on a coarse block
        -- stride so 'getRTSStats' stays off the every-block path.
        liftIO $ do
          n <- readIORef (clsBlockCount cls)
          when (n `rem` memPeakSampleInterval == 0) $
            recordMemSample (clsMemStats cls)

        -- Mid-fill forced-major-GC probe (Debug-gated). Distinguishes
        -- reachable per-epoch structure from floating garbage: force a
        -- major GC partway through the fill and log live before/after.
        -- liveAfter far below liveBefore ⇒ the growth was collectible;
        -- liveAfter tracking liveBefore ⇒ a reachable structure.
        -- naturalMajorGCs across lines (minus the one forced per
        -- probe) shows whether GHC majors fire mid-fill.
        -- performMajorGC always completes, so this cannot wedge the loop.
        liftIO $ when (minSev <= Debug) $ do
          n <- readIORef (clsBlockCount cls)
          enabled <- getRTSStatsEnabled
          when (enabled && n `rem` majorGcProbeInterval == 0) $ do
            before <- getRTSStats
            performMajorGC
            after <- getRTSStats
            traceWith tracer $ LogMsg Debug "MajorGcProbe"
              ( "blk " <> show n
                  <> " liveBefore=" <> fmtBytes (gcdetails_live_bytes (gc before))
                  <> " liveAfter=" <> fmtBytes (gcdetails_live_bytes (gc after))
                  <> " naturalMajorGCs=" <> show (major_gcs before)
              )

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
advanceReplayLog tracer replayRef slot bootSlot replayStart =
  -- Early-out before any IO: a fresh sync has no replay window at
  -- all, and on a resume the window closes permanently once the
  -- machine reaches 'NoReplay'. Without this the clock read +
  -- atomic CAS run on every block of the entire sync. The ref is
  -- only written by this thread, so the plain read is exact.
  when (isJust bootSlot) $ do
    st <- liftIO $ readIORef replayRef
    case st of
      NoReplay -> pure ()
      _        -> step
  where
    step :: IngestM ()
    step = do
      now <- liftIO getCurrentTime
      logEvent <- liftIO $ atomicModifyIORef' replayRef $ \prev ->
        let adv = advanceReplay slot bootSlot now prev
        in (raNewState adv, raLog adv)
      let traceReplay msg =
            liftIO $ traceWith tracer $ LogMsg Info "LedgerReplay" msg
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
          <> renderEraIdx (otFromEra t) <> " → " <> renderEraIdx (otToEra t)
          <> " at slot " <> show (unSlotNo (otAtSlot t))
          <> " (epoch " <> show (unEpochNo (otAtEpoch t)) <> ")"
      )
  ObservationBroken fromEra toEra -> do
    cached <- isInterpreterCached
    unless cached $
      liftIO $ traceWith tracer $ LogMsg Warning "StateQuery"
        ( "Observed era jump too large ("
            <> renderEraIdx fromEra <> " → " <> renderEraIdx toEra
            <> "); falling back to node interpreter"
        )
  Unchanged -> pure ()

-- | Whether the first processed block crosses an epoch boundary
-- relative to the pre-boot chain. Mirrors the ledger worker's
-- 'mkOnNewEpoch' enqueue condition: the genesis block of a fresh
-- boot always crosses (into epoch 0); a resumed boot crosses exactly
-- when this block's epoch is one past the boot slot's. Anything else
-- (same epoch, or a gap the worker would not fire on) must not
-- trigger a drain — 'readBoundaryApplyResult' blocks until a payload
-- arrives, so an unmatched drain here would hang the consumer.
bootBlockCrossesBoundary :: Maybe SlotNo -> EpochNo -> IngestM Bool
bootBlockCrossesBoundary mBootSlot blockEpoch = case mBootSlot of
  Nothing -> pure True
  Just bs -> do
    bootSd <- getSlotDetails bs
    pure (unEpochNo blockEpoch == 1 + unEpochNo (sdEpochNo bootSd))

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
  :: HasLedgerEnv
  -> IORef ExtractState
  -> IngestM ()
runBoundaryExtractor hasLedger extractStRef = case hasLedger of
  LedgerDisabled _   -> pure ()
  LedgerEnabled lenv -> do
    applyResult  <- liftIO $ readBoundaryApplyResult lenv
    mLastBlockId <- liftIO $ esLastBlockId <$> readIORef extractStRef
    governanceOn <- asks (prEnabled . pcGovernance . scDbProfile . getConfig)
    poolStatsOn  <- asks (prEnabled . pcPoolStats  . scDbProfile . getConfig)
    sdlOn        <- asks (prEnabled . pcStakeDelegationLedger . scDbProfile . getConfig)
    for_ mLastBlockId $ \lastBid -> do
      -- Governance runs first when enabled: it refreshes the
      -- enacted-state ids and apGovExpiresAfter on ExtractState that
      -- EpochBoundary then reads when it constructs the epoch_state
      -- row.
      when governanceOn $
        runGovernanceBoundary applyResult (BlockId lastBid)
      runEpochBoundary applyResult (BlockId lastBid)
      when poolStatsOn $
        runPoolStatsBoundary applyResult (BlockId lastBid)
      when sdlOn $
        runStakeDelegationLedgerBoundary applyResult (BlockId lastBid)

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
