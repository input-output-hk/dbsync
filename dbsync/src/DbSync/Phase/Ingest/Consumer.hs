{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Block consumer for 'IngestChainHistory'. It drains
-- 'ChainSyncMsg' values from the env's 'TBQueue', parses each
-- forward block into a 'GenericBlock', runs the extractors, and
-- writes rows through the 'LoaderStream'. An 'sdEpochNo' change
-- hands off to
-- 'DbSync.Phase.Ingest.Boundary.handleEpochBoundary'.
--
-- The receiver enqueues rollback markers at any depth, but this
-- consumer exits at @nodeTip − k@, below which the chain cannot roll
-- back. A marker that still arrives panics.
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
  , Extractors (..)
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

-- | Block stride, not a duration, between RTS memory samples. It is
-- coarse enough to keep 'getRTSStats' off the every-block path.
memPeakSampleInterval :: Word64
memPeakSampleInterval = 256

-- | Block stride between mid-fill forced-major-GC probes.
-- 'clsBlockCount' resets each boundary, so a probe fires this many
-- blocks into every epoch.
majorGcProbeInterval :: Word64
majorGcProbeInterval = 5000

-- | Drives a 'ConsumerLoopState' across the loop iterations. The
-- inner loop runs in 'IngestM', so 'processBlock' stays env-aware.
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

    -- Drain the final queued resolve job and advance @sync_state@ to
    -- the last completed epoch. The consumer exits mid-epoch at the
    -- rollback boundary, so the previous epoch is the last commit
    -- point. It does nothing when the pipeline crossed no boundary.
    finalFlushSyncState :: ConsumerLoopState -> IngestM ()
    finalFlushSyncState cls = do
      ie <- ask
      let txOutWorker  = ieTxOutWorker ie
          mConsumedBuf = ieConsumedByBuffer ie
          loaderStream = ieLoaderStream ie
          ledgerEnabled = lcEnabled (scLedger (getConfig ie))
          schemaVersion = currentSchemaVersion
      -- Drain the residual mid-epoch consumed-by pairs through one
      -- last job with an empty address buffer. Nothing reaches this
      -- branch when consumed-by is off.
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
      -- This branch means the node sent a rollback for a block below
      -- the @chain_tip − k@ boundary, which violates k-safety.
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

      -- A replayed block skips processBlock, but the worker still
      -- enqueues a per-block ApplyResult. Drain and discard it, or
      -- the queue backpressures the worker.
      when isReplay $
        liftIO $ void $ takeBlockLedgerData hasLedger

      -- PG already holds the replayed blocks.
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
            -- Tag every exception from this block with its slot and
            -- hash for the crash log.
            inBlock :: IngestM a -> IngestM a
            inBlock act = withRunInIO $ \run -> Exception.annotateIO blockAnn (run act)

        prevEpoch <- liftIO $ readIORef (clsPrevEpoch cls)
        -- Restart the epoch timer on the first block, so socket waits
        -- stay out of the first epoch's elapsed-seconds stat.
        when (isNothing prevEpoch) $
          liftIO $ getCurrentTime >>= writeIORef (clsEpochStart cls)

        case prevEpoch of
          Just prev | prev /= blockEpoch ->
            inBlock (handleEpochBoundary cls prev slot)
          _ -> pure ()

        inBlock (processBlock genBlock)

        -- Epoch-table writes that depend on the ledger worker's
        -- @apNewEpoch@. The first processed block needs the same
        -- drain when it crosses an epoch against the pre-boot chain:
        -- the worker enqueues a 'BoundaryApplyData' for it too, and
        -- an unmatched payload shifts every later drain onto its
        -- predecessor's payload.
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

        liftIO $ do
          n <- readIORef (clsBlockCount cls)
          when (n `rem` memPeakSampleInterval == 0) $
            recordMemSample (clsMemStats cls)

        -- Debug-gated probe separating reachable per-epoch structure
        -- from floating garbage. A liveAfter far below liveBefore
        -- means the growth was collectible; a liveAfter tracking
        -- liveBefore means a reachable structure. performMajorGC
        -- always completes, so this cannot wedge the loop.
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
  -- Early-out before any IO. A fresh sync has no replay window, and
  -- on a resume the window closes for good once the machine reaches
  -- 'NoReplay'. Without this the clock read and the atomic CAS run
  -- on every block of the whole sync. Only this thread writes the
  -- ref, so the plain read is exact.
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

-- | Trace any era transition 'observeBlockSTM' reports. A cached
-- interpreter skips the "falling back to node" warning, because
-- nothing uses the observed-summary path then.
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

-- | 'True' when the first processed block crosses an epoch boundary
-- against the pre-boot chain. This matches the ledger worker's
-- 'mkOnNewEpoch' enqueue condition: a fresh boot's genesis block
-- always crosses into epoch 0, and a resumed boot crosses only when
-- this block's epoch is one past the boot slot's. Any other case
-- must not drain: 'readBoundaryApplyResult' blocks until a payload
-- arrives, so an unmatched drain hangs the consumer.
bootBlockCrossesBoundary :: Maybe SlotNo -> EpochNo -> IngestM Bool
bootBlockCrossesBoundary mBootSlot blockEpoch = case mBootSlot of
  Nothing -> pure True
  Just bs -> do
    bootSd <- getSlotDetails bs
    pure (unEpochNo blockEpoch == 1 + unEpochNo (sdEpochNo bootSd))

-- | Drain the worker's next boundary 'ApplyResult' and run the
-- boundary extractors against it. Does nothing when the ledger
-- feature is off, or when no block arrived yet.
--
-- 'runGovernanceBoundary' runs only with the @governance@ extractor
-- enabled, because its writes target tables that extractor owns.
-- Otherwise the resolver's enacted-id snapshot keeps its default
-- @(Nothing, Nothing, Nothing)@ and 'runEpochBoundary' writes NULLs
-- into the three governance FK columns of @epoch_state@.
runBoundaryExtractor
  :: HasLedgerEnv
  -> IORef ExtractState
  -> IngestM ()
runBoundaryExtractor hasLedger extractStRef = case hasLedger of
  LedgerDisabled _   -> pure ()
  LedgerEnabled lenv -> do
    applyResult  <- liftIO $ readBoundaryApplyResult lenv
    mLastBlockId <- liftIO $ esLastBlockId <$> readIORef extractStRef
    governanceOn <- asks (prEnabled . exGovernance . scExtractors . getConfig)
    poolStatsOn  <- asks (prEnabled . exPoolStats  . scExtractors . getConfig)
    sdlOn        <- asks (prEnabled . exStakeDelegationLedger . scExtractors . getConfig)
    for_ mLastBlockId $ \lastBid -> do
      -- Governance runs first: it refreshes the enacted-state ids and
      -- apGovExpiresAfter on ExtractState, which EpochBoundary then
      -- reads to build the epoch_state row.
      when governanceOn $
        runGovernanceBoundary applyResult (BlockId lastBid)
      runEpochBoundary applyResult (BlockId lastBid)
      when poolStatsOn $
        runPoolStatsBoundary governanceOn applyResult (BlockId lastBid)
      when sdlOn $
        runStakeDelegationLedgerBoundary applyResult (BlockId lastBid)

-- ---------------------------------------------------------------------------
-- * Queue utilities
-- ---------------------------------------------------------------------------

-- | Drain up to @maxN@ blocks. It waits for the first one, then
-- takes every block already available without further waiting.
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

-- | 'True' when the last processed block reached the finalised-tip
-- boundary @nodeTip − k@. An unset ref gives 'False': either no
-- block arrived yet, or the receiver saw no tip at or above @k@.
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

-- | The panic message for a 'MsgRollback' during
-- 'IngestChainHistory'. Exposed so the test suite can pin the
-- message shape.
ingestRollbackPanicMessage :: Show point => point -> Text
ingestRollbackPanicMessage point =
  "IngestChainHistory: received MsgRollback at "
    <> show point
    <> "; this should be impossible (k-safety violation)."
