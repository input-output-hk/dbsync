{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- | Block consumer for 'IngestChainHistory'.
--
-- Reads 'ChainSyncMsg' values from the 'TBQueue' on the env. Forward
-- blocks are parsed into 'GenericBlock', then the enabled extractors
-- run and rows are written to PostgreSQL via the 'LoaderStream'. Epoch
-- boundaries are detected via 'sdEpochNo' comparison and trigger
-- commit + reopen cycles. Rollback markers are unreachable in this
-- phase (the receiver only enqueues rollbacks for blocks above the
-- @chain_tip − k@ boundary, and the consumer exits at the boundary);
-- if one slips through, panic.
--
-- == Per-epoch progress log
--
-- At each epoch boundary the consumer emits one summary line:
--
-- @
-- Epoch 265 | 21,427 blk in 41s (526 blk/s) | HEALTHY | [63.21%]
-- @
--
-- The bracketed percentage is the current block's position relative
-- to the rollback boundary (@nodeTip − k@) — how close
-- 'IngestChainHistory' is to its exit point. The segment is omitted
-- while the chain is still shorter than @k@ blocks. The @blk in X@
-- duration is end-to-end: it spans the previous boundary's
-- post-commit reset through this boundary's post-commit reset, so
-- the @blk/s@ rate reflects what the operator actually sees.
--
-- Detailed pipeline diagnostics (queue depths, drain-size
-- distribution, receiver writes-blocked counter, per-thread activity
-- notes) live on the watchdog at 'Debug' level rather than this
-- user-facing line.
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

import qualified Control.Concurrent.Class.MonadSTM.Strict as Strict
import Control.Concurrent.STM (TBQueue, TVar, readTBQueue, readTVarIO, tryReadTBQueue)
import Control.Tracer (traceWith)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Strict.Maybe as SMaybe
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import GHC.Stats (RTSStats (..), GCDetails (..), getRTSStats, getRTSStatsEnabled)
import System.Mem (performMajorGC)
import Text.Printf (printf)

import DbSync.AppM (IngestM, runAppM)
import DbSync.Block.Parser (parseBlock)
import DbSync.Block.Types (GenericBlock (..))
import DbSync.Checkpoint.Manager (mkBoundarySyncStateRow)
import DbSync.Checkpoint.SyncState (ControlConnection (..), writeSyncState)
import DbSync.Phase.Ingest.Counter (IdCounters)
import DbSync.Config.Types (LedgerConfig (..), SyncConfig (..))
import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.EpochSyncStats (EpochSyncStats (..))
import DbSync.Db.Schema.Ids (BlockId (..))
import DbSync.Env (CoreEnv (..), HasConfig (..), IngestEnv (..))
import DbSync.Extractor (ExtractState (..))
import DbSync.Extractor.EpochBoundary (runEpochBoundary)
import DbSync.Phase.Ingest.DedupMap (dedupMapSizes)
import DbSync.Phase.Ingest.PipelineStats (PipelineStats (..), emptyPipelineStats)
import DbSync.Trace.Replay
  ( ReplayAdvance (..)
  , ReplayLog (..)
  , ReplayLogState (..)
  , advanceReplay
  , renderReplayPercent
  )
import DbSync.Block.Pipeline (processBlock)
import DbSync.Phase.Type (SyncPhase (..), renderPhase)
import DbSync.Ledger.DepositAccumulator (drainCompletedEpochs, flushEpochParams)
import DbSync.Ledger.Types
  ( ApplyResult (..)
  , HasLedgerEnv (..)
  , LedgerEnv (..)
  )
import DbSync.Node.ChainSyncMsg (ChainSyncMsg (..))
import DbSync.Resolver (IdResolver (..))
import DbSync.Worker.TxOut.AddressBuffer (emptyEpochAddressBuffer, takeAndReset)
import qualified DbSync.Worker.TxOut.ConsumedByBuffer as ConsumedByBuffer
import DbSync.Worker.TxOut
  ( TxOutJob (..)
  , awaitTxOutDrained
  , enqueueTxOutJob
  , readAddressIdCounter
  )
import DbSync.StateQuery
  ( ObservationResult (..)
  , ObservedTransition (..)
  , SlotDetails (..)
  , getSlotDetails
  , isInterpreterCached
  , observeBlockSTM
  )
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Timing (fmtCount, fmtF2)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))
import DbSync.Trace.Watchdog (bumpConsumer, setConsumerNote)
import DbSync.Writer (Writer (..))

import Ouroboros.Consensus.Block (blockSlot)
import Ouroboros.Consensus.Shelley.HFEras ()                -- per-era HFC instances
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()  -- 'LedgerSupportsProtocol' orphans

-- ---------------------------------------------------------------------------
-- * Pipeline statistics (zero per-block overhead)
-- ---------------------------------------------------------------------------

-- | Baseline blocks/sec captured from the first fast epoch.
-- Used to detect slowdowns via the "Nx vs baseline" indicator.
newtype BaselineRef = BaselineRef
  { brBlocksPerSec :: Double  -- ^ Baseline throughput
  }

-- | Snapshot of the data a @sync_state@ row will eventually carry
-- for one finished epoch, held until the resolver has caught up to
-- that epoch. Pipelined-boundary semantics: a job for epoch @N@ is
-- enqueued at the boundary between @N@ and @N+1@, but @sync_state@
-- for @N@ only advances at the *next* boundary (@N+1@ → @N+2@), once
-- the worker has resolved every @tx_out.address_id@ FK for @N@. On a
-- clean exit at the rollback boundary the consumer drains the final
-- queued job and writes the snapshot held here.
data PendingBoundary = PendingBoundary
  { pbEpoch       :: !EpochNo
  , pbLastSlot    :: !Word64
  , pbLastBlockNo :: !Word64
  , pbLastHash    :: !ByteString
  , pbCounters    :: !IdCounters
  }

-- ---------------------------------------------------------------------------
-- * Running
-- ---------------------------------------------------------------------------

-- | Run the consumer loop in 'IngestM'.
--
-- Pulls everything it needs (tracer, queue, resolver, writer, copyWriter,
-- state-query handle, system start) from the 'IngestEnv'. The hot inner
-- loop runs in 'IngestM' itself rather than dropping back to raw 'IO' so
-- the env-aware 'processBlock' call can stay polymorphic.
--
-- Zero per-block overhead beyond the existing IORef bookkeeping: timing
-- still happens only at epoch boundaries via 'getCurrentTime', and drain
-- sizes are tracked with simple integer increments.
runConsumer :: IngestM ()
runConsumer = do
  prevEpochRef  <- liftIO $ newIORef (Nothing :: Maybe EpochNo)
  blockCountRef <- liftIO $ newIORef (0 :: Word64)
  epochStartRef <- liftIO $ getCurrentTime >>= newIORef
  -- Shared with the watchdog (see 'iePipelineStats' on 'IngestEnv').
  statsRef      <- asks iePipelineStats
  baselineRef   <- liftIO $ newIORef (Nothing :: Maybe BaselineRef)
  -- (slot, blockNo, hash) of the most recently processed block;
  -- the resume point captured by 'commitEpoch' at each boundary.
  lastBlockRef  <- liftIO $ newIORef (Nothing :: Maybe (Word64, Word64, ByteString))
  -- Snapshot of the previous epoch's boundary state, held until the
  -- resolver has resolved that epoch. See 'PendingBoundary'.
  pendingBoundaryRef <- liftIO $ newIORef (Nothing :: Maybe PendingBoundary)
  -- Replay-progress state machine. Seeded as 'ReplayPending' iff a
  -- replay boundary was supplied at boot; otherwise 'NoReplay'.
  bootSlot      <- asks ieLastCommittedSlotAtBoot
  replayRef     <- liftIO $ newIORef $ case bootSlot of
                     Just _  -> ReplayPending
                     Nothing -> NoReplay
  tracer        <- asks getTracer
  boundaryVar   <- asks ieRollbackBoundary
  loop tracer boundaryVar prevEpochRef blockCountRef epochStartRef statsRef baselineRef lastBlockRef pendingBoundaryRef replayRef
  where
    batchSize :: Int
    batchSize = 100

    loop
      :: AppTracer
      -> TVar (Maybe BlockNo)
      -> IORef (Maybe EpochNo)
      -> IORef Word64
      -> IORef UTCTime
      -> IORef PipelineStats
      -> IORef (Maybe BaselineRef)
      -> IORef (Maybe (Word64, Word64, ByteString))
      -> IORef (Maybe PendingBoundary)
      -> IORef ReplayLogState
      -> IngestM ()
    loop tracer boundaryVar prevEpochRef blockCountRef epochStartRef statsRef baselineRef lastBlockRef pendingBoundaryRef replayRef = do
      queue <- asks ieBlockQueue

      -- 1. Drain a batch of blocks (no timing — just count)
      blocks <- liftIO $ drainTBQueue queue batchSize
      let !drainSize = length blocks

      -- Update drain stats (integer ops only, no syscalls)
      liftIO $ modifyIORef' statsRef $ \ps -> ps
        { psDrainTotal   = psDrainTotal ps + fromIntegral drainSize
        , psDrainCount   = psDrainCount ps + 1
        , psDrainMax     = max (psDrainMax ps) drainSize
        , psSingleDrains = psSingleDrains ps + if drainSize == 1 then 1 else 0
        , psFullDrains   = psFullDrains ps + if drainSize >= batchSize then 1 else 0
        }

      -- 2. Process batch (releases each forward block after parsing)
      processBatch prevEpochRef blockCountRef epochStartRef statsRef baselineRef lastBlockRef pendingBoundaryRef replayRef blocks

      -- 3. Exit cleanly when the last processed block has reached
      --    the rollback boundary; the caller then runs Prep and
      --    transitions to FollowingChainTip.
      reached <- liftIO $ rollbackBoundaryReached lastBlockRef boundaryVar
      if reached
        then do
          finalFlushSyncState pendingBoundaryRef
          mLast <- liftIO $ readIORef lastBlockRef
          liftIO $ traceWith tracer $ LogMsg Info "Ingest"
            ( "reached rollback boundary at "
                <> renderLastBlock mLast
                <> "; exiting consumer loop"
            ) Nothing
        else
          loop tracer boundaryVar prevEpochRef blockCountRef epochStartRef statsRef baselineRef lastBlockRef pendingBoundaryRef replayRef

    renderLastBlock :: Maybe (Word64, Word64, ByteString) -> Text
    renderLastBlock = \case
      Nothing                -> "(no block processed yet)"
      Just (slot, blk, _hash) ->
        "block " <> show blk <> " (slot " <> show slot <> ")"

    -- Drain the final queued resolve job and advance @sync_state@ to
    -- the last fully-completed epoch. Pipelined-boundary semantics:
    -- during normal operation @sync_state@ lags by one epoch behind
    -- the consumer; at the rollback boundary the consumer exits
    -- mid-epoch so the previous epoch becomes the last commit point.
    -- A no-op when the pipeline never crossed a boundary
    -- ('pendingBoundaryRef' = 'Nothing').
    finalFlushSyncState :: IORef (Maybe PendingBoundary) -> IngestM ()
    finalFlushSyncState pendingBoundaryRef = do
      txOutWorker     <- asks ieTxOutWorker
      mConsumedByBuf  <- asks ieConsumedByBuffer
      loaderStream    <- asks ieLoaderStream
      watchdog        <- asks ieWatchdog
      cfg             <- asks getConfig
      let ledgerEnabledCfg = lcEnabled (scLedger cfg)
          schemaVersion    = 1 :: Int
      -- Drain any residual mid-epoch consumed-by pairs by enqueueing
      -- one last job with an empty address buffer; the worker pairs
      -- them with the (empty) address work and applies the UPDATE
      -- before exiting. When consumed-by is off this is unreachable.
      mResidualCb <- liftIO $ case mConsumedByBuf of
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
      mPending <- liftIO $ readIORef pendingBoundaryRef
      for_ mPending $ \pb ->
        writeSyncState $
          mkBoundarySyncStateRow
            (pbLastSlot pb) (pbLastBlockNo pb) (pbLastHash pb)
            (pbCounters pb) addressIdCounter
            schemaVersion ledgerEnabledCfg
      -- Commit the in-progress epoch's loader-stream rows so Prep
      -- and Follow don't have to replay them from the node.
      liftIO $ setConsumerNote watchdog "consumer: final lsCommit"
      liftIO $ lsCommit loaderStream

    processBatch
      :: IORef (Maybe EpochNo)
      -> IORef Word64
      -> IORef UTCTime
      -> IORef PipelineStats
      -> IORef (Maybe BaselineRef)
      -> IORef (Maybe (Word64, Word64, ByteString))
      -> IORef (Maybe PendingBoundary)
      -> IORef ReplayLogState
      -> [ChainSyncMsg]
      -> IngestM ()
    processBatch _ _ _ _ _ _ _ _ [] = pure ()
    processBatch _ _ _ _ _ _ _ _ (MsgRollback point : _) =
      -- Reaching this branch would mean the node sent a rollback for
      -- a block below the @chain_tip − k@ boundary, violating
      -- k-safety. Crash loudly so the operator can investigate.
      panic (ingestRollbackPanicMessage point)
    processBatch prevEpochRef blockCountRef epochStartRef statsRef baselineRef lastBlockRef pendingBoundaryRef replayRef (MsgForward cardanoBlock : rest) = do
      tracer        <- asks getTracer
      sqv           <- asks ieStateQueryVar
      resolver      <- asks ieResolver
      writer        <- asks ieWriter
      loaderStream  <- asks ieLoaderStream
      hasLedger     <- asks ieHasLedgerEnv
      extractStRef  <- asks ieExtractState
      dedupMaps     <- asks ieDedupMaps
      addressBuffer <- asks ieAddressBuffer
      txOutWorker    <- asks ieTxOutWorker
      mConsumedByBuf <- asks ieConsumedByBuffer
      ctrlConn       <- asks ieControlConnection
      bootSlot      <- asks ieLastCommittedSlotAtBoot
      replayStart   <- asks ieReplayStartSlot
      watchdog      <- asks ieWatchdog
      boundaryVar   <- asks ieRollbackBoundary
      securityParam <- asks (ceSecurityParam . ieCore)
      cfg           <- asks getConfig
      let ledgerEnabledCfg = lcEnabled (scLedger cfg)
          schemaVersion    = 1 :: Int
          slot             = blockSlot cardanoBlock
          isReplay         = case bootSlot of
            Just bs -> slot <= bs
            Nothing -> False

      -- Advance the replay-log state machine before the 'unless
      -- isReplay' branch so 'ReplayLogComplete' fires on the first
      -- /non/-replay block, just before normal processing resumes.
      nowForReplay <- liftIO getCurrentTime
      logEvent <- liftIO $ atomicModifyIORef' replayRef $ \prev ->
        let advance = advanceReplay slot bootSlot nowForReplay prev
        in (raNewState advance, raLog advance)
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

      -- Replayed blocks are already in PG; skip processBlock.
      unless isReplay $ do
        -- Update the observed summary before 'getSlotDetails' so
        -- any era-boundary transition is in scope when the slot
        -- details are computed.
        obsResult <- liftIO $ atomically $ observeBlockSTM sqv cardanoBlock
        case obsResult of
          NewTransition t ->
            liftIO $ traceWith tracer $ LogMsg Info "StateQuery"
              ( "Observed era transition "
                  <> show (otFromEra t) <> " → " <> show (otToEra t)
                  <> " at slot " <> show (unSlotNo (otAtSlot t))
                  <> " (epoch " <> show (unEpochNo (otAtEpoch t)) <> ")"
              ) Nothing
          ObservationBroken fromEra toEra -> do
            -- Suppress the misleading "falling back to node" warning
            -- when 'sqvInterpreterVar' is already seeded — the
            -- observed-summary path isn't actually used in that case.
            cached <- isInterpreterCached
            unless cached $
              liftIO $ traceWith tracer $ LogMsg Warning "StateQuery"
                ( "Observed era jump too large ("
                    <> show fromEra <> " → " <> show toEra
                    <> "); falling back to node interpreter"
                ) Nothing
          Unchanged -> pure ()

        sd <- getSlotDetails slot
        let !genBlock = parseBlock sd cardanoBlock
            !blockEpoch = sdEpochNo sd

        -- Epoch boundary check
        prevEpoch <- liftIO $ readIORef prevEpochRef
        case prevEpoch of
          Just prev | prev /= blockEpoch -> do
            epochStart <- liftIO $ readIORef epochStartRef
            blockCount <- liftIO $ readIORef blockCountRef

            -- Snapshot the last fully-extracted block + ID counters
            -- of the just-finished epoch. Persisted to
            -- 'pendingBoundaryRef' at the end of this block so that
            -- the *next* boundary writes @sync_state@ for it once the
            -- resolver has caught up.
            mLastBlock      <- liftIO $ readIORef lastBlockRef
            extractState    <- liftIO $ readIORef extractStRef
            let counters    = esIdCounters extractState

            -- Pipelined epoch boundary: flush COPY for the
            -- just-finished epoch, advance @sync_state@ for the
            -- *previously* queued epoch (the resolver is guaranteed
            -- idle here), enqueue the just-finished epoch, reopen
            -- streams. The worker then resolves the just-finished
            -- epoch in parallel with the consumer's ingest of the
            -- next one — see the @PendingBoundary@ Haddock for the
            -- crash-safety reasoning.
            (buf, mConsumedBuf) <- liftIO $ do
              -- 1. Flush loader streams — tx_outs durable, address_id = NULL.
              setConsumerNote watchdog "consumer: lsCommit (flushing loader stream)"
              lsCommit loaderStream

              -- 2. Snapshot the just-finished epoch's address-resolution
              --    buffer; it will be handed to the worker once the
              --    previous boundary's job is committed below.
              setConsumerNote watchdog "consumer: takeAndReset addressBuffer"
              b <- takeAndReset addressBuffer

              -- Snapshot the consumed-by buffer too when the feature is on.
              cb <- case mConsumedByBuf of
                Just ref -> Just <$> ConsumedByBuffer.takeAndReset ref
                Nothing  -> pure Nothing

              -- 3. Wait for the worker to finish the job queued at the
              --    *previous* boundary (epoch N-1). On the first boundary
              --    the queue is empty and this returns immediately.
              setConsumerNote watchdog "consumer: awaitTxOutDrained (epoch N-1)"
              awaitTxOutDrained txOutWorker

              -- 4. Flush the ledger worker's per-epoch protocol-param
              --    deposit data for the just-finished epoch.
              --    'epoch_param_pending' INSERTs are idempotent
              --    (@ON CONFLICT DO NOTHING@), so flushing eagerly here
              --    is safe even though @sync_state@ will only advance
              --    to the previous epoch a step later.
              setConsumerNote watchdog "consumer: flushEpochParams"
              flushPendingDeposits hasLedger prev slot ctrlConn
              pure (b, cb)

            -- 5. Advance @sync_state@ for the previously snapshotted
            --    epoch — its tx_out / collateral_tx_out FKs are now
            --    fully resolved. The address counter is read from
            --    the resolver (its sole allocator) right after the
            --    drain so it reflects exactly the rows it inserted
            --    for that epoch.
            liftIO $ setConsumerNote watchdog "consumer: writeSyncState (lagging)"
            addressIdCounter <- liftIO $ readAddressIdCounter txOutWorker
            mPending <- liftIO $ readIORef pendingBoundaryRef
            for_ mPending $ \pb ->
              writeSyncState $
                mkBoundarySyncStateRow
                  (pbLastSlot pb) (pbLastBlockNo pb) (pbLastHash pb)
                  (pbCounters pb) addressIdCounter
                  schemaVersion ledgerEnabledCfg

            -- 6. Queue the just-finished epoch's resolve job. The
            --    worker now runs in parallel with the consumer's
            --    ingest of the next epoch. 'enqueueTxOutJob' blocks
            --    if the worker queue is at its bound, back-pressuring
            --    the main pipeline.
            liftIO $ setConsumerNote watchdog "consumer: enqueueTxOutJob"
            liftIO $ enqueueTxOutJob txOutWorker
              (TxOutJob prev buf mConsumedBuf)

            -- 7. Save the snapshot that the *next* boundary will use
            --    to advance @sync_state@ once this epoch's job is
            --    resolved. 'mLastBlock' should never be 'Nothing'
            --    here — the boundary detection requires at least one
            --    processed block — but skip cleanly just in case.
            liftIO $ for_ mLastBlock $ \(lastSlot, lastBlockNo, lastHash) ->
              writeIORef pendingBoundaryRef $ Just PendingBoundary
                { pbEpoch       = prev
                , pbLastSlot    = lastSlot
                , pbLastBlockNo = lastBlockNo
                , pbLastHash    = lastHash
                , pbCounters    = counters
                }

            -- 8. Reopen loader streams for the next epoch.
            liftIO $ do
              setConsumerNote watchdog "consumer: lsReopen"
              lsReopen loaderStream
              setConsumerNote watchdog "consumer: post-commit"

            -- End-to-end timing: spans the previous boundary's
            -- post-commit reset through this boundary's post-commit
            -- reset, so the @blk/s@ rate reflects what the operator
            -- actually sees rather than the ingestion-only window.
            epochEnd <- liftIO getCurrentTime
            let elapsed = diffUTCTime epochEnd epochStart
                blocksPerSec :: Double
                blocksPerSec
                  | elapsed > 0 = fromIntegral blockCount / realToFrac elapsed
                  | otherwise   = 0
                elapsedSec :: Double
                elapsedSec = realToFrac elapsed

            -- 9. Record per-epoch sync stats. Writing after 'lsReopen'
            --    queues the row into the *next* epoch's stream, so it
            --    durabilises at the next boundary. Acceptable for a
            --    stats-only table with no FK dependants — losing a
            --    single row on crash is preferable to recording a
            --    different elapsed time than the user-facing log.
            essId <- liftIO $ assignEpochSyncStatsId resolver
            let ess = EpochSyncStats
                  { epochSyncStatsEpochNo        = unEpochNo prev
                  , epochSyncStatsBlocksProcessed = blockCount
                  , epochSyncStatsBlocksPerSec    = blocksPerSec
                  , epochSyncStatsElapsedSec      = elapsedSec
                  , epochSyncStatsSyncedAt        = epochEnd
                  , epochSyncStatsPhase           = renderPhase IngestChainHistory
                  }
            liftIO $ writeEpochSyncStats writer essId ess

            -- Major GC on heavy epochs only. Gated at >10s to avoid
            -- penalising fast Byron epochs (2-3s each).
            when (elapsedSec > 10.0) $ liftIO performMajorGC

            ps       <- liftIO $ readIORef statsRef
            baseline <- liftIO $ readIORef baselineRef

            -- Progress percentage against the current node tip
            -- (derived from the published rollback boundary + k).
            -- Omitted while the chain is still shorter than k.
            mBoundary <- liftIO $ readTVarIO boundaryVar
            let pctSeg = renderBoundaryPercent mBoundary securityParam
                          (fmap (\(_, b, _) -> b) mLastBlock)
                status = diagnose batchSize blocksPerSec ps baseline

            when (isNothing baseline && blocksPerSec > 500) $
              liftIO $ writeIORef baselineRef (Just (BaselineRef blocksPerSec))

            liftIO $ traceWith tracer $ LogMsg Info "Ingest"
              ( "Epoch " <> show (unEpochNo prev)
                <> " | " <> fmtCount blockCount <> " blk in " <> fmtDuration elapsedSec
                <> " (" <> show (round blocksPerSec :: Int) <> " blk/s)"
                <> " | " <> status
                <> pctSeg
              ) Nothing

            -- Dedup-map size + heap-usage trace: diagnostic only.
            -- 'dedupMapSizes' iterates every hash table and
            -- 'sampleHeapBytes' calls 'getRTSStats', so the whole
            -- block is gated on 'Debug' to keep production runs free
            -- of the overhead.
            minSev <- asks (ceMinSeverity . ieCore)
            when (minSev <= Debug) $ do
              dedupCounts <- liftIO $ dedupMapSizes dedupMaps
              heapInfo    <- liftIO sampleHeapBytes
              let heapText = case heapInfo of
                    Just live -> " | heap=" <> fmtBytes live
                    Nothing   -> ""
              liftIO $ traceWith tracer $ LogMsg Debug "Dedup"
                ( "Epoch " <> show (unEpochNo prev)
                  <> " | " <> renderDedupCounts dedupCounts
                  <> heapText
                ) Nothing

            -- Reset for next epoch
            liftIO $ writeIORef statsRef emptyPipelineStats
            liftIO $ writeIORef blockCountRef 0
            liftIO $ writeIORef epochStartRef epochEnd
          _ -> pure ()

        -- Run extractors + write to COPY queues
        liftIO $ setConsumerNote watchdog "consumer: processBlock"
        processBlock genBlock

        -- Boundary-block extractor (epoch-table writes that depend
        -- on the ledger worker's apNewEpoch).
        case prevEpoch of
          Just prev | prev /= blockEpoch ->
            case hasLedger of
              LedgerEnabled lenv -> do
                liftIO $ setConsumerNote watchdog "consumer: waitForApplyResultAt (boundary)"
                applyResult <- liftIO $ waitForApplyResultAt lenv slot
                mLastBlockId <- liftIO $ esLastBlockId <$> readIORef extractStRef
                case mLastBlockId of
                  Just lastBid -> do
                    liftIO $ setConsumerNote watchdog "consumer: runEpochBoundary"
                    runEpochBoundary applyResult (BlockId lastBid)
                  Nothing -> pure ()
              LedgerDisabled _ ->
                pure ()
          _ -> pure ()

        -- Update counters
        liftIO $ modifyIORef' blockCountRef (+ 1)
        liftIO $ writeIORef prevEpochRef (Just blockEpoch)
        -- Record this block's identity for the next boundary commit.
        liftIO $ writeIORef lastBlockRef $ Just
          ( unSlotNo (blkSlotNo genBlock)
          , unBlockNo (blkBlockNo genBlock)
          , blkHash genBlock
          )

      -- Watchdog bump: per iteration, replay or not, so the
      -- watchdog still sees forward progress during the replay
      -- window (where 'processBlock' is skipped).
      liftIO $ bumpConsumer watchdog slot

      -- Recurse, whether the block was processed or skipped.
      processBatch prevEpochRef blockCountRef epochStartRef statsRef baselineRef lastBlockRef pendingBoundaryRef replayRef rest

-- ---------------------------------------------------------------------------
-- * Diagnosis
-- ---------------------------------------------------------------------------

-- | Compute average drain size as an integer.
avgDrain :: PipelineStats -> Int
avgDrain ps
  | psDrainCount ps == 0 = 0
  | otherwise = fromIntegral (psDrainTotal ps) `div` fromIntegral (psDrainCount ps)

-- | Throughput-aware diagnosis. Returns a short status string.
--
-- Check order matters — drain level is checked before wait/throughput:
--
--   1. High throughput (>1000 blk/s) → 'HEALTHY' regardless
--   2. Low drain (<5) → 'NODE STARVED' (queue empty, waiting for node)
--   3. High drain + slowing vs baseline → 'SLOWING (Nx vs eY)'
--   4. Medium drain → 'BALANCED'
--   5. High drain, steady → 'SATURATED'
diagnose
  :: Int              -- ^ batchSize
  -> Double           -- ^ blocks/sec this epoch
  -> PipelineStats    -- ^ drain stats this epoch
  -> Maybe BaselineRef
  -> Text
diagnose batchSz bps ps mBaseline
  -- Fast — no concern
  | bps > 1000 = "HEALTHY"

  -- Queue nearly empty — node can't keep up
  | avg < 5 = "NODE STARVED"

  -- Queue full + throughput declining vs baseline
  | avg > highDrain
  , Just bl <- mBaseline
  , brBlocksPerSec bl > 0
  , bps < brBlocksPerSec bl * 0.5 = "SLOWING"

  -- Queue partially full — balanced
  | avg >= 5 && avg <= highDrain = "BALANCED"

  -- Queue consistently full — pipeline at capacity
  | otherwise = "SATURATED"
  where
    avg = avgDrain ps
    highDrain = (batchSz * 4) `div` 5  -- 80% of batchSize

-- ---------------------------------------------------------------------------
-- * LedgerWorker coordination
-- ---------------------------------------------------------------------------

-- | Block until the 'LedgerWorker' has produced an 'ApplyResult' whose
-- slot is at-or-past @targetSlot@, then return it.
--
-- Used at epoch boundaries to fetch the @apNewEpoch@ payload from
-- 'leLatestApplyResult'. STM 'retry' suspends the consumer thread
-- until the worker writes a fresh 'ApplyResult'.
--
-- The worker writes 'leLatestApplyResult' on every successful
-- 'applyBlock' (DbSync.Ledger.State), so the wait progresses
-- deterministically — no polling, no sleep loops.
waitForApplyResultAt :: LedgerEnv -> SlotNo -> IO ApplyResult
waitForApplyResultAt lenv targetSlot = Strict.atomically $ do
  mAR <- Strict.readTVar (leLatestApplyResult lenv)
  case mAR of
    SMaybe.Just ar
      | sdSlotNo (apSlotDetails ar) >= targetSlot -> pure ar
    _ -> retry

-- | Wait for the worker to catch up to the boundary block, then
-- drain every accumulated deposit-param entry at or before the
-- just-completed epoch and flush them to @epoch_param_pending@.
-- A no-op when the ledger feature is disabled.
flushPendingDeposits
  :: HasLedgerEnv
  -> EpochNo            -- ^ just-completed epoch (drain watermark)
  -> SlotNo             -- ^ boundary block slot (worker catch-up target)
  -> ControlConnection
  -> IO ()
flushPendingDeposits hasLedger prev slot ctrl = case hasLedger of
  LedgerDisabled _   -> pure ()
  LedgerEnabled lenv -> do
    _ <- waitForApplyResultAt lenv slot
    completed <- drainCompletedEpochs (leDepositAccumulator lenv) prev
    runAppM ctrl (flushEpochParams completed)

-- ---------------------------------------------------------------------------
-- * Queue utilities
-- ---------------------------------------------------------------------------

-- | Drain up to @maxN@ blocks from the queue.
-- Blocks until at least one is available, then takes as many as
-- are immediately available (up to @maxN@) without waiting.
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
-- 'MsgRollback'. Should be unreachable in practice — the receiver
-- only enqueues rollback markers for blocks above the @chain_tip − k@
-- boundary, and the consumer exits before draining anything above
-- it. Exposed as a pure helper so the test suite can pin the
-- message shape.
ingestRollbackPanicMessage :: Show point => point -> Text
ingestRollbackPanicMessage point =
  "IngestChainHistory: received MsgRollback at "
    <> show point
    <> "; this should be impossible (k-safety violation)."


-- | Render the Ingest progress segment of the form @\" | (~87.32%)\"@.
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

-- | Format a @(name, count)@ list as @"name=N1,234 …"@ for log lines.
renderDedupCounts :: [(Text, Int)] -> Text
renderDedupCounts = Text.intercalate " " . map one
  where
    one (n, c) = n <> "=" <> fmtCount c

-- | Live data after the most recent GC. Sampled at epoch boundaries
-- so the value reflects the heap working set at the end of that
-- epoch, not the cumulative lifetime peak. Requires @+RTS -T -RTS@;
-- returns 'Nothing' otherwise.
sampleHeapBytes :: IO (Maybe Word64)
sampleHeapBytes = do
  enabled <- getRTSStatsEnabled
  if enabled
    then do
      Just . gcdetails_live_bytes . gc <$> getRTSStats
    else pure Nothing

-- | Render a byte count as a short human-readable string, e.g.
-- @123MB@, @1.4GB@.
fmtBytes :: Word64 -> Text
fmtBytes b
  | b >= gib = Text.pack (printf "%.1fGB" (fromIntegral b / fromIntegral gib :: Double))
  | b >= mib = Text.pack (printf "%dMB"   (b `div` mib))
  | b >= kib = Text.pack (printf "%dKB"   (b `div` kib))
  | otherwise = show b <> "B"
  where
    kib, mib, gib :: Word64
    kib = 1024
    mib = 1024 * 1024
    gib = 1024 * 1024 * 1024

-- | Format seconds as human-readable duration.
fmtDuration :: Double -> Text
fmtDuration secs
  | secs < 60 = show (round secs :: Int) <> "s"
  | secs < 3600 =
      let t = round secs :: Int
      in show (t `div` 60) <> "m " <> show (t `mod` 60) <> "s"
  | otherwise =
      let t = round secs :: Int
      in show (t `div` 3600) <> "h " <> show ((t `mod` 3600) `div` 60) <> "m"
