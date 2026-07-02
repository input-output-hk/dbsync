{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Epoch-boundary handling for the Ingest consumer.
--
-- The consumer's per-block loop in "DbSync.Phase.Ingest.Consumer"
-- detects an epoch transition by comparing 'sdEpochNo' against the
-- previous block's. When it crosses, control passes here.
--
-- 'handleEpochBoundary' runs a pipelined cascade — flush COPY,
-- snapshot per-epoch buffers, await the tx-out worker, advance
-- @sync_state@ for the /previous/ pending epoch, enqueue the
-- just-finished one, reopen the loader stream, persist the LSM
-- tables, and emit the per-epoch summary line. The pipelining means
-- @sync_state@ always lags by one epoch behind the consumer.
module DbSync.Phase.Ingest.Boundary
  ( -- * Loop state
    ConsumerLoopState (..)
  , newConsumerLoopState
  , PendingBoundary (..)

    -- * Boundary handler
  , handleEpochBoundary

    -- * LedgerWorker coordination
  , waitForApplyResultAt
  , readBoundaryApplyResult
  , flushPendingDeposits

    -- * Helpers
  , compactIngestStores
  , renderEpochSummary
  , renderUtxoHitRate
  , renderDedupCounts
  , renderMemCurve
  , renderBufferSizes
  , recordMemSample
  , sampleHeapBytes
  , fmtBytes
  , fmtDuration
  ) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import qualified Control.Concurrent.Class.MonadSTM.Strict as Strict
import Control.Concurrent.STM (readTVarIO)
import Control.Concurrent.STM.TBQueue (readTBQueue)
import Control.Tracer (traceWith)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Strict.Maybe as SMaybe
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import GHC.Stats (RTSStats (..), GCDetails (..), getRTSStats, getRTSStatsEnabled)
import System.Mem (performMajorGC)
import qualified System.Timeout as Timeout
import Text.Printf (printf)

import DbSync.AppM (IngestM, runAppM)
import DbSync.App.Config.Types (LedgerConfig (..), SyncConfig (..))
import DbSync.App.Env (CoreEnv (..), HasConfig (..), IngestEnv (..))
import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.EpochSyncStats (EpochSyncStats (..))
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounters)
import DbSync.Phase.Ingest.DedupStore (DedupStores, dedupStoreSizes)
import qualified DbSync.Phase.Ingest.DedupStore as DedupStore
import DbSync.Phase.Ingest.LsmSession (LsmSession)
import qualified DbSync.Phase.Ingest.UtxoStore as UtxoStore
import DbSync.Phase.Type (SyncPhase (..), renderPhase)
import DbSync.SyncState.Manager (mkBoundarySyncStateRow)
import DbSync.SyncState.Row (ControlConnection, writeSyncState)
import DbSync.Schema.Version (currentSchemaVersion)
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Replay (ReplayLogState (..))
import DbSync.Trace.Timing (fmtCount, fmtF2)
import DbSync.Trace.Types (LogMsg (..), Severity (..))
import DbSync.Worker.Ledger.DepositAccumulator (drainCompletedEpochs, flushEpochParams)
import DbSync.Worker.Ledger.Types
  ( ApplyResult (..)
  , BoundaryApplyData (..)
  , HasLedgerEnv (..)
  , LedgerEnv (..)
  )
import DbSync.Worker.TxOut.AddressBuffer
  ( addressBufferCounts
  , forceEpochAddressBuffer
  , takeAndReset
  )
import qualified DbSync.Worker.TxOut.ConsumedByBuffer as ConsumedByBuffer
import DbSync.Worker.TxOut.Worker
  ( TxOutJob (..)
  , awaitTxOutDrained
  , enqueueTxOutJob
  , readAddressIdCounter
  )
import DbSync.Resolver (IdResolver (..))
import DbSync.Writer (Writer (..))
import DbSync.StateQuery (sdSlotNo)

-- ---------------------------------------------------------------------------
-- * Loop state
-- ---------------------------------------------------------------------------

-- | Snapshot of one finished epoch, held until the resolver has
-- caught up. A job for epoch @N@ is enqueued at the @N → N+1@
-- boundary; @sync_state@ for @N@ only advances at the @N+1 → N+2@
-- boundary, once the tx-out worker has resolved every
-- @tx_out.address_id@ FK for @N@. On a clean exit at the rollback
-- boundary the consumer drains the final queued job and writes the
-- snapshot held here.
data PendingBoundary = PendingBoundary
  { pbEpoch       :: !EpochNo
  , pbLastSlot    :: !Word64
  , pbLastBlockNo :: !Word64
  , pbLastHash    :: !ByteString
  , pbCounters    :: !IdCounters
  }

-- | Per-epoch memory samples plus running peaks. The @(in-use, live)@
-- samples (newest first) drive the curve; the peaks are tracked
-- separately so they reflect every sample, not just the curve points.
data MemStats = MemStats
  { msSamples   :: ![(Word64, Word64)]
  , msPeakInUse :: !Word64
  , msPeakLive  :: !Word64
  }

emptyMemStats :: MemStats
emptyMemStats = MemStats [] 0 0

-- | Mutable state threaded through the consumer loop. Lives for one
-- 'runConsumer' invocation.
data ConsumerLoopState = ConsumerLoopState
  { clsPrevEpoch       :: !(IORef (Maybe EpochNo))
    -- ^ Epoch of the most recently processed block; 'Nothing' until
    -- the first block lands. Boundary detection compares against
    -- this.
  , clsBlockCount      :: !(IORef Word64)
    -- ^ Blocks processed in the current epoch; reset at each
    -- boundary. Feeds the per-epoch @blk/s@ rate.
  , clsEpochStart      :: !(IORef UTCTime)
    -- ^ Wall-clock start of the current epoch's window.
  , clsLastBlock       :: !(IORef (Maybe (Word64, Word64, ByteString)))
    -- ^ @(slot, blockNo, hash)@ of the most recently processed
    -- block; the resume point captured at each boundary.
  , clsPendingBoundary :: !(IORef (Maybe PendingBoundary))
    -- ^ Snapshot of the previous epoch's boundary state, held until
    -- the resolver catches up.
  , clsReplay          :: !(IORef ReplayLogState)
    -- ^ Replay-progress state machine; 'ReplayPending' on a resume
    -- with a replay window, 'NoReplay' otherwise.
  , clsMemStats        :: !(IORef MemStats)
    -- ^ Per-epoch @(in-use, live)@ samples + running peaks, taken on the
    -- consumer's sample stride. Rendered as a curve+peak readout and
    -- reset at each boundary. Empty until first sampled (or without
    -- @+RTS -T@).
  }

-- | Allocate a fresh 'ConsumerLoopState'. The replay machine is
-- seeded based on whether the boot had a replay window.
newConsumerLoopState :: Maybe SlotNo -> IO ConsumerLoopState
newConsumerLoopState bootSlot = do
  now <- getCurrentTime
  ConsumerLoopState
    <$> newIORef Nothing
    <*> newIORef 0
    <*> newIORef now
    <*> newIORef Nothing
    <*> newIORef Nothing
    <*> newIORef (case bootSlot of
                    Just _  -> ReplayPending
                    Nothing -> NoReplay)
    <*> newIORef emptyMemStats

-- ---------------------------------------------------------------------------
-- * Boundary handler
-- ---------------------------------------------------------------------------

-- | Run the 9-step boundary cascade for the @prev → blockEpoch@
-- transition. Called from the consumer when @prev /= blockEpoch@.
--
-- The steps, in order:
--
--   1. Commit the loader stream — tx_outs durable, address_id NULL.
--   2. Snapshot the address buffer (+ optional consumed-by buffer).
--   3. Await the tx-out worker draining the /previous/ boundary's job.
--   4. Flush the ledger worker's pending epoch_param deposits.
--   5. Advance @sync_state@ for the lagging epoch (now fully resolved).
--   6. Enqueue the just-finished epoch's resolve job to the worker.
--   7. Stash the snapshot for the /next/ boundary's @sync_state@ write.
--   8. Reopen the loader stream for the next epoch.
--   9. Persist the LSM tables (snapshot refresh; full
--      reopen-compaction every 'ingestCompactEveryEpochs').
--
-- Followed by the per-epoch summary log, optional dedup-debug log,
-- and a major GC on epochs above 10s wall-clock.
handleEpochBoundary
  :: ConsumerLoopState
  -> EpochNo        -- ^ epoch just completed
  -> SlotNo         -- ^ boundary block's slot
  -> IngestM ()
handleEpochBoundary cls prev slot = do
  ie       <- ask
  let tracer        = getTracer ie
      txOutWorker   = ieTxOutWorker ie
      mConsumedBuf  = ieConsumedByBuffer ie
      loaderStream  = ieLoaderStream ie
      ctrlConn      = ieControlConnection ie
      hasLedger     = ieHasLedgerEnv ie
      addressBuffer = ieAddressBuffer ie
      dedupStores   = ieDedupStores ie
      lsm           = ieLsmSession ie
      utxoStore     = ieUtxoStore ie
      resolver      = ieResolver ie
      writer        = ieWriter ie
      coreEnv       = ieCore ie
      boundaryVar   = ieRollbackBoundary ie
      securityParam = ceSecurityParam coreEnv
      minSev        = ceMinSeverity coreEnv
      ledgerEnabled = lcEnabled (scLedger (getConfig ie))
      schemaVersion = currentSchemaVersion

  epochStart <- liftIO $ readIORef (clsEpochStart cls)
  blockCount <- liftIO $ readIORef (clsBlockCount cls)
  mLastBlock <- liftIO $ readIORef (clsLastBlock cls)
  counters   <- liftIO $ esIdCounters <$> readIORef (ieExtractState ie)

  -- Fold in a near-boundary sample so even a short epoch (fewer than
  -- the consumer's sample cadence) still contributes to the curve.
  liftIO $ recordMemSample (clsMemStats cls)

  -- Steps 1–4: drain the existing pipeline so the resolver can run
  -- in parallel with the next epoch's ingest.
  (buf, mConsumedSnap) <- liftIO $ do
    lsCommit loaderStream
    b <- takeAndReset addressBuffer
    cb <- case mConsumedBuf of
      Just ref -> Just <$> ConsumedByBuffer.takeAndReset ref
      Nothing  -> pure Nothing
    awaitTxOutDrained txOutWorker
    flushPendingDeposits hasLedger prev slot ctrlConn
    pure (b, cb)

  -- Boundary probe (Debug-gated): brackets the buffer force with
  -- per-step logs and a timeout, so a wedged force surfaces as a log
  -- line instead of a hung pipeline. No performMajorGC here (it
  -- cannot be interrupted).
  when (minSev <= Debug) $ liftIO $ do
    let tag m = traceWith tracer $ LogMsg Debug "TxOutProbe"
          ("epoch " <> show (unEpochNo prev) <> " " <> m) Nothing
    l0 <- sampleHeapBytes
    tag ("preForce live=" <> maybe "n/a" fmtBytes l0)
    r <- Timeout.timeout 60000000 $ do
      _ <- evaluate (forceEpochAddressBuffer buf)
      for_ mConsumedSnap (evaluate . ConsumedByBuffer.forceEpochConsumedByBuffer)
      pure ()
    tag (maybe "force TIMEOUT(60s)" (const "force done") r)

  -- Step 5: advance @sync_state@ for the previously snapshotted
  -- epoch. The address counter reflects rows just drained in step 3.
  addressIdCounter <- liftIO $ readAddressIdCounter txOutWorker
  mPending         <- liftIO $ readIORef (clsPendingBoundary cls)
  for_ mPending $ \pb ->
    writeSyncState $
      mkBoundarySyncStateRow
        (pbLastSlot pb) (pbLastBlockNo pb) (pbLastHash pb)
        (pbCounters pb) addressIdCounter
        schemaVersion ledgerEnabled

  -- Step 6: queue the just-finished epoch for the worker.
  liftIO $ enqueueTxOutJob txOutWorker (TxOutJob prev buf mConsumedSnap)

  -- Step 7: stash the snapshot for the next boundary.
  liftIO $ for_ mLastBlock $ \(lastSlot, lastBlockNo, lastHash) ->
    writeIORef (clsPendingBoundary cls) $ Just PendingBoundary
      { pbEpoch       = prev
      , pbLastSlot    = lastSlot
      , pbLastBlockNo = lastBlockNo
      , pbLastHash    = lastHash
      , pbCounters    = counters
      }

  -- Step 8: reopen loader streams for the next epoch.
  liftIO $ lsReopen loaderStream

  -- Step 9: persist the LSM-backed Ingest tables (snapshot refresh
  -- for restart-resume); every 'ingestCompactEveryEpochs' the cycle
  -- also reopens each table from its snapshot to cap active runs
  -- and open fds.
  liftIO $ compactIngestStores utxoStore dedupStores lsm prev

  -- Per-epoch stats row + summary log.
  epochEnd <- liftIO getCurrentTime
  let elapsed   = diffUTCTime epochEnd epochStart
      elapsedSec = realToFrac elapsed :: Double
      blocksPerSec
        | elapsed > 0 = fromIntegral blockCount / realToFrac elapsed
        | otherwise   = 0

  essId <- liftIO $ assignEpochSyncStatsId resolver
  liftIO $ writeEpochSyncStats writer essId EpochSyncStats
    { epochSyncStatsEpochNo         = unEpochNo prev
    , epochSyncStatsBlocksProcessed = blockCount
    , epochSyncStatsBlocksPerSec    = blocksPerSec
    , epochSyncStatsElapsedSec      = elapsedSec
    , epochSyncStatsSyncedAt        = epochEnd
    , epochSyncStatsPhase           = renderPhase IngestChainHistory
    }

  when (elapsedSec > 10.0) $ liftIO performMajorGC

  mBoundary  <- liftIO $ readTVarIO boundaryVar
  storeStats <- liftIO $ UtxoStore.readStoreStats utxoStore
  memStats   <- liftIO $ readIORef (clsMemStats cls)

  let summary = renderEpochSummary prev blockCount elapsedSec blocksPerSec
                  storeStats mBoundary securityParam
                  (fmap (\(_, b, _) -> b) mLastBlock)
                  <> renderBufferSizes (addressBufferCounts buf)

  liftIO $ traceWith tracer $ LogMsg Info "Ingest" summary Nothing
  liftIO $ for_ (renderMemCurve prev memStats) $ \line ->
    traceWith tracer $ LogMsg Info "Ingest" line Nothing

  -- Dedup-store size + heap diagnostic. Gated on Debug because
  -- sampling 'getRTSStats' isn't free.
  when (minSev <= Debug) $ do
    dedupCounts <- liftIO $ dedupStoreSizes dedupStores
    heapInfo    <- liftIO sampleHeapBytes
    let heapText = case heapInfo of
          Just live -> " | heap=" <> fmtBytes live
          Nothing   -> ""
    liftIO $ traceWith tracer $ LogMsg Debug "Dedup"
      ( "Epoch " <> show (unEpochNo prev)
        <> " | " <> renderDedupCounts dedupCounts
        <> heapText
      ) Nothing

  -- Reset for the next epoch.
  liftIO $ do
    writeIORef (clsBlockCount cls) 0
    writeIORef (clsEpochStart cls) epochEnd
    writeIORef (clsMemStats cls) emptyMemStats

-- ---------------------------------------------------------------------------
-- * LedgerWorker coordination
-- ---------------------------------------------------------------------------

-- | Block until 'LedgerWorker' has produced any 'ApplyResult' at or
-- past @targetSlot@. Used as a slot-reached barrier; callers that
-- need the boundary's 'ApplyResult' itself use
-- 'readBoundaryApplyResult'.
waitForApplyResultAt :: LedgerEnv -> SlotNo -> IO ApplyResult
waitForApplyResultAt lenv targetSlot = Strict.atomically $ do
  mAR <- Strict.readTVar (leLatestApplyResult lenv)
  case mAR of
    SMaybe.Just ar
      | sdSlotNo (apSlotDetails ar) >= targetSlot -> pure ar
    _ -> retry

-- | Take the next boundary 'BoundaryApplyData' from the worker's FIFO,
-- blocking until one is available. Each call consumes exactly one
-- boundary; pair every consumer-side boundary detection with one
-- 'readBoundaryApplyResult'.
readBoundaryApplyResult :: LedgerEnv -> IO BoundaryApplyData
readBoundaryApplyResult lenv =
  Strict.atomically (readTBQueue (leBoundaryApplyResults lenv))

-- | Wait for the worker to catch up to @slot@, drain every
-- accumulated deposit-param entry at or before @prev@, and flush
-- them to @epoch_param_pending@. No-op when the ledger feature is
-- disabled.
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
-- * Helpers
-- ---------------------------------------------------------------------------

-- | Epoch cadence for the full snapshot+reopen cycle in
-- 'compactIngestStores'. Reopening a table from its snapshot
-- re-reads and CRC-checks every run file, so its cost grows
-- linearly with store size; a coarse cadence bounds active-run/fd
-- growth while amortising that stall. Snapshots themselves are
-- refreshed at every boundary regardless, so the restart-resume
-- anchor is always at most one epoch old and lookup contents (and
-- hence the UTxO hit rate that keeps Prep's backfill small) are
-- identical either way.
ingestCompactEveryEpochs :: Word64
ingestCompactEveryEpochs = 20

-- | Persist every LSM-backed Ingest table (cheap snapshot refresh);
-- on every 'ingestCompactEveryEpochs'-th epoch also reopen each
-- table from its snapshot to collapse the active run set and cap
-- open fds.
compactIngestStores
  :: UtxoStore.UtxoStore
  -> DedupStores
  -> LsmSession
  -> EpochNo          -- ^ epoch just completed
  -> IO ()
compactIngestStores utxoStore dedupStores lsm prev = do
  let fullCycle = unEpochNo prev `mod` ingestCompactEveryEpochs == 0
  if fullCycle
    then UtxoStore.compactUtxoStore utxoStore lsm
    else UtxoStore.persistUtxoStore utxoStore lsm
  for_ (DedupStore.allDedupStores dedupStores) $ \store ->
    if fullCycle
      then DedupStore.compactDedupStore store lsm
      else DedupStore.persistDedupStore store lsm

-- | Build the operator-facing per-epoch summary line:
-- @"Epoch N | M blk in Ts (R blk/s) [| utxo HR=…%] [| (~P%)]"@.
renderEpochSummary
  :: EpochNo
  -> Word64                   -- ^ blocks this epoch
  -> Double                   -- ^ elapsed seconds
  -> Double                   -- ^ blocks per second
  -> UtxoStore.StoreStats     -- ^ for the hit-rate segment
  -> Maybe BlockNo            -- ^ rollback boundary
  -> Word64                   -- ^ security parameter @k@
  -> Maybe Word64             -- ^ current block number (for the % segment)
  -> Text
renderEpochSummary prev blockCount elapsedSec blocksPerSec storeStats
                   mBoundary securityParam mCurBlock =
  "Epoch " <> show (unEpochNo prev)
    <> " | " <> fmtCount blockCount <> " blk in " <> fmtDuration elapsedSec
    <> " (" <> show (round blocksPerSec :: Int) <> " blk/s)"
    <> renderUtxoHitRate storeStats
    <> renderBoundaryPercent mBoundary securityParam mCurBlock

-- | Inline copy of 'DbSync.Phase.Ingest.Consumer.renderBoundaryPercent'
-- used by 'renderEpochSummary' so the public version stays in
-- "DbSync.Phase.Ingest.Consumer" (where the test suite pins its
-- location).
renderBoundaryPercent :: Maybe BlockNo -> Word64 -> Maybe Word64 -> Text
renderBoundaryPercent (Just (BlockNo boundary)) k (Just curBlock)
  | tip > 0 =
      let raw     = (fromIntegral curBlock / fromIntegral tip :: Double) * 100
          clamped = max 0 (min 100 raw)
      in " | [" <> fmtF2 clamped <> "%]"
  where
    tip = boundary + k
renderBoundaryPercent _ _ _ = ""

-- | Render the UtxoStore lifetime hit rate as @" | utxo HR=X.YY%"@.
-- Empty until the first lookup so the boot epoch isn't tagged
-- @0.00%@.
renderUtxoHitRate :: UtxoStore.StoreStats -> Text
renderUtxoHitRate s
  | looked == 0 = ""
  | otherwise   =
      let pct = fromIntegral (UtxoStore.ssHits s) * 100
              / fromIntegral looked :: Double
      in " | utxo HR=" <> fmtF2 pct <> "%"
  where
    looked = UtxoStore.ssHits s + UtxoStore.ssMisses s

-- | Render the per-epoch memory curve as three log lines: a header,
-- then aligned @in-use@ and @live@ rows at 4 evenly-spaced points
-- across the epoch (start → end) plus each row's running peak. @live@
-- rising with @in-use@ is retention; @live@ flat beneath a rising
-- @in-use@ is fragmentation. @[]@ when unsampled.
renderMemCurve :: EpochNo -> MemStats -> [Text]
renderMemCurve epoch ms
  | null (msSamples ms) = []
  | otherwise =
      [ ep <> " in-use" <> cols fst <> "  |" <> col (msPeakInUse ms)
      , ep <> " live  " <> cols snd <> "  |" <> col (msPeakLive ms)
      ]
  where
    ep       = "Epoch " <> show (unEpochNo epoch)
    pts      = downsampleN 4 (reverse (msSamples ms))
    cols sel = Text.concat [ col (sel p) | p <- pts ]
    col b    = Text.pack (printf "%8.1f" (fromIntegral b / gib :: Double))
    gib      = 1073741824 :: Double

-- | Pick up to @k@ evenly-spaced elements (including first and last).
downsampleN :: Int -> [a] -> [a]
downsampleN k xs
  | k <= 0 || null xs = []
  | n <= k            = xs
  | otherwise         = [ x | (i, x) <- zip [(0 :: Int) ..] xs, i `elem` idxs ]
  where
    n    = length xs
    step = fromIntegral (n - 1) / fromIntegral (k - 1) :: Double
    idxs = [ round (fromIntegral j * step) | j <- [0 .. k - 1] ] :: [Int]

-- | Render the per-epoch buffer cardinalities as @" | addrs N out N"@
-- (unique addresses, tx_out rows recorded this epoch).
renderBufferSizes :: (Int, Int) -> Text
renderBufferSizes (addrs, outs) =
  " | addrs " <> fmtCount addrs <> " out " <> fmtCount outs

-- | Render @[(name, count)]@ as @"name=N1,234 …"@.
renderDedupCounts :: [(Text, Int)] -> Text
renderDedupCounts = Text.intercalate " " . map one
  where
    one (n, c) = n <> "=" <> fmtCount c

-- | Live bytes after the most recent GC, sampled at epoch
-- boundaries. Requires @+RTS -T -RTS@; returns 'Nothing' otherwise.
sampleHeapBytes :: IO (Maybe Word64)
sampleHeapBytes = do
  enabled <- getRTSStatsEnabled
  if enabled
    then Just . gcdetails_live_bytes . gc <$> getRTSStats
    else pure Nothing

-- | Append the current @(in-use, live)@ sample to @ref@ (newest first)
-- and bump the running peaks. Components are forced so the sampler
-- can't itself retain the 'GCDetails'. No-op without @+RTS -T@.
recordMemSample :: IORef MemStats -> IO ()
recordMemSample ref = do
  enabled <- getRTSStatsEnabled
  when enabled $ do
    d <- gc <$> getRTSStats
    let !inuse = gcdetails_mem_in_use_bytes d
        !live  = gcdetails_live_bytes d
    modifyIORef' ref $ \ms -> ms
      { msSamples   = (inuse, live) : msSamples ms
      , msPeakInUse = max (msPeakInUse ms) inuse
      , msPeakLive  = max (msPeakLive ms) live
      }

-- | Render a byte count as a short human-readable string.
fmtBytes :: Word64 -> Text
fmtBytes b
  | b >= gib  = Text.pack (printf "%.1fGB" (fromIntegral b / fromIntegral gib :: Double))
  | b >= mib  = Text.pack (printf "%dMB"   (b `div` mib))
  | b >= kib  = Text.pack (printf "%dKB"   (b `div` kib))
  | otherwise = show b <> "B"
  where
    kib, mib, gib :: Word64
    kib = 1024
    mib = 1024 * 1024
    gib = 1024 * 1024 * 1024

-- | Format seconds as a coarse human-readable duration. Unlike
-- 'DbSync.Trace.Timing.fmtDuration' the sub-minute case is rounded
-- to whole seconds, keeping the per-epoch summary line compact.
fmtDuration :: Double -> Text
fmtDuration secs
  | secs < 60   = show (round secs :: Int) <> "s"
  | secs < 3600 =
      let t = round secs :: Int
      in show (t `div` 60) <> "m " <> show (t `mod` 60) <> "s"
  | otherwise   =
      let t = round secs :: Int
      in show (t `div` 3600) <> "h " <> show ((t `mod` 3600) `div` 60) <> "m"
