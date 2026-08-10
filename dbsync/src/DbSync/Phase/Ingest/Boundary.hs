{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Epoch-boundary handling for the Ingest consumer. The per-block
-- loop in "DbSync.Phase.Ingest.Consumer" spots an 'sdEpochNo' change
-- and calls 'handleEpochBoundary', which commits the finished epoch
-- and sets up the next one. The cascade is pipelined, so
-- @sync_state@ always lags the consumer by one epoch.
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
  , renderBoundaryPercent
  , renderUtxoHitRate
  , renderDedupCounts
  , renderMemCurve
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
import Control.Monad.IO.Unlift (withRunInIO)
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
  ( forceEpochAddressBuffer
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

-- | Snapshot of one finished epoch, held until the resolver catches
-- up. The @N → N+1@ boundary enqueues the job for epoch @N@, but
-- @sync_state@ for @N@ only advances at the @N+1 → N+2@ boundary,
-- once the tx-out worker resolved every @tx_out.address_id@ FK. A
-- clean exit drains the final queued job and writes this snapshot.
data PendingBoundary = PendingBoundary
  { pbEpoch       :: !EpochNo
    -- ^ Unused. The handler writes this field but never reads it.
  , pbLastSlot    :: !Word64
  , pbLastBlockNo :: !Word64
  , pbLastHash    :: !ByteString
  , pbCounters    :: !IdCounters
  }

-- | Per-epoch memory samples plus running peaks. The @(in-use, live)@
-- samples run newest first and drive the curve. The peaks track
-- separately, so they cover every sample, not just the curve points.
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
    -- ^ Per-epoch samples and peaks, taken on the consumer's sample
    -- stride and reset at each boundary. The in-use peak feeds the
    -- summary line; the full curve is Debug-only. Empty before the
    -- first sample, and without @+RTS -T@.
  , clsLastGcLive      :: !(IORef (Maybe Word64))
    -- ^ Live bytes right after the previous boundary major GC, the
    -- growth baseline that gates the next one. 'Nothing' before the
    -- first boundary GC, and without @+RTS -T@.
  }

-- | The replay machine starts primed when the boot had a replay
-- window.
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
    <*> newIORef Nothing

-- ---------------------------------------------------------------------------
-- * Boundary handler
-- ---------------------------------------------------------------------------

-- | Run the boundary cascade for the @prev → blockEpoch@ transition.
-- The consumer calls this when @prev /= blockEpoch@. The LSM persist
-- starts first so it overlaps the PG-bound steps. The cascade then
-- writes the per-epoch summary log and runs a growth-gated major GC
-- on long epochs.
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

  -- A near-boundary sample keeps a short epoch on the curve, even
  -- when it never reached the consumer's sample cadence.
  liftIO $ recordMemSample (clsMemStats cls)

  -- The persist runs first and concurrently. The rest of the cascade
  -- is PG round-trips and worker waits, so the stores stay quiescent
  -- and the persist — plus the CRC-heavy reopen on compaction epochs
  -- — overlaps it. 'withAsync' scopes the work to the cascade, and
  -- the per-table masked delete-then-save leaves exactly one snapshot
  -- per table on disk whichever way it exits.
  withRunInIO $ \runInIO ->
    withAsync (compactIngestStores utxoStore dedupStores lsm prev) $
      \compactAsync -> runInIO $ do
        -- Drain the existing pipeline, so the resolver runs in
        -- parallel with the next epoch's ingest.
        (buf, mConsumedSnap) <- liftIO $ do
          lsCommit loaderStream
          b <- takeAndReset addressBuffer
          cb <- case mConsumedBuf of
            Just ref -> Just <$> ConsumedByBuffer.takeAndReset ref
            Nothing  -> pure Nothing
          awaitTxOutDrained txOutWorker
          flushPendingDeposits hasLedger prev slot ctrlConn
          pure (b, cb)

        -- Debug-gated probe. It brackets the buffer force with logs
        -- and a timeout, so a wedged force shows as a log line
        -- instead of a hung pipeline. No performMajorGC here,
        -- because a GC cannot be interrupted.
        when (minSev <= Debug) $ liftIO $ do
          let tag m = traceWith tracer $ LogMsg Debug "TxOutProbe"
                ("epoch " <> show (unEpochNo prev) <> " " <> m)
          l0 <- sampleHeapBytes
          tag ("preForce live=" <> maybe "n/a" fmtBytes l0)
          r <- Timeout.timeout 60000000 $ do
            _ <- evaluate (forceEpochAddressBuffer buf)
            for_ mConsumedSnap (evaluate . ConsumedByBuffer.forceEpochConsumedByBuffer)
            pure ()
          tag (maybe "force TIMEOUT(60s)" (const "force done") r)

        -- Advance @sync_state@ for the epoch snapshotted last time.
        -- The address counter covers the rows the drain above wrote.
        addressIdCounter <- liftIO $ readAddressIdCounter txOutWorker
        mPending         <- liftIO $ readIORef (clsPendingBoundary cls)
        for_ mPending $ \pb ->
          writeSyncState $
            mkBoundarySyncStateRow
              (pbLastSlot pb) (pbLastBlockNo pb) (pbLastHash pb)
              (pbCounters pb) addressIdCounter
              schemaVersion ledgerEnabled

        liftIO $ enqueueTxOutJob txOutWorker (TxOutJob prev buf mConsumedSnap)

        -- Stash the snapshot for the next boundary.
        liftIO $ for_ mLastBlock $ \(lastSlot, lastBlockNo, lastHash) ->
          writeIORef (clsPendingBoundary cls) $ Just PendingBoundary
            { pbEpoch       = prev
            , pbLastSlot    = lastSlot
            , pbLastBlockNo = lastBlockNo
            , pbLastHash    = lastHash
            , pbCounters    = counters
            }

        -- Reopen the loader streams for the next epoch.
        liftIO $ lsReopen loaderStream

        -- The stores must settle before the boundary extractors
        -- resolve against them.
        liftIO $ wait compactAsync

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

  -- Collect only once the live heap outgrows the previous boundary
  -- collection's baseline. Without @+RTS -T@ there is no growth
  -- signal, so every epoch past the threshold collects.
  when (elapsedSec > 10.0) $ liftIO $ do
    mLive <- sampleHeapBytes
    mBase <- readIORef (clsLastGcLive cls)
    let grown = case (mLive, mBase) of
          (Just live, Just base) -> 2 * live >= 3 * base
          _                      -> True
    when grown $ do
      performMajorGC
      sampleHeapBytes >>= writeIORef (clsLastGcLive cls)

  mBoundary  <- liftIO $ readTVarIO boundaryVar
  storeStats <- liftIO $ UtxoStore.readStoreStats utxoStore
  memStats   <- liftIO $ readIORef (clsMemStats cls)

  let summary = renderEpochSummary prev blockCount elapsedSec blocksPerSec
                  storeStats (msPeakInUse memStats) mBoundary securityParam
                  (fmap (\(_, b, _) -> b) mLastBlock)

  liftIO $ traceWith tracer $ LogMsg Info "Ingest" summary
  when (minSev <= Debug) $ liftIO $
    for_ (renderMemCurve prev memStats) $ \line ->
      traceWith tracer $ LogMsg Debug "Ingest" line

  -- Dedup-store size and heap diagnostic, gated on Debug because
  -- sampling 'getRTSStats' is not free.
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
      )

  -- Reset for the next epoch.
  liftIO $ do
    writeIORef (clsBlockCount cls) 0
    writeIORef (clsEpochStart cls) epochEnd
    writeIORef (clsMemStats cls) emptyMemStats

-- ---------------------------------------------------------------------------
-- * LedgerWorker coordination
-- ---------------------------------------------------------------------------

-- | Block until 'LedgerWorker' produces any 'ApplyResult' at or past
-- @targetSlot@. This is a slot-reached barrier. A caller that needs
-- the boundary's own 'ApplyResult' uses 'readBoundaryApplyResult'.
waitForApplyResultAt :: LedgerEnv -> SlotNo -> IO ApplyResult
waitForApplyResultAt lenv targetSlot = Strict.atomically $ do
  mAR <- Strict.readTVar (leLatestApplyResult lenv)
  case mAR of
    SMaybe.Just ar
      | sdSlotNo (apSlotDetails ar) >= targetSlot -> pure ar
    _ -> retry

-- | Take the next 'BoundaryApplyData' from the worker's FIFO,
-- blocking until one arrives. Each call consumes exactly one
-- boundary, so pair every boundary detection with one call.
readBoundaryApplyResult :: LedgerEnv -> IO BoundaryApplyData
readBoundaryApplyResult lenv =
  Strict.atomically (readTBQueue (leBoundaryApplyResults lenv))

-- | Wait for the worker to reach @slot@, drain every accumulated
-- deposit-param entry at or before @prev@, then write them to
-- @epoch_param_pending@. Does nothing when the ledger feature is off.
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

-- | Epoch cadence for the full snapshot and reopen cycle in
-- 'compactIngestStores'. A reopen re-reads and CRC-checks every run
-- file, so a coarse cadence bounds the active-run and fd growth
-- while it amortises that stall. Every boundary still refreshes the
-- snapshots, so the restart anchor stays at most one epoch old and
-- the lookup contents match either way.
ingestCompactEveryEpochs :: Word64
ingestCompactEveryEpochs = 20

-- | Persist every LSM-backed Ingest table. On each
-- 'ingestCompactEveryEpochs'-th epoch it also reopens each table
-- from its snapshot, which collapses the active run set and caps
-- open fds. Nothing may touch the stores while this runs; the
-- boundary handler overlaps it with the PG-bound cascade.
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

-- | Build the per-epoch summary line:
-- @"Epoch N | M blk in Ts (R blk/s) [| utxo hitrate=…%] [| peak mem=…] [| (~P%)]"@.
renderEpochSummary
  :: EpochNo
  -> Word64                   -- ^ blocks this epoch
  -> Double                   -- ^ elapsed seconds
  -> Double                   -- ^ blocks per second
  -> UtxoStore.StoreStats     -- ^ for the hit-rate segment
  -> Word64                   -- ^ peak in-use bytes (0 when unsampled)
  -> Maybe BlockNo            -- ^ rollback boundary
  -> Word64                   -- ^ security parameter @k@
  -> Maybe Word64             -- ^ current block number (for the % segment)
  -> Text
renderEpochSummary prev blockCount elapsedSec blocksPerSec storeStats
                   peakInUse mBoundary securityParam mCurBlock =
  "Epoch " <> show (unEpochNo prev)
    <> " | " <> fmtCount blockCount <> " blk in " <> fmtDuration elapsedSec
    <> " (" <> show (round blocksPerSec :: Int) <> " blk/s)"
    <> renderUtxoHitRate storeStats
    <> renderPeakMem peakInUse
    <> renderBoundaryPercent mBoundary securityParam mCurBlock

-- | Render the epoch's peak in-use memory as @" | peak mem=X.YGB"@.
-- Empty when nothing sampled it.
renderPeakMem :: Word64 -> Text
renderPeakMem b
  | b == 0    = ""
  | otherwise = " | peak mem=" <> fmtBytes b

-- | Progress segment @\" | [87.32%]\"@, the current block number
-- against the derived node tip @boundary + k@. Empty until a
-- boundary arrives, a block lands, and the tip is non-zero.
renderBoundaryPercent :: Maybe BlockNo -> Word64 -> Maybe Word64 -> Text
renderBoundaryPercent (Just (BlockNo boundary)) k (Just curBlock)
  | tip > 0 =
      let raw     = (fromIntegral curBlock / fromIntegral tip :: Double) * 100
          clamped = max 0 (min 100 raw)
      in " | [" <> fmtF2 clamped <> "%]"
  where
    tip = boundary + k
renderBoundaryPercent _ _ _ = ""

-- | Render the lifetime hit rate as @" | utxo hitrate=X.YY%"@. Empty
-- until the first lookup, so the boot epoch avoids a @0.00%@ tag.
renderUtxoHitRate :: UtxoStore.StoreStats -> Text
renderUtxoHitRate s
  | looked == 0 = ""
  | otherwise   =
      let pct = fromIntegral (UtxoStore.ssHits s) * 100
              / fromIntegral looked :: Double
      in " | utxo hitrate=" <> fmtF2 pct <> "%"
  where
    looked = UtxoStore.ssHits s + UtxoStore.ssMisses s

-- | Render the per-epoch memory curve as two aligned log lines,
-- @in-use@ and @live@, at 4 evenly-spaced points from epoch start to
-- end, each followed by its running peak. A @live@ row that rises
-- with @in-use@ shows retention. A flat @live@ row under a rising
-- @in-use@ shows fragmentation. Empty when nothing sampled it.
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

-- | Render @[(name, count)]@ as @"name=N1,234 …"@.
renderDedupCounts :: [(Text, Int)] -> Text
renderDedupCounts = Text.intercalate " " . map one
  where
    one (n, c) = n <> "=" <> fmtCount c

-- | Live bytes after the most recent GC. Requires @+RTS -T -RTS@ and
-- returns 'Nothing' without it.
sampleHeapBytes :: IO (Maybe Word64)
sampleHeapBytes = do
  enabled <- getRTSStatsEnabled
  if enabled
    then Just . gcdetails_live_bytes . gc <$> getRTSStats
    else pure Nothing

-- | Prepend the current @(in-use, live)@ sample to @ref@ and bump the
-- running peaks. Both components are forced, so the sampler cannot
-- retain the 'GCDetails'. Does nothing without @+RTS -T@.
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

-- | Format seconds as a coarse duration. This rounds the sub-minute
-- case to whole seconds, which keeps the summary line compact.
-- 'DbSync.Trace.Timing.fmtDuration' does not.
fmtDuration :: Double -> Text
fmtDuration secs
  | secs < 60   = show (round secs :: Int) <> "s"
  | secs < 3600 =
      let t = round secs :: Int
      in show (t `div` 60) <> "m " <> show (t `mod` 60) <> "s"
  | otherwise   =
      let t = round secs :: Int
      in show (t `div` 3600) <> "h " <> show ((t `mod` 3600) `div` 60) <> "m"
