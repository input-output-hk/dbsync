{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The Follow loop: per-block INSERT against PG with rollback
-- support. Drives both the 'FollowingVolatileTail' and
-- 'FollowingChainTip' phases; the only behavioural difference is
-- the phase tag itself, which flips between the two as the consumer
-- catches up with or falls behind the receiver.
--
-- The loop reads one 'ChainSyncMsg' at a time from 'feBlockQueue'
-- and either applies a forward block in its own PG transaction, or
-- runs the rollback cascade for a 'MsgRollback' marker. Between
-- messages the loop also fires an idle heartbeat every
-- 'idleHeartbeatMicros' microseconds while in 'FollowingChainTip',
-- so a quiet chain doesn't look like a stalled app at Info level.
module DbSync.Phase.Following.Run
  ( run
  ) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Tracer (traceWith)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Numeric (showFFloat)
import qualified Hasql.Connection as Conn
import qualified Hasql.Pipeline as Pipeline
import qualified Hasql.Session as Sess


import Ouroboros.Consensus.Block (blockSlot)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Network.Block (pattern BlockPoint)

import DbSync.AppM (FollowM, runAppM)
import DbSync.Db.Schema.EpochSyncStats (EpochSyncStats (..))
import DbSync.Parser.Dispatch (parseBlock)
import DbSync.Parser.Types (CardanoPoint, GenericBlock (..))
import DbSync.Phase.Type (SyncPhase (..), renderPhase)
import DbSync.Db.Run (useConn)
import DbSync.Db.Statement.SyncState (writeSyncStateSlotStmt)
import DbSync.Db.Statement.Transaction (beginSql, commitSql, rollbackSql)

import DbSync.App.Config.Types
  ( SyncConfig (..)
  , OptionFlag (..)
  , DbSyncOptions (..)
  )
import DbSync.App.Env (CoreEnv (..), FollowEnv (..), HasConfig (..), HasNetwork)
import DbSync.Extractor (ExtractorDef (..), takeBlockLedgerData)
import DbSync.Extractor.EpochBoundary (runEpochBoundary)
import DbSync.Extractor.Governance (runGovernanceBoundary)
import DbSync.Extractor.PoolStats (runPoolStatsBoundary)
import DbSync.Extractor.StakeDelegationLedger (runStakeDelegationLedgerBoundary)
import DbSync.Extractor.Pipeline (processBlock)
import DbSync.ChainSync.Msg (ChainSyncMsg (..))
import DbSync.Phase.Following.IdAllocator (allocateAllIds)
import DbSync.Phase.Following.IdCounts (countAssignableIds)
import DbSync.Phase.Following.Resolver (mkBufferedFollowResolver)
import qualified DbSync.Phase.Following.Rollback as Rollback
import DbSync.Phase.Following.WriteBuffer (drain, newWriteBuffer)
import DbSync.Phase.Following.Writer (mkBufferedWriter)
import DbSync.Phase.Ingest.Boundary (readBoundaryApplyResult)
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.SyncState.Row (HasControlConnection)
import DbSync.Worker.Ledger.Types (HasLedgerEnv (..))
import DbSync.Writer (HasWriter (..), Writer (..))
import DbSync.Phase.Current
  ( CurrentPhase
  , readCurrentPhase
  , readCurrentPhaseSTM
  , setCurrentPhase
  )
import DbSync.StateQuery (getSlotDetails, observeBlockSTM)
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Replay
  ( ReplayAdvance (..)
  , ReplayLog (..)
  , ReplayLogState (..)
  , advanceReplay
  , renderReplayPercent
  )
import DbSync.Db.Schema.Types (TableDef)
import DbSync.Error (AppError (..))
import DbSync.Trace.Timing (fmtCount, fmtDuration, fmtF2)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))
import DbSync.Trace.Watchdog (Watchdog, bumpConsumer, setConsumerNote)

-- | Cadence for the periodic Follow-loop progress log while in
-- 'FollowingVolatileTail'. In 'FollowingChainTip' the loop logs every
-- applied block instead so the operator has per-block visibility at
-- mainnet's ~20 s/block cadence.
logEveryNBlocks :: Word64
logEveryNBlocks = 100

-- | Maximum quiet period before the Follow loop emits a
-- "still at tip" heartbeat. Only fires in 'FollowingChainTip'; in
-- 'FollowingVolatileTail' the windowed summary covers visibility.
-- 30 s gives the operator a clear stall signal without flooding logs
-- during normal block gaps (mainnet caps at ~40 s on a missed-slot
-- pair).
idleHeartbeatMicros :: Int
idleHeartbeatMicros = 30_000_000

-- | State carried across forward blocks. Drives the windowed log
-- cadence, the per-block delta at tip, the idle-heartbeat
-- "N ago" suffix, and the per-epoch counters consumed by the
-- boundary @epoch_sync_stats@ write.
data FollowProgress = FollowProgress
  { fpWindowStart      :: !UTCTime
    -- ^ When the current 'logEveryNBlocks' window opened.
  , fpBlocksThisWindow :: !Word64
  , fpLastEpoch        :: !(Maybe Word64)
    -- ^ 'Nothing' before the first block lands.
  , fpLastBlockAt      :: !(Maybe UTCTime)
    -- ^ When the most recent block finished 'processForward'.
  , fpLastSlot         :: !(Maybe Word64)
    -- ^ Slot of the most recent applied block.
  , fpEpochStart       :: !UTCTime
    -- ^ Wall-clock at which the current epoch's first Follow-observed
    -- block landed.
  , fpEpochBlocks      :: !Word64
    -- ^ Forward blocks observed in the current epoch.
  }

-- | Drain the chainsync queue forever.
--
-- Each 'MsgForward' is parsed, extracted, and applied to PG inside a
-- single @BEGIN@/@COMMIT@ envelope that also advances
-- @dbsync_sync_state.last_committed_slot@ — so a crash between blocks
-- never leaves rows in PG past the recorded position. 'MsgRollback'
-- runs the cascade and updates the same sync-state columns to the
-- target slot.
run :: FollowM ()
run = do
  FollowEnv{feCore, feBlockQueue, feReplayBootSlot} <- ask
  let tracer   = ceTracer    feCore
      phaseRef = ceCurrentPhase feCore
  liftIO $ do
    component <- readPhaseComponent phaseRef
    traceWith tracer $ LogMsg Info component
      "consumer started; draining chainsync queue" Nothing
  startedAt <- liftIO getCurrentTime
  progressRef <- liftIO $ newIORef FollowProgress
    { fpWindowStart      = startedAt
    , fpBlocksThisWindow = 0
    , fpLastEpoch        = Nothing
    , fpLastBlockAt      = Nothing
    , fpLastSlot         = Nothing
    , fpEpochStart       = startedAt
    , fpEpochBlocks      = 0
    }
  -- Seed the replay-progress state machine. Inert ('NoReplay')
  -- when there's no replay window, primed ('ReplayPending') when
  -- there is.
  replayRef <- liftIO $ newIORef $ case feReplayBootSlot of
    Just _  -> ReplayPending
    Nothing -> NoReplay
  forever $ do
    mMsg <- liftIO $
      waitForMsgOrHeartbeat feBlockQueue phaseRef idleHeartbeatMicros
    case mMsg of
      Just (MsgForward  blk)   -> processForward progressRef replayRef blk
      Just (MsgRollback point) -> processRollback point
      Nothing                  -> emitIdleHeartbeat progressRef

-- | Read the next message from the queue, or fall through with
-- 'Nothing' after the heartbeat timer expires. Only fires the timer
-- branch while the current phase is 'FollowingChainTip'; in any
-- other phase this behaves like a plain 'readTBQueue' (windowed
-- summaries cover visibility there).
waitForMsgOrHeartbeat
  :: STM.TBQueue ChainSyncMsg
  -> CurrentPhase
  -> Int
  -> IO (Maybe ChainSyncMsg)
waitForMsgOrHeartbeat q phaseRef micros = do
  delayVar <- STM.registerDelay micros
  STM.atomically $
    (Just <$> STM.readTBQueue q)
      `STM.orElse` heartbeatBranch delayVar
  where
    heartbeatBranch delayVar = do
      phase <- readCurrentPhaseSTM phaseRef
      when (phase /= FollowingChainTip) STM.retry
      expired <- STM.readTVar delayVar
      unless expired STM.retry
      pure Nothing

-- | Render the current phase as the log-component string. Always
-- reflects whether we are catching up or steady-state, so a reader
-- can tell at a glance.
readPhaseComponent :: CurrentPhase -> IO Text
readPhaseComponent = fmap renderPhase . readCurrentPhase

-- | Apply one forward block.
--
-- Two regimes:
--
--   * Replay window ('feReplayBootSlot' set, @slot <= bootSlot@) —
--     the block is already in PG and the ledger worker is
--     re-applying it via the receiver fan-out. The consumer just
--     bumps the watchdog, advances the replay-progress state
--     machine, and runs the phase-flip predicate.
--
--   * Normal — apply the block inside one PG transaction:
--
--       1. Count the IDs the extractors will need
--          ('countAssignableIds') and allocate them in a single
--          libpq pipeline ('allocateAllIds').
--       2. Run extractors with a buffered resolver + writer. Dedup
--          resolves still hit PG synchronously (one SELECT plus a
--          possible @nextval@ on miss) but consult a per-block dedup
--          cache so siblings find each other. INSERTs land on a
--          single 'WriteBuffer'.
--       3. BEGIN, pipeline-flush the writes plus the
--          @last_committed_*@ UPDATE, COMMIT. The three Sessions are
--          inlined here so the 'onException' rolls back cleanly
--          without masking the original exception.
processForward
  :: IORef FollowProgress
  -> IORef ReplayLogState
  -> CardanoBlock StandardCrypto
  -> FollowM ()
processForward progressRef replayRef cardanoBlock = do
  env@FollowEnv
    { feCore
    , feWatchdog
    , feStateQueryVar
    , feHasqlConnection
    , feHasLedgerEnv
    , feReplayBootSlot
    , feReplayStartSlot
    } <- ask
  let slot     = blockSlot cardanoBlock
      tracer   = getTracer env
      phaseRef = ceCurrentPhase feCore
  liftIO $ do
    setConsumerNote feWatchdog "follow: processForward"
    bumpConsumer feWatchdog slot
    advanceAndLogReplay tracer replayRef feReplayStartSlot feReplayBootSlot slot
  case feReplayBootSlot of
    Just bootSlot | slot <= bootSlot -> do
      -- Replay window: the row is already in PG and the ledger
      -- worker is re-applying the block via 'leLedgerQueue'. Skip
      -- the INSERT + sync_state advance; 'maybeFlipToTip' runs
      -- unchanged (it gates on the queue + latest received point,
      -- not on PG writes).
      liftIO $ setConsumerNote feWatchdog "follow: replay-skip"
      -- The worker still enqueues a per-block ApplyResult for the
      -- replayed block; drain and discard so the queue stays empty.
      liftIO $ void $ takeBlockLedgerData feHasLedgerEnv
      maybeFlipToTip slot
    _ -> do
      liftIO $ void $ atomically $ observeBlockSTM feStateQueryVar cardanoBlock
      sd <- getSlotDetails slot
      now <- liftIO getCurrentTime
      let !genBlock = parseBlock sd cardanoBlock
          !curEpoch = unEpochNo (blkEpochNo genBlock)
          !counts   = countAssignableIds genBlock
          triple    = ( unSlotNo  (blkSlotNo  genBlock)
                      , unBlockNo (blkBlockNo genBlock)
                      , blkHash   genBlock
                      )
      snap <- liftIO $ readIORef progressRef
      let prevEpoch = fpLastEpoch snap
          boundaryCrossed = case prevEpoch of
            Just prev -> prev /= curEpoch
            Nothing   -> False
      liftIO $ do
        preAllocated <- allocateAllIds feHasqlConnection counts
        buf          <- newWriteBuffer
        resolver     <- mkBufferedFollowResolver feHasqlConnection preAllocated buf
        let writer      = mkBufferedWriter buf
            bufferedEnv = env { feResolver = resolver, feWriter = writer }
        runAppM bufferedEnv (processBlock genBlock)
        case (boundaryCrossed, prevEpoch) of
          (True, Just prev) -> do
            phase <- readCurrentPhase phaseRef
            runAppM bufferedEnv $
              writeFollowEpochSyncStats phase snap prev now
            runAppM bufferedEnv (runFollowBoundary feHasLedgerEnv)
          _ -> pure ()
        writes <- drain buf
        let flushAndAdvance =
              writes *> void (Pipeline.statement triple writeSyncStateSlotStmt)
        runFollowBlockTx feHasqlConnection feWatchdog flushAndAdvance
      maybeFlipToTip (blkSlotNo genBlock)
      maybeLogProgress progressRef now genBlock

-- | Drain the ledger worker's next boundary 'ApplyResult' and run
-- the governance and epoch-boundary extractors against it. No-op
-- when the ledger feature is disabled or no block has yet been
-- extracted.
--
-- Governance runs first when enabled: it publishes the enacted
-- @(committee, no_confidence, constitution)@ id triple onto the
-- resolver's scratchpad so 'runEpochBoundary' picks it up when it
-- constructs the next @epoch_state@ row.
runFollowBoundary
  :: ( HasResolver env
     , HasWriter env
     , HasConfig env
     , HasControlConnection env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => HasLedgerEnv -> m ()
runFollowBoundary = \case
  LedgerDisabled _   -> pure ()
  LedgerEnabled lenv -> do
    applyResult  <- liftIO $ readBoundaryApplyResult lenv
    resolver     <- asks getResolver
    mBlockId     <- liftIO $ lookupLastBlockId resolver
    governanceOn <- asks (prEnabled . pcGovernance . scOptions . getConfig)
    poolStatsOn  <- asks (prEnabled . pcPoolStats  . scOptions . getConfig)
    sdlOn        <- asks (prEnabled . pcStakeDelegationLedger . scOptions . getConfig)
    for_ mBlockId $ \blockId -> do
      when governanceOn $ runGovernanceBoundary applyResult blockId
      runEpochBoundary applyResult blockId
      when poolStatsOn  $ runPoolStatsBoundary  applyResult blockId
      when sdlOn        $ runStakeDelegationLedgerBoundary applyResult blockId

-- | Step the replay-progress state machine for this block and emit
-- any indicated trace. A no-op outside the replay window (the
-- machine is 'NoReplay'); inside the window, fires periodic progress
-- lines and one completion line on the first post-window block.
advanceAndLogReplay
  :: AppTracer
  -> IORef ReplayLogState
  -> Maybe SlotNo          -- ^ lower edge of the window (snapshot slot)
  -> Maybe SlotNo          -- ^ upper edge of the window (last_committed_slot)
  -> SlotNo                -- ^ current block's slot
  -> IO ()
advanceAndLogReplay tracer replayRef mReplayStart mReplayBoot slot = do
  now <- getCurrentTime
  logEvent <- atomicModifyIORef' replayRef $ \prev ->
    let adv = advanceReplay slot mReplayBoot now prev
    in (raNewState adv, raLog adv)
  let traceReplay msg =
        traceWith tracer $ LogMsg Info "LedgerReplay" msg Nothing
  case logEvent of
    ReplayLogNothing -> pure ()
    ReplayLogProgress n ->
      traceReplay $
        "applied " <> fmtCount n <> " blocks; current slot "
          <> show (unSlotNo slot)
          <> renderReplayPercent mReplayStart mReplayBoot slot
    ReplayLogComplete n elapsed ->
      traceReplay $
        "replay complete; applied " <> fmtCount n
          <> " blocks in " <> fmtF2 (realToFrac elapsed :: Double)
          <> "s, resuming Follow PG writes at slot "
          <> show (unSlotNo slot)

-- | Wrap one block's worth of pipelined writes in a PG
-- @BEGIN@/@COMMIT@ envelope. A flush failure triggers a best-effort
-- ROLLBACK so the open transaction doesn't leak, but the original
-- exception still propagates.
runFollowBlockTx :: Conn.Connection -> Watchdog -> Pipeline.Pipeline () -> IO ()
runFollowBlockTx conn watchdog flush = do
  setConsumerNote watchdog "follow: BEGIN"
  useConn "Following.BEGIN" conn (Sess.script beginSql)
  setConsumerNote watchdog "follow: flush pipeline"
  useConn "Following.flush" conn (Sess.pipeline flush)
    `onException` rollbackQuiet conn
  setConsumerNote watchdog "follow: COMMIT"
  useConn "Following.COMMIT" conn (Sess.script commitSql)

-- | Best-effort ROLLBACK. Swallows its own errors so a failed
-- rollback doesn't mask the original exception that triggered it.
rollbackQuiet :: Conn.Connection -> IO ()
rollbackQuiet conn =
  void (Conn.use conn (Sess.script rollbackSql))
    `catch` \(_ :: SomeException) -> pure ()

-- | Flip the phase from 'FollowingVolatileTail' to
-- 'FollowingChainTip' once the consumer has caught the receiver's
-- latest received slot and the block queue is empty. One-way:
-- a subsequent 'MsgRollback' is the only path back.
--
-- Suppressed inside the replay window: the skip-only consumer
-- outpaces the receiver, so the predicate fires spuriously.
maybeFlipToTip :: SlotNo -> FollowM ()
maybeFlipToTip appliedSlot = do
  FollowEnv{feCore, feBlockQueue, feLatestReceivedPoint, feReplayBootSlot} <- ask
  let phaseRef = ceCurrentPhase feCore
      inReplay = maybe False (appliedSlot <=) feReplayBootSlot
  unless inReplay $ do
    phase <- liftIO $ readCurrentPhase phaseRef
    when (phase == FollowingVolatileTail) $ do
      qLen <- liftIO $ atomically (STM.lengthTBQueue feBlockQueue)
      when (qLen == 0) $ do
        mLatest <- liftIO $ readIORef feLatestReceivedPoint
        case mLatest of
          Just (BlockPoint latestSlot _)
            | appliedSlot >= latestSlot ->
                setCurrentPhase phaseRef FollowingChainTip
          _ -> pure ()

-- | Write the @epoch_sync_stats@ row for the just-finished epoch.
-- Block count and elapsed window come from the pre-block
-- 'FollowProgress' snapshot.
writeFollowEpochSyncStats
  :: ( HasResolver env
     , HasWriter env
     , MonadReader env m
     , MonadIO m
     )
  => SyncPhase
  -> FollowProgress
  -> Word64        -- ^ just-completed epoch number
  -> UTCTime       -- ^ wall-clock when the crossing block landed
  -> m ()
writeFollowEpochSyncStats phase snap prev now = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  let blockCount   = fpEpochBlocks snap
      elapsedSec   = realToFrac (diffUTCTime now (fpEpochStart snap)) :: Double
      blocksPerSec
        | elapsedSec > 0 = fromIntegral blockCount / elapsedSec
        | otherwise      = 0
  liftIO $ do
    essId <- assignEpochSyncStatsId resolver
    writeEpochSyncStats writer essId EpochSyncStats
      { epochSyncStatsEpochNo         = prev
      , epochSyncStatsBlocksProcessed = blockCount
      , epochSyncStatsBlocksPerSec    = blocksPerSec
      , epochSyncStatsElapsedSec      = elapsedSec
      , epochSyncStatsSyncedAt        = now
      , epochSyncStatsPhase           = renderPhase phase
      }

-- | Update the progress counter for this block, then emit either:
--
--   * one Info line per applied block when in 'FollowingChainTip' —
--     at mainnet's ~20 s/block cadence this is roughly one log per
--     20 s, the natural "I'm alive" rhythm at tip; or
--   * the windowed summary when 'logEveryNBlocks' blocks have been
--     applied or a new epoch has crossed (other phases). Per-block
--     spam isn't useful while still catching up.
--
-- Also rolls the per-epoch counters forward (reset at each boundary
-- crossing) for the next @epoch_sync_stats@ row.
maybeLogProgress :: IORef FollowProgress -> UTCTime -> GenericBlock -> FollowM ()
maybeLogProgress progressRef now gb = do
  FollowEnv{feCore, feBlockQueue} <- ask
  let tracer   = ceTracer    feCore
      phaseRef = ceCurrentPhase feCore
  liftIO $ do
    phase <- readCurrentPhase phaseRef
    let !curSlot  = unSlotNo  (blkSlotNo  gb)
        !curEpoch = unEpochNo (blkEpochNo gb)
        !curBlock = unBlockNo (blkBlockNo gb)
    (mWindowed, mPrevBlockAt) <- atomicModifyIORef' progressRef $ \p ->
      let !window'      = fpBlocksThisWindow p + 1
          !epochCrossed = case fpLastEpoch p of
                            Just prev -> curEpoch /= prev
                            Nothing   -> False
          !cadenceHit   = window' >= logEveryNBlocks
          !shouldLogWindowed = epochCrossed || cadenceHit
          !prevBlockAt = fpLastBlockAt p
          !p' = (if shouldLogWindowed
                   then p { fpWindowStart      = now
                          , fpBlocksThisWindow = 0
                          }
                   else p { fpBlocksThisWindow = window' }
                ) { fpLastEpoch   = Just curEpoch
                  , fpLastBlockAt = Just now
                  , fpLastSlot    = Just curSlot
                  , fpEpochStart  = if epochCrossed then now else fpEpochStart p
                  , fpEpochBlocks = if epochCrossed then 1 else fpEpochBlocks p + 1
                  }
          !info = (fpWindowStart p, window')
      in (p', ( if shouldLogWindowed then Just info else Nothing
              , prevBlockAt
              ))
    component <- readPhaseComponent phaseRef
    case phase of
      FollowingChainTip ->
        traceWith tracer $ LogMsg Info component (mconcat
          [ "applied block ", show curBlock
          , ", slot ", show curSlot
          , ", epoch ", show curEpoch
          , renderSinceLast now mPrevBlockAt
          ]) Nothing
      _ ->
        for_ mWindowed $ \(windowStart, blocks) -> do
          qLen <- atomically $ STM.lengthTBQueue feBlockQueue
          let !elapsed = realToFrac (diffUTCTime now windowStart) :: Double
              !rate    = if elapsed > 0 then fromIntegral blocks / elapsed else 0
              msg      = mconcat
                [ "slot ",  show curSlot
                , ", epoch ", show curEpoch
                , " | ", show blocks, " blk in ", fmtDuration elapsed
                , " (", fmtRate rate, " blk/s)"
                , " | queue=", show qLen
                ]
          traceWith tracer $ LogMsg Info component msg Nothing

-- | Render the "+T since prev" suffix used on the per-block
-- 'FollowingChainTip' log. Empty on the first block (no previous)
-- so the line stays compact.
renderSinceLast :: UTCTime -> Maybe UTCTime -> Text
renderSinceLast now = \case
  Nothing -> ""
  Just t  -> " (+" <> fmtDuration (realToFrac (diffUTCTime now t)) <> " since prev)"

-- | Emit the idle "still at tip" heartbeat. Fired from the main
-- loop's heartbeat branch; the wait function gates on phase so this
-- is only reachable in 'FollowingChainTip'.
emitIdleHeartbeat :: IORef FollowProgress -> FollowM ()
emitIdleHeartbeat progressRef = do
  FollowEnv{feCore, feBlockQueue} <- ask
  let tracer   = ceTracer    feCore
      phaseRef = ceCurrentPhase feCore
  liftIO $ do
    now <- getCurrentTime
    progress <- readIORef progressRef
    qLen <- atomically $ STM.lengthTBQueue feBlockQueue
    component <- readPhaseComponent phaseRef
    let body = case (fpLastSlot progress, fpLastBlockAt progress) of
          (Just s, Just t) -> mconcat
            [ "still at tip, last block at slot ", show s
            , " (", fmtDuration (realToFrac (diffUTCTime now t)), " ago)"
            , ", queue=", show qLen
            ]
          _ -> "still at tip, no blocks applied yet, queue=" <> show qLen
    traceWith tracer $ LogMsg Info component body Nothing

-- | Compact rate formatter: more precision at low rates so a slow
-- sync doesn't read as "0 blk/s", less precision once the rate is
-- big enough that decimals are noise.
fmtRate :: Double -> Text
fmtRate r
  | r < 10    = toS (showFFloat (Just 2) r "")
  | r < 1000  = toS (showFFloat (Just 1) r "")
  | otherwise = toS (showFFloat (Just 0) r "")

-- | Apply one rollback marker.
--
-- Drops the phase back to 'FollowingVolatileTail' before the
-- cascade so the log identifies the run as catching up again.
-- The cascade itself DELETEs every row past the target block and
-- advances @last_committed_*@ to match, all in one PG transaction.
processRollback :: CardanoPoint -> FollowM ()
processRollback point = do
  FollowEnv{feCore, feWatchdog} <- ask
  let tracer    = ceTracer    feCore
      phaseRef  = ceCurrentPhase feCore
      tableDefs = concatMap pdTables (ceExtractors feCore)
  liftIO $ setConsumerNote feWatchdog "follow: processRollback"
  setCurrentPhase phaseRef FollowingVolatileTail
  liftIO $ do
    component <- readPhaseComponent phaseRef
    traceWith tracer $ LogMsg Info component
      ("rollback to " <> show point) Nothing
  rollbackWithRetry tableDefs point

-- | Total attempts (initial + retries) before a rollback failure
-- surfaces. Reads are idempotent and writes run in one PG
-- transaction, so a mid-flight failure commits nothing.
rollbackMaxAttempts :: Int
rollbackMaxAttempts = 3

-- | Wait before the first retry (microseconds); doubles each retry.
rollbackBaseDelayMicros :: Int
rollbackBaseDelayMicros = 250_000

-- | Run the rollback cascade, retrying a few times on
-- 'AppDatabaseError'. Lets a transient PG-side glitch (e.g. EINTR
-- surfaced as @XX000 internal_error@) ride out without crashing.
rollbackWithRetry :: [TableDef] -> CardanoPoint -> FollowM ()
rollbackWithRetry tableDefs point = go 1
  where
    go attempt = do
      tracer <- asks getTracer
      result <- withRunInIO $ \runInIO ->
        try (runInIO (Rollback.rollbackToPoint tableDefs point))
      case (result :: Either AppError ()) of
        Right () -> pure ()
        Left (AppDatabaseError _ msg)
          | attempt < rollbackMaxAttempts -> do
              let delayMicros =
                    rollbackBaseDelayMicros * 2 ^ (attempt - 1)
              liftIO $ do
                traceWith tracer $ LogMsg Warning "Rollback" (mconcat
                  [ "transient PG error rolling back to ", show point
                  , " (attempt ", show attempt
                  , "/", show rollbackMaxAttempts
                  , "); retrying in "
                  , show (delayMicros `div` 1000), "ms — "
                  , msg
                  ]) Nothing
                threadDelay delayMicros
              go (attempt + 1)
        Left e -> liftIO (throwIO e)
