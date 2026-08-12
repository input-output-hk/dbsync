{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

-- | The Follow loop: per-block INSERT against PG with rollback
-- support. It reads one 'ChainSyncMsg' at a time from 'feBlockQueue'
-- and either applies a forward block in its own PG transaction, or
-- runs the rollback cascade for a 'MsgRollback' marker.
--
-- The loop drives both 'FollowingVolatileTail' and
-- 'FollowingChainTip'. Only the phase tag differs, and it flips as
-- the consumer catches up with or falls behind the receiver.
module DbSync.Phase.Following.Run
  ( run
  , shouldFlipToTip
  ) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Tracer (traceWith)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Numeric (showFFloat)
import qualified Hasql.Connection as Conn
import qualified Hasql.Pipeline as Pipeline
import qualified Hasql.Session as Sess


import Ouroboros.Consensus.Block (blockNo, blockSlot)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)

import DbSync.AppM (FollowM, runAppM)
import DbSync.Db.Schema.EpochSyncStats (EpochSyncStats (..))
import DbSync.Parser.Dispatch (parseBlock)
import DbSync.Parser.Types (CardanoPoint, GenericBlock (..))
import DbSync.Phase.Type (SyncPhase (..), renderPhase)
import DbSync.Db.Run (useConn)
import DbSync.Db.Statement.Core (queryLatestEpochNoStmt)
import DbSync.Db.Statement.SyncState (writeSyncStateSlotStmt)
import DbSync.Db.Statement.Transaction (beginSql, commitSql, rollbackSql)

import DbSync.App.Config.Types
  ( SyncConfig (..)
  , OptionFlag (..)
  , Extractors (..)
  , UtxoOption (..)
  )
import DbSync.App.Env (CoreEnv (..), FollowEnv (..), HasConfig (..), HasNetwork)
import DbSync.Extractor (ExtractorDef (..), cborCaptureEnabled, takeBlockLedgerData)
import DbSync.Extractor.EpochBoundary (runEpochBoundary)
import DbSync.Extractor.Governance (runGovernanceBoundary)
import DbSync.Extractor.PoolStats (runPoolStatsBoundary)
import DbSync.Extractor.StakeDelegationLedger (runStakeDelegationLedgerBoundary)
import DbSync.Extractor.Pipeline (processBlock)
import DbSync.ChainSync.Msg (ChainSyncMsg (..))
import DbSync.Phase.Following.IdAllocator (allocateAllIds)
import DbSync.Phase.Following.IdCounts (countAssignableIds)
import DbSync.Phase.Following.Resolver (ConsumedTracking (..), mkBufferedFollowResolver)
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
import DbSync.Db.Schema.EpochView (epochFinalizedTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.EpochView (appendEpochFinalizedStmt)
import DbSync.Error (AppError (..), BlockAnnotation (..))
import DbSync.Trace.Timing (fmtCount, fmtDuration, fmtF2)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- | Cadence for the periodic progress log in
-- 'FollowingVolatileTail'. 'FollowingChainTip' logs every applied
-- block instead.
logEveryNBlocks :: Word64
logEveryNBlocks = 100

-- | Longest quiet period before the loop emits a "still at tip"
-- heartbeat. Only 'FollowingChainTip' fires it; the windowed summary
-- covers visibility in 'FollowingVolatileTail'.
idleHeartbeatMicros :: Int
idleHeartbeatMicros = 30_000_000

-- | State carried across forward blocks. It drives the windowed log
-- cadence, the per-block delta at tip, the idle-heartbeat suffix, and
-- the per-epoch counters for the @epoch_sync_stats@ write.
data FollowProgress = FollowProgress
  { fpWindowStart      :: !UTCTime
    -- ^ When the current 'logEveryNBlocks' window opened.
  , fpBlocksThisWindow :: !Word64
    -- ^ Forward blocks applied since the window opened.
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

-- | What 'waitForMsgOrHeartbeat' woke up for.
data FollowWakeup
  = WakeMsg !ChainSyncMsg
  | WakeHeartbeat
  | WakeStop

-- | Drain the chainsync queue until @stopVar@ flips. One
-- @BEGIN@/@COMMIT@ envelope covers each 'MsgForward' and also advances
-- @dbsync_sync_state.last_committed_slot@, so a crash between blocks
-- never leaves rows past the recorded position. 'MsgRollback' runs
-- the cascade and moves the same columns to the target slot.
--
-- The loop stops on a flag, not a cancellation. An async exception
-- inside a hasql session sends hasql into a masked connection repair
-- that no canceller can interrupt.
run :: STM.TVar Bool -> FollowM ()
run stopVar = do
  FollowEnv{feCore, feBlockQueue, feReplayBootSlot, feHasqlConnection} <- ask
  let tracer   = ceTracer    feCore
      phaseRef = ceCurrentPhase feCore
  liftIO $ do
    component <- readPhaseComponent phaseRef
    traceWith tracer $ LogMsg Info component
      "consumer started; draining chainsync queue"
  startedAt <- liftIO getCurrentTime
  -- Seed the previous-epoch marker from PG so the first processed
  -- block can detect an epoch crossing; a crossing skipped here
  -- leaves its 'BoundaryApplyData' in the queue and shifts every
  -- later drain onto its predecessor's payload.
  mDbEpoch <- useConn "Phase.Following.Run" feHasqlConnection
    (Sess.statement () queryLatestEpochNoStmt)
  progressRef <- liftIO $ newIORef FollowProgress
    { fpWindowStart      = startedAt
    , fpBlocksThisWindow = 0
    , fpLastEpoch        = mDbEpoch
    , fpLastBlockAt      = Nothing
    , fpLastSlot         = Nothing
    , fpEpochStart       = startedAt
    , fpEpochBlocks      = 0
    }
  replayRef <- liftIO $ newIORef $ case feReplayBootSlot of
    Just _  -> ReplayPending
    Nothing -> NoReplay
  -- Monotonic delivery guard holding the last block number this
  -- consumer applied. The receiver delivers at-least-once across
  -- session boundaries, so a forward that does not advance is a
  -- re-delivery. It must go, not insert again: @block@ carries no
  -- unique constraint, and a @tx@ conflict aborts the app.
  -- 'processRollback' resets it, because post-rollback forwards
  -- legitimately carry lower block numbers.
  lastAppliedRef <- liftIO $ newIORef Nothing
  let loop = do
        wakeup <- liftIO $
          waitForMsgOrHeartbeat stopVar feBlockQueue phaseRef idleHeartbeatMicros
        case wakeup of
          WakeMsg (MsgForward blk) -> do
            redelivered <- liftIO $
              dropRedeliveredForward tracer phaseRef lastAppliedRef blk
            unless redelivered $
              processForward progressRef replayRef lastAppliedRef blk
            loop
          WakeMsg (MsgRollback point) -> do
            processRollback progressRef lastAppliedRef point
            loop
          WakeHeartbeat -> emitIdleHeartbeat progressRef >> loop
          WakeStop -> liftIO $ do
            component <- readPhaseComponent phaseRef
            traceWith tracer $ LogMsg Info component
              "shutdown requested; consumer loop exiting"
  loop

-- | 'True' when this forward block sits at or below the last applied
-- block number, which makes it a re-delivery. It logs at 'Warning':
-- atomic receiver-side delivery should never trigger this, so an
-- occurrence deserves attention even though the loop handles it.
dropRedeliveredForward
  :: AppTracer
  -> CurrentPhase
  -> IORef (Maybe BlockNo)
  -> CardanoBlock StandardCrypto
  -> IO Bool
dropRedeliveredForward tracer phaseRef lastAppliedRef blk = do
  mLastApplied <- readIORef lastAppliedRef
  case mLastApplied of
    Just lastBn | blockNo blk <= lastBn -> do
      component <- readPhaseComponent phaseRef
      traceWith tracer $ LogMsg Warning component
        ( "dropping re-delivered block " <> show (unBlockNo (blockNo blk))
            <> " (slot " <> show (unSlotNo (blockSlot blk))
            <> "); already applied up to block " <> show (unBlockNo lastBn)
        )
      pure True
    _ -> pure False

-- | Read the next message, or wake on the stop flag or the heartbeat
-- timer. The stop branch comes first, so a pending shutdown wins over
-- a backlogged queue. The timer branch only fires in
-- 'FollowingChainTip'; any other phase behaves like a plain
-- 'readTBQueue'.
waitForMsgOrHeartbeat
  :: STM.TVar Bool
  -> STM.TBQueue ChainSyncMsg
  -> CurrentPhase
  -> Int
  -> IO FollowWakeup
waitForMsgOrHeartbeat stopVar q phaseRef micros = do
  delayVar <- STM.registerDelay micros
  STM.atomically $
    stopBranch
      `STM.orElse` (WakeMsg <$> STM.readTBQueue q)
      `STM.orElse` heartbeatBranch delayVar
  where
    stopBranch = do
      stopping <- STM.readTVar stopVar
      unless stopping STM.retry
      pure WakeStop

    heartbeatBranch delayVar = do
      phase <- readCurrentPhaseSTM phaseRef
      when (phase /= FollowingChainTip) STM.retry
      expired <- STM.readTVar delayVar
      unless expired STM.retry
      pure WakeHeartbeat

readPhaseComponent :: CurrentPhase -> IO Text
readPhaseComponent = fmap renderPhase . readCurrentPhase

-- | Apply one forward block, in one of two regimes.
--
-- Inside the replay window — 'feReplayBootSlot' set and
-- @slot <= bootSlot@ — PG already holds the block and the ledger
-- worker re-applies it through the receiver fan-out. The consumer
-- only advances the replay state machine and the phase-flip
-- predicate.
--
-- Otherwise the block goes to PG in one transaction: count and
-- allocate the ids, run the extractors against a buffered resolver
-- and writer, then flush the buffer with the @last_committed_*@
-- UPDATE. Dedup resolves still hit PG synchronously, but a per-block
-- cache lets siblings find each other.
processForward
  :: IORef FollowProgress
  -> IORef ReplayLogState
  -> IORef (Maybe BlockNo)   -- ^ re-delivery guard; written after commit
  -> CardanoBlock StandardCrypto
  -> FollowM ()
processForward progressRef replayRef lastAppliedRef cardanoBlock = do
  env@FollowEnv
    { feCore
    , feStateQueryVar
    , feHasqlConnection
    , feHasLedgerEnv
    , feReplayBootSlot
    , feReplayStartSlot
    } <- ask
  let slot     = blockSlot cardanoBlock
      tracer   = getTracer env
      phaseRef = ceCurrentPhase feCore
  liftIO $ advanceAndLogReplay tracer replayRef feReplayStartSlot feReplayBootSlot slot
  case feReplayBootSlot of
    Just bootSlot | slot <= bootSlot -> do
      -- Skip the INSERT and the sync_state advance. The worker still
      -- enqueues a per-block ApplyResult for the replayed block, so
      -- drain and discard it to keep the queue empty.
      liftIO $ void $ takeBlockLedgerData feHasLedgerEnv
      maybeFlipToTip slot (blockNo cardanoBlock)
    _ -> do
      liftIO $ void $ atomically $ observeBlockSTM feStateQueryVar cardanoBlock
      sd <- getSlotDetails slot
      now <- liftIO getCurrentTime
      let !cborEnabled = cborCaptureEnabled (ceExtractors feCore)
          epochViewOn = any ((== tdName epochFinalizedTableDef) . tdName)
                            (concatMap pdTables (ceExtractors feCore))
          consumedTracking =
            if uoConsumedByTxId (exUtxo (scExtractors (getConfig env)))
              then TrackConsumedBy
              else SkipConsumedBy
          !genBlock = parseBlock cborEnabled sd cardanoBlock
          !curEpoch = unEpochNo (blkEpochNo genBlock)
          !counts   = countAssignableIds (ceExtractors feCore) genBlock
          triple    = ( unSlotNo  (blkSlotNo  genBlock)
                      , unBlockNo (blkBlockNo genBlock)
                      , blkHash   genBlock
                      )
          blockAnn  = BlockAnnotation
                        (unSlotNo  (blkSlotNo  genBlock))
                        (unBlockNo (blkBlockNo genBlock))
                        (blkHash   genBlock)
      snap <- liftIO $ readIORef progressRef
      let prevEpoch = fpLastEpoch snap
          boundaryCrossed = case prevEpoch of
            Just prev -> prev /= curEpoch
            Nothing   -> False
      liftIO $ Exception.annotateIO blockAnn $ do
        preAllocated <- allocateAllIds feHasqlConnection counts
        buf          <- newWriteBuffer
        resolver     <- mkBufferedFollowResolver feHasqlConnection preAllocated buf consumedTracking
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
        -- Finalise the completed epoch in the same transaction as the
        -- crossing block; the upsert makes a re-issued boundary
        -- harmless and a crash before COMMIT re-fires it on replay.
        let appendFinalized = case (boundaryCrossed, prevEpoch) of
              (True, Just prev) | epochViewOn ->
                Pipeline.statement prev appendEpochFinalizedStmt
              _ -> pure ()
            flushAndAdvance =
              writes *> appendFinalized
                     *> void (Pipeline.statement triple writeSyncStateSlotStmt)
        runFollowBlockTx feHasqlConnection flushAndAdvance
        -- Only advance the re-delivery guard once the block's PG
        -- transaction has committed; a crash before this point must
        -- leave the guard at the previous block.
        writeIORef lastAppliedRef (Just (blkBlockNo genBlock))
      maybeFlipToTip (blkSlotNo genBlock) (blkBlockNo genBlock)
      maybeLogProgress progressRef now genBlock

-- | Drain the ledger worker's next boundary 'ApplyResult' and run the
-- boundary extractors against it. Does nothing when the ledger
-- feature is off, or when no block has arrived yet.
--
-- Governance runs first: it publishes the enacted
-- @(committee, no_confidence, constitution)@ id triple onto the
-- resolver's scratchpad, and 'runEpochBoundary' reads it when it
-- builds the next @epoch_state@ row.
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
    governanceOn <- asks (prEnabled . exGovernance . scExtractors . getConfig)
    poolStatsOn  <- asks (prEnabled . exPoolStats  . scExtractors . getConfig)
    sdlOn        <- asks (prEnabled . exStakeDelegationLedger . scExtractors . getConfig)
    for_ mBlockId $ \blockId -> do
      when governanceOn $ runGovernanceBoundary applyResult blockId
      runEpochBoundary applyResult blockId
      when poolStatsOn  $ runPoolStatsBoundary governanceOn applyResult blockId
      when sdlOn        $ runStakeDelegationLedgerBoundary applyResult blockId

-- | Step the replay-progress state machine and emit any trace it
-- indicates. Outside the replay window the machine is 'NoReplay' and
-- this does nothing. Inside, it emits periodic progress lines, then
-- one completion line on the first block past the window.
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
        traceWith tracer $ LogMsg Info "LedgerReplay" msg
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

-- | Wrap one block's pipelined writes in a @BEGIN@/@COMMIT@
-- envelope. A flush failure runs a best-effort ROLLBACK so the open
-- transaction does not leak, and the original exception still
-- propagates.
runFollowBlockTx :: Conn.Connection -> Pipeline.Pipeline () -> IO ()
runFollowBlockTx conn flush = do
  useConn "Following.BEGIN" conn (Sess.script beginSql)
  useConn "Following.flush" conn (Sess.pipeline flush)
    `onException` rollbackQuiet conn
  useConn "Following.COMMIT" conn (Sess.script commitSql)

-- | Swallows its own errors, so a failed rollback cannot mask the
-- original exception.
rollbackQuiet :: Conn.Connection -> IO ()
rollbackQuiet conn =
  void (Conn.use conn (Sess.script rollbackSql))
    `catch` \(_ :: SomeException) -> pure ()

-- | Steady-state lag at chain tip: the receiver stages the next
-- block while the consumer applies the current one.
tipFollowMargin :: Word64
tipFollowMargin = 1

-- | Pure predicate for the 'FollowingVolatileTail' ->
-- 'FollowingChainTip' transition, split out of 'maybeFlipToTip' so a
-- unit test can drive it.
--
-- A 'Nothing' tip means the receiver saw no server tip yet, so there
-- is nothing to flip against.
shouldFlipToTip
  :: Bool          -- ^ inside the replay window
  -> SyncPhase     -- ^ current phase
  -> Maybe BlockNo -- ^ server tip block number
  -> BlockNo       -- ^ just-applied block number
  -> Bool
shouldFlipToTip inReplay phase mTip (BlockNo applied) =
  not inReplay
    && phase == FollowingVolatileTail
    && case mTip of
         Just (BlockNo t) -> applied + tipFollowMargin >= t
         Nothing          -> False

-- | Flip the phase once 'shouldFlipToTip' agrees. The flip is
-- one-way: only a later 'MsgRollback' goes back.
--
-- The replay window suppresses it, because the skip-only consumer
-- outpaces the receiver and the predicate would fire wrongly.
maybeFlipToTip :: SlotNo -> BlockNo -> FollowM ()
maybeFlipToTip appliedSlot appliedBlock = do
  FollowEnv{feCore, feLatestTipBlock, feReplayBootSlot} <- ask
  let phaseRef = ceCurrentPhase feCore
      inReplay = maybe False (appliedSlot <=) feReplayBootSlot
  phase <- liftIO $ readCurrentPhase phaseRef
  mTip  <- liftIO $ STM.readTVarIO feLatestTipBlock
  when (shouldFlipToTip inReplay phase mTip appliedBlock) $
    setCurrentPhase phaseRef FollowingChainTip

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

-- | Update the progress counters for this block, then emit one Info
-- line per applied block in 'FollowingChainTip', or the windowed
-- summary in any other phase. The window closes after
-- 'logEveryNBlocks' blocks or on an epoch crossing.
--
-- This also rolls the per-epoch counters forward for the next
-- @epoch_sync_stats@ row, resetting them at each crossing.
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
          ])
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
          traceWith tracer $ LogMsg Info component msg

-- | Render the "+T since prev" suffix for the per-block
-- 'FollowingChainTip' log. The first block has no previous time, so
-- the suffix is empty.
renderSinceLast :: UTCTime -> Maybe UTCTime -> Text
renderSinceLast now = \case
  Nothing -> ""
  Just t  -> " (+" <> fmtDuration (realToFrac (diffUTCTime now t)) <> " since prev)"

-- | Emit the idle "still at tip" heartbeat.
-- 'waitForMsgOrHeartbeat' gates on phase, so only
-- 'FollowingChainTip' reaches this.
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
    traceWith tracer $ LogMsg Info component body

-- | Compact rate formatter. Low rates keep more precision, so a slow
-- sync does not read as "0 blk/s".
fmtRate :: Double -> Text
fmtRate r
  | r < 10    = toS (showFFloat (Just 2) r "")
  | r < 1000  = toS (showFFloat (Just 1) r "")
  | otherwise = toS (showFFloat (Just 0) r "")

-- | Apply one rollback marker. The phase drops back to
-- 'FollowingVolatileTail' before the cascade, so the log shows the
-- run catching up again.
processRollback
  :: IORef FollowProgress
  -> IORef (Maybe BlockNo)   -- ^ re-delivery guard; reset by the rollback
  -> CardanoPoint
  -> FollowM ()
processRollback progressRef lastAppliedRef point = do
  FollowEnv{feCore, feHasqlConnection} <- ask
  let tracer    = ceTracer    feCore
      phaseRef  = ceCurrentPhase feCore
      tableDefs = concatMap pdTables (ceExtractors feCore)
  setCurrentPhase phaseRef FollowingVolatileTail
  liftIO $ do
    component <- readPhaseComponent phaseRef
    traceWith tracer $ LogMsg Info component
      ("rollback to " <> show point)
  rollbackWithRetry tableDefs point
  -- The fork's replacement blocks arrive with block numbers at or
  -- below the last applied one; disarm the re-delivery guard until
  -- the next forward block re-seeds it.
  liftIO $ writeIORef lastAppliedRef Nothing
  -- The cascade can delete past an epoch boundary. Re-seed the
  -- previous-epoch marker from PG, so the next forward block neither
  -- fires a spurious boundary drain, which would block on an empty
  -- queue, nor misses the real re-crossing.
  mDbEpoch <- useConn "Phase.Following.Run" feHasqlConnection
    (Sess.statement () queryLatestEpochNoStmt)
  liftIO $ modifyIORef' progressRef $ \p -> p { fpLastEpoch = mDbEpoch }

-- | Total attempts (initial + retries) before a rollback failure
-- surfaces. Reads are idempotent and writes run in one PG
-- transaction, so a mid-flight failure commits nothing.
rollbackMaxAttempts :: Int
rollbackMaxAttempts = 3

-- | Wait before the first retry (microseconds); doubles each retry.
rollbackBaseDelayMicros :: Int
rollbackBaseDelayMicros = 250_000

-- | Run the rollback cascade, and retry on 'AppDatabaseError'. A
-- transient PG glitch, such as EINTR surfaced as
-- @XX000 internal_error@, then rides out without a crash.
rollbackWithRetry :: [TableDef] -> CardanoPoint -> FollowM ()
rollbackWithRetry tableDefs point = go 1
  where
    go attempt = do
      tracer <- asks getTracer
      outcome <- withRunInIO $ \runInIO ->
        (Right <$> runInIO (Rollback.rollbackToPoint tableDefs point))
          `Exception.catchNoPropagate`
            \(ewc :: Exception.ExceptionWithContext AppError) -> pure (Left ewc)
      case outcome of
        Right () -> pure ()
        Left ewc@(Exception.ExceptionWithContext _ appErr)
          | AppDatabaseError _ msg <- appErr
          , attempt < rollbackMaxAttempts -> do
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
                  ])
                threadDelay delayMicros
              go (attempt + 1)
          -- Retries exhausted, or a non-database error: rethrow with the
          -- original context (backtrace) intact.
          | otherwise -> liftIO (Exception.rethrowIO ewc)
