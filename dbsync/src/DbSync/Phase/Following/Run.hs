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
import DbSync.Block.Parser (parseBlock)
import DbSync.Block.Types (CardanoPoint, GenericBlock (..))
import DbSync.Phase.Type (SyncPhase (..), renderPhase)
import DbSync.Db.Statement.SyncState (writeSyncStateSlotStmt)
import DbSync.Db.Statement.Transaction (beginSql, commitSql, rollbackSql)

import DbSync.Env (CoreEnv (..), FollowEnv (..))
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Block.Pipeline (processBlock)
import DbSync.Node.ChainSyncMsg (ChainSyncMsg (..))
import DbSync.Phase.Following.IdAllocator (allocateAllIds)
import DbSync.Phase.Following.IdCounts (countAssignableIds)
import DbSync.Phase.Following.Resolver (mkBufferedFollowResolver)
import qualified DbSync.Phase.Following.Rollback as Rollback
import DbSync.Phase.Following.WriteBuffer (drain, newWriteBuffer)
import DbSync.Phase.Following.Writer (mkBufferedWriter)
import DbSync.Phase.Current
  ( CurrentPhase
  , readCurrentPhase
  , readCurrentPhaseSTM
  , setCurrentPhase
  )
import DbSync.StateQuery (getSlotDetails, observeBlockSTM)
import DbSync.Trace.Timing (fmtDuration)
import DbSync.Trace.Types (LogMsg (..), Severity (..))
import DbSync.Trace.Watchdog (bumpConsumer, setConsumerNote)

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
-- cadence in 'FollowingVolatileTail', the per-block delta in
-- 'FollowingChainTip', and the "N ago" suffix on the idle heartbeat.
data FollowProgress = FollowProgress
  { fpWindowStart      :: !UTCTime
    -- ^ When the current 'logEveryNBlocks' window opened.
  , fpBlocksThisWindow :: !Word64
  , fpLastEpoch        :: !(Maybe Word64)
    -- ^ 'Nothing' before the first block lands.
  , fpLastBlockAt      :: !(Maybe UTCTime)
    -- ^ When the most recent block finished 'processForward'.
    -- Drives the per-block delta and the idle-heartbeat "N ago" suffix.
  , fpLastSlot         :: !(Maybe Word64)
    -- ^ Slot of the most recent applied block, surfaced in the idle
    -- heartbeat so the chain pointer is visible even between blocks.
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
  FollowEnv{feCore, feBlockQueue} <- ask
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
    }
  forever $ do
    mMsg <- liftIO $
      waitForMsgOrHeartbeat feBlockQueue phaseRef idleHeartbeatMicros
    case mMsg of
      Just (MsgForward  blk)   -> processForward progressRef blk
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

-- | Apply one forward block inside one PG transaction.
--
--   1. Count the IDs the extractors will need ('countAssignableIds')
--      and allocate them in a single libpq pipeline
--      ('allocateAllIds').
--   2. Run extractors with a buffered resolver + writer. Dedup
--      resolves still hit PG synchronously (one SELECT plus a
--      possible @nextval@ on miss) but consult a per-block dedup
--      cache so siblings find each other. INSERTs land on a single
--      'WriteBuffer'.
--   3. BEGIN, pipeline-flush the writes plus the @last_committed_*@
--      UPDATE, COMMIT. The three Sessions are inlined here so the
--      'onException' rolls back cleanly without masking the original
--      exception.
processForward :: IORef FollowProgress -> CardanoBlock StandardCrypto -> FollowM ()
processForward progressRef cardanoBlock = do
  env@FollowEnv
    { feWatchdog
    , feStateQueryVar
    , feHasqlConnection
    } <- ask
  let slot = blockSlot cardanoBlock
  liftIO $ setConsumerNote feWatchdog "follow: processForward"
  liftIO $ bumpConsumer feWatchdog slot
  liftIO $ void $ atomically $ observeBlockSTM feStateQueryVar cardanoBlock
  sd <- getSlotDetails slot
  let !genBlock = parseBlock sd cardanoBlock
      !counts   = countAssignableIds genBlock
      triple    = ( unSlotNo  (blkSlotNo  genBlock)
                  , unBlockNo (blkBlockNo genBlock)
                  , blkHash   genBlock
                  )
  liftIO $ do
    preAllocated <- allocateAllIds feHasqlConnection counts
    buf          <- newWriteBuffer
    resolver     <- mkBufferedFollowResolver feHasqlConnection preAllocated buf
    let writer      = mkBufferedWriter buf
        bufferedEnv = env { feResolver = resolver, feWriter = writer }
    runAppM bufferedEnv (processBlock genBlock)
    writes <- drain buf
    let flushAndAdvance =
          writes *> void (Pipeline.statement triple writeSyncStateSlotStmt)
    setConsumerNote feWatchdog "follow: BEGIN"
    runSession feHasqlConnection (Sess.script beginSql) "BEGIN"
    setConsumerNote feWatchdog "follow: flush pipeline"
    let runFlush = runSession feHasqlConnection
          (Sess.pipeline flushAndAdvance) "flush"
    runFlush `onException` rollbackQuiet feHasqlConnection
    setConsumerNote feWatchdog "follow: COMMIT"
    runSession feHasqlConnection (Sess.script commitSql) "COMMIT"
  maybeFlipToTip (blkSlotNo genBlock)
  maybeLogProgress progressRef genBlock

-- | Run a hasql 'Session' against the supplied connection, panicking
-- with a labelled message on failure. Used by 'processForward' to
-- inline the BEGIN/flush/COMMIT segments with separate timing while
-- preserving the exception semantics of 'withTransactionOn'.
runSession :: Conn.Connection -> Sess.Session () -> Text -> IO ()
runSession conn sess label = do
  r <- Conn.use conn sess
  case r of
    Right () -> pure ()
    Left e   -> panic $ "Following: " <> label <> ": " <> show e

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
maybeFlipToTip :: SlotNo -> FollowM ()
maybeFlipToTip appliedSlot = do
  FollowEnv{feCore, feBlockQueue, feLatestReceivedPoint} <- ask
  let phaseRef = ceCurrentPhase feCore
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

-- | Update the progress counter for this block, then emit either:
--
--   * one Info line per applied block when in 'FollowingChainTip' —
--     at mainnet's ~20 s/block cadence this is roughly one log per
--     20 s, the natural "I'm alive" rhythm at tip; or
--   * the windowed summary when 'logEveryNBlocks' blocks have been
--     applied or a new epoch has crossed (other phases). Per-block
--     spam isn't useful while still catching up.
maybeLogProgress :: IORef FollowProgress -> GenericBlock -> FollowM ()
maybeLogProgress progressRef gb = do
  FollowEnv{feCore, feBlockQueue} <- ask
  let tracer   = ceTracer    feCore
      phaseRef = ceCurrentPhase feCore
  liftIO $ do
    now <- getCurrentTime
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
  Rollback.rollbackToPoint tableDefs point
