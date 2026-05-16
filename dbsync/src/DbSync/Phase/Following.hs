{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The Follow loop: per-block INSERT against PG with rollback
-- support. Drives both the 'FollowingVolatileTail' and
-- 'FollowingChainTip' phases; the only behavioural difference is
-- the phase tag itself, which flips between the two as the consumer
-- catches up with or falls behind the receiver.
--
-- The loop reads one 'ChainSyncMsg' at a time from 'feBlockQueue'
-- and either applies a forward block in its own PG transaction, or
-- runs the rollback cascade for a 'MsgRollback' marker.
module DbSync.Phase.Following
  ( run
  ) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Control.Concurrent.STM (TBQueue, readTBQueue)
import qualified Control.Concurrent.STM as STM
import Control.Tracer (traceWith)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Numeric (showFFloat)
import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import Ouroboros.Consensus.Block (blockSlot)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Network.Block (pattern BlockPoint)

import DbSync.AppM (FollowM, runAppM)
import DbSync.Block.Parser (parseBlock)
import DbSync.Block.Types (CardanoPoint, GenericBlock (..))
import DbSync.Db.Phase (SyncPhase (..), renderSyncPhase)
import DbSync.Db.Statement.SyncState (writeSyncStateSlotStmt)
import DbSync.Db.Transaction (withTransaction)
import DbSync.Env (CoreEnv (..), FollowEnv (..))
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Ingest.Pipeline (processBlock)
import DbSync.Node.ChainSyncMsg (ChainSyncMsg (..))
import qualified DbSync.Phase.Following.Rollback as Rollback
import DbSync.Phase.Ref (SyncPhaseRef, readSyncPhase, setSyncPhase)
import DbSync.StateQuery (getSlotDetailsIO, observeBlockSTM)
import DbSync.Trace.Timing (fmtDuration)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))
import DbSync.Trace.Watchdog (bumpConsumer, setConsumerNote)

-- | Cadence for the periodic Follow-loop progress log.
logEveryNBlocks :: Word64
logEveryNBlocks = 100

-- | How many of the first forward blocks each consumer session
-- should log at 'Info'. Bridges the gap between the receiver's
-- "intersect confirmed" log and the first 'logEveryNBlocks' summary,
-- which is otherwise a multi-minute silence on a slow Follow.
firstBlocksToHeartbeat :: Int
firstBlocksToHeartbeat = 5

-- | State carried across forward blocks so we can emit a single
-- summary every 'logEveryNBlocks' blocks or whenever a new epoch
-- crosses, whichever comes first.
data FollowProgress = FollowProgress
  { fpWindowStart      :: !UTCTime
    -- ^ When the current window opened — the diff against @now@ is
    -- the elapsed wall-clock used for the rate column.
  , fpBlocksThisWindow :: !Word64
  , fpLastEpoch        :: !(Maybe Word64)
    -- ^ 'Nothing' before the first block lands; afterwards holds
    -- the most recent block's epoch so we can detect a crossing.
  , fpHeartbeatsLeft   :: !Int
    -- ^ Counts down from 'firstBlocksToHeartbeat'; while positive
    -- the loop logs one Info line per applied block so the operator
    -- sees the consumer is processing before the first windowed
    -- summary fires.
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
      phaseRef = ceSyncPhase feCore
  liftIO $ do
    component <- readPhaseComponent phaseRef
    traceWith tracer $ LogMsg Info component
      "consumer started; draining chainsync queue" Nothing
  startedAt <- liftIO getCurrentTime
  progressRef <- liftIO $ newIORef FollowProgress
    { fpWindowStart      = startedAt
    , fpBlocksThisWindow = 0
    , fpLastEpoch        = Nothing
    , fpHeartbeatsLeft   = firstBlocksToHeartbeat
    }
  forever $ do
    msg <- liftIO $ atomically $ readTBQueue feBlockQueue
    case msg of
      MsgForward  blk   -> processForward progressRef blk
      MsgRollback point -> processRollback point

-- | Render the current phase as the log-component string. Always
-- reflects whether we are catching up or steady-state, so a reader
-- can tell at a glance.
readPhaseComponent :: SyncPhaseRef -> IO Text
readPhaseComponent = fmap renderSyncPhase . readSyncPhase

-- | Apply one forward block.
--
-- The block parse and extractor pipeline run against the same hasql
-- connection as the sync-state advance; @BEGIN@/@COMMIT@ are wrapped
-- around both so the row writes and the @last_committed_*@ update
-- commit atomically.
processForward :: IORef FollowProgress -> CardanoBlock StandardCrypto -> FollowM ()
processForward progressRef cardanoBlock = do
  env@FollowEnv
    { feCore
    , feWatchdog
    , feStateQueryVar
    , feHasqlConnection
    , feSystemStart
    , feBlockQueue
    , feLatestReceivedPoint
    } <- ask
  let tracer   = ceTracer    feCore
      phaseRef = ceSyncPhase feCore
      slot     = blockSlot cardanoBlock
  liftIO $ do
    setConsumerNote feWatchdog "follow: processForward"
    bumpConsumer feWatchdog slot
    _ <- atomically $ observeBlockSTM feStateQueryVar cardanoBlock
    sd <- getSlotDetailsIO tracer feStateQueryVar feSystemStart slot
    let !genBlock = parseBlock sd cardanoBlock
    withTransaction feHasqlConnection $ do
      runAppM env (processBlock genBlock)
      let triple =
            ( unSlotNo  (blkSlotNo  genBlock)
            , unBlockNo (blkBlockNo genBlock)
            , blkHash   genBlock
            )
      sessR <- Conn.use feHasqlConnection (Sess.statement triple writeSyncStateSlotStmt)
      case sessR of
        Right _ -> pure ()
        Left e  ->
          panic $ "Following: writeSyncStateSlotStmt: " <> show e
    maybeFlipToTip tracer phaseRef feLatestReceivedPoint feBlockQueue (blkSlotNo genBlock)
    maybeLogProgress tracer phaseRef progressRef feBlockQueue genBlock

-- | Flip the phase from 'FollowingVolatileTail' to
-- 'FollowingChainTip' once the consumer has caught the receiver's
-- latest received slot and the block queue is empty. One-way:
-- a subsequent 'MsgRollback' is the only path back.
maybeFlipToTip
  :: AppTracer
  -> SyncPhaseRef
  -> IORef (Maybe CardanoPoint)
  -> TBQueue ChainSyncMsg
  -> SlotNo
  -> IO ()
maybeFlipToTip tracer phaseRef latestRef queue appliedSlot = do
  phase <- readSyncPhase phaseRef
  when (phase == FollowingVolatileTail) $ do
    qLen <- atomically (STM.lengthTBQueue queue)
    when (qLen == 0) $ do
      mLatest <- readIORef latestRef
      case mLatest of
        Just (BlockPoint latestSlot _)
          | appliedSlot >= latestSlot ->
              setSyncPhase tracer phaseRef FollowingChainTip
        _ -> pure ()

-- | Update the progress counter for this block, then emit either:
--
--   * a per-block heartbeat for the first 'firstBlocksToHeartbeat'
--     forward blocks of the session (consumer-side "I'm alive"
--     log); or
--   * the windowed summary when 'logEveryNBlocks' blocks have been
--     applied or a new epoch has crossed.
--
-- Both fire on the same block only when the window cadence happens
-- to coincide with the heartbeat window — uncommon and harmless.
maybeLogProgress
  :: AppTracer
  -> SyncPhaseRef
  -> IORef FollowProgress
  -> TBQueue ChainSyncMsg
  -> GenericBlock
  -> IO ()
maybeLogProgress tracer phaseRef progressRef queue gb = do
  now <- getCurrentTime
  let !curSlot  = unSlotNo  (blkSlotNo  gb)
      !curEpoch = unEpochNo (blkEpochNo gb)
      !curBlock = unBlockNo (blkBlockNo gb)
  (mHeartbeat, mDue) <- atomicModifyIORef' progressRef $ \p ->
    let !window'      = fpBlocksThisWindow p + 1
        !epochCrossed = case fpLastEpoch p of
                          Just prev -> curEpoch /= prev
                          Nothing   -> False
        !cadenceHit   = window' >= logEveryNBlocks
        !shouldLog    = epochCrossed || cadenceHit
        !hbLeft       = fpHeartbeatsLeft p
        !hbFires      = hbLeft > 0
        !p' = (if shouldLog
                 then p { fpWindowStart      = now
                        , fpBlocksThisWindow = 0
                        }
                 else p { fpBlocksThisWindow = window' }
              ) { fpLastEpoch      = Just curEpoch
                , fpHeartbeatsLeft = max 0 (hbLeft - 1)
                }
        !info = (fpWindowStart p, window')
    in (p', ( if hbFires then Just () else Nothing
            , if shouldLog then Just info else Nothing
            ))
  component <- readPhaseComponent phaseRef
  for_ mHeartbeat $ \() ->
    traceWith tracer $ LogMsg Info component (mconcat
      [ "applied block ", show curBlock
      , " (slot ", show curSlot
      , ", epoch ", show curEpoch, ")"
      ]) Nothing
  for_ mDue $ \(windowStart, blocks) -> do
    qLen <- atomically $ STM.lengthTBQueue queue
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
  FollowEnv{feCore, feWatchdog, feHasqlConnection} <- ask
  let tracer    = ceTracer    feCore
      phaseRef  = ceSyncPhase feCore
      tableDefs = concatMap pdTables (ceExtractors feCore)
  liftIO $ do
    setConsumerNote feWatchdog "follow: processRollback"
    setSyncPhase tracer phaseRef FollowingVolatileTail
    component <- readPhaseComponent phaseRef
    traceWith tracer $ LogMsg Info component
      ("rollback to " <> show point) Nothing
    Rollback.rollbackToPoint tableDefs feHasqlConnection point
