{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | FollowingChainTip phase: per-block INSERT against PG with
-- rollback support.
--
-- The loop reads one 'ChainSyncMsg' at a time from 'feBlockQueue'
-- and either applies a forward block in its own PG transaction, or
-- runs the rollback cascade for a 'MsgRollback' marker.
module DbSync.Phase.FollowingChainTip
  ( run
  ) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.STM (readTBQueue)
import Control.Tracer (traceWith)
import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import Ouroboros.Consensus.Block (blockSlot)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)

import DbSync.AppM (FollowM, runAppM)
import DbSync.Block.Parser (parseBlock)
import DbSync.Block.Types (CardanoPoint, GenericBlock (..))
import DbSync.Db.Statement.SyncState (writeSyncStateSlotStmt)
import DbSync.Db.Transaction (withTransaction)
import DbSync.Env (CoreEnv (..), FollowEnv (..))
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Ingest.Pipeline (processBlock)
import DbSync.Node.ChainSyncMsg (ChainSyncMsg (..))
import qualified DbSync.Phase.FollowingChainTip.Rollback as Rollback
import DbSync.StateQuery (getSlotDetailsIO, observeBlockSTM)
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (LogMsg (..), Severity (..))
import DbSync.Trace.Watchdog (bumpConsumer, setConsumerNote)

-- | Drain the chainsync queue forever.
--
-- Each 'MsgForward' is parsed, extracted, and applied to PG inside a
-- single @BEGIN@/@COMMIT@ envelope that also advances
-- @dbsync_sync_state.last_committed_slot@ — so a crash between blocks
-- never leaves rows in PG past the recorded position. 'MsgRollback'
-- runs the cascade and updates the same sync-state columns to the
-- target slot.
run :: FollowM ()
run = forever $ do
  queue <- asks feBlockQueue
  msg   <- liftIO $ atomically $ readTBQueue queue
  case msg of
    MsgForward  blk   -> processForward blk
    MsgRollback point -> processRollback point

-- | Apply one forward block.
--
-- The block parse and extractor pipeline run against the same hasql
-- connection as the sync-state advance; @BEGIN@/@COMMIT@ are wrapped
-- around both so the row writes and the @last_committed_*@ update
-- commit atomically.
processForward :: CardanoBlock StandardCrypto -> FollowM ()
processForward cardanoBlock = do
  env       <- ask
  watchdog  <- asks feWatchdog
  sqv       <- asks feStateQueryVar
  hasqlConn <- asks feHasqlConnection
  tracer    <- asks getTracer
  systemStart <- asks feSystemStart
  let slot = blockSlot cardanoBlock
  liftIO $ do
    setConsumerNote watchdog "follow: processForward"
    bumpConsumer watchdog slot
    _ <- atomically $ observeBlockSTM sqv cardanoBlock
    sd <- getSlotDetailsIO tracer sqv systemStart slot
    let !genBlock = parseBlock sd cardanoBlock
    withTransaction hasqlConn $ do
      runAppM env (processBlock genBlock)
      let triple =
            ( unSlotNo  (blkSlotNo  genBlock)
            , unBlockNo (blkBlockNo genBlock)
            , blkHash   genBlock
            )
      sessR <- Conn.use hasqlConn (Sess.statement triple writeSyncStateSlotStmt)
      case sessR of
        Right _ -> pure ()
        Left e  ->
          panic $ "FollowingChainTip: writeSyncStateSlotStmt: " <> show e

-- | Apply one rollback marker.
--
-- Delegated to the rollback cascade, which DELETEs every row past
-- the target block and advances @last_committed_*@ to match, all in
-- one PG transaction.
processRollback :: CardanoPoint -> FollowM ()
processRollback point = do
  tracer     <- asks getTracer
  watchdog   <- asks feWatchdog
  hasqlConn  <- asks feHasqlConnection
  extractors <- asks (ceExtractors . feCore)
  let tableDefs = concatMap pdTables extractors
  liftIO $ do
    setConsumerNote watchdog "follow: processRollback"
    traceWith tracer $ LogMsg Info "FollowingChainTip"
      ("rollback to " <> show point) Nothing
    Rollback.rollbackToPoint tableDefs hasqlConn point
