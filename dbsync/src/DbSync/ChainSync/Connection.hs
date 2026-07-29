{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | ChainSync node connection.
--
-- Connects to a cardano-node via Unix socket, runs the ChainSync
-- mini-protocol, and pushes received blocks to the 'IngestEnv'\'s
-- block queue.
module DbSync.ChainSync.Connection
  ( -- * Types
    IntersectionRequirement (..)

    -- * Running
  , connectToNode
  , getNetworkMagic
  , networkNameFromMagic

    -- * Block delivery
    --
    -- Exported for the delivery-atomicity tests; production code
    -- reaches them only through 'blockFetchClient'.
  , deliverForwardBlock
  , deliverRollback
  ) where

import Cardano.Prelude hiding ((%), Nat)

import Cardano.Client.Subscription
  ( Decision (..)
  , SubscriptionParams (..)
  , SubscriptionTrace (..)
  , SubscriptionTracers (..)
  , subscribe
  )
import Control.Concurrent.Async (AsyncCancelled (..))
import Control.Concurrent.STM (TBQueue, TVar, readTVarIO, writeTBQueue, writeTVar)
import Control.Tracer (contramap, nullTracer, traceWith)
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import System.IO.Error (IOError, ioeGetErrorType, isDoesNotExistErrorType)
import qualified Network.Mux as Mux
import Network.TypedProtocol.Peer (Nat (..))

import Cardano.Ledger.BaseTypes (unNonZero)
import Ouroboros.Consensus.Block.Abstract (CodecConfig)
import Ouroboros.Consensus.Byron.Node ()
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Config (TopLevelConfig, configCodec, configSecurityParam)
import Ouroboros.Consensus.Protocol.Abstract (maxRollbacks)
import Ouroboros.Consensus.Cardano.Block
  ( CardanoBlock
  , StandardCrypto
  )
import Ouroboros.Consensus.Network.NodeToClient
  ( Codecs' (..)
  , clientCodecs
  )
import Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( BlockNodeToClientVersion
  , supportedNodeToClientVersions
  )
import Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import Ouroboros.Network.Block
  ( BlockNo (..)
  , Point
  , Tip (..)
  , blockNo
  , blockPoint
  , blockSlot
  , genesisPoint
  , getTipBlockNo
  )
import Ouroboros.Network.Driver.Simple (runPipelinedPeer)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux
  ( MiniProtocolCb (..)
  , RunMiniProtocol (..)
  , RunMiniProtocolWithMinimalCtx
  )
import qualified Ouroboros.Network.Mux as Mux
import Cardano.Network.NodeToClient
  ( IOManager
  , LocalAddress
  , NodeToClientProtocols (..)
  , localSnocket
  , localTxMonitorPeerNull
  , localTxSubmissionPeerNull
  )
import Ouroboros.Network.Protocol.LocalStateQuery.Client (localStateQueryClientPeer)
import qualified Cardano.Network.NodeToClient.Version as Network
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
  ( ChainSyncClientPipelined (..)
  , ClientPipelinedStIdle (..)
  , ClientPipelinedStIntersect (..)
  , ClientStNext (..)
  , chainSyncClientPeerPipelined
  )
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision
  ( MkPipelineDecision
  , PipelineDecision (..)
  , pipelineDecisionLowHighMark
  , runPipelineDecision
  )
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import qualified Ouroboros.Network.Snocket as Snocket

import Cardano.Slotting.Slot (WithOrigin (..))

import DbSync.Parser.Types (CardanoPoint)
import DbSync.App.Config.Genesis (GenesisConfig (..), ShelleyConfig (..))
import DbSync.App.Env (HasReceiverChannels (..))
import DbSync.Error (throwNetwork)
import DbSync.ChainSync.Msg (ChainSyncMsg (..))
import DbSync.StateQuery (StateQueryVar, localStateQueryHandler)
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | What the chainsync receiver should do at startup.
data IntersectionRequirement
  = IntersectGenesis
    -- ^ Fresh start. The receiver requests intersection at genesis;
    -- if the node also has nothing, it follows from origin.
  | IntersectAt ![CardanoPoint]
    -- ^ Resume past a previous run. The receiver offers a list of
    -- candidate points (newest-first) to the node, which picks
    -- whichever is on its chain. The list is non-empty by
    -- construction; an empty list would degenerate to genesis-only,
    -- which the boot flow signals via 'IntersectGenesis' instead.
    --
    -- If the node can't intersect at /any/ candidate, the connection
    -- fails fatally — the node's chain has diverged from every
    -- snapshot we know about.

-- ---------------------------------------------------------------------------
-- * Network magic
-- ---------------------------------------------------------------------------

-- | Extract the 'NetworkMagic' from a 'GenesisConfig'.
-- Comes from the Shelley genesis 'sgNetworkMagic' field.
getNetworkMagic :: GenesisConfig -> NetworkMagic
getNetworkMagic gc = NetworkMagic $ sgNetworkMagic (scConfig $ gcShelley gc)

-- | Human-readable label for the well-known network magics; any other
-- magic renders as @magic-\<N\>@. The magic is the identity — the name
-- exists for log lines and the @dbsync_sync_state.network_name@ column.
networkNameFromMagic :: NetworkMagic -> Text
networkNameFromMagic (NetworkMagic magic) = case magic of
  764824073 -> "mainnet"
  1         -> "preprod"
  2         -> "preview"
  4         -> "sanchonet"
  n         -> "magic-" <> show n

-- ---------------------------------------------------------------------------
-- * Connection
-- ---------------------------------------------------------------------------

-- | Connect to a cardano-node and run the ChainSync protocol.
--
-- Received blocks are written to the env's block queue. Blocks
-- indefinitely (reconnects on failure).
--
-- Polymorphic over the env so the same call drives Ingest and Follow.
-- Both 'IngestEnv' and 'FollowEnv' have 'HasTracer' and
-- 'HasReceiverChannels' instances, so the consumer-side env at each
-- phase boundary is the only thing that changes.
connectToNode
  :: ( MonadReader env m, MonadIO m
     , HasTracer env, HasReceiverChannels env
     )
  => IOManager
  -> TopLevelConfig (CardanoBlock StandardCrypto)
  -> NetworkMagic
  -> FilePath                                    -- ^ Node socket path
  -> IntersectionRequirement
  -> m ()
connectToNode iomgr topLevelCfg networkMagic socketPath intersect = do
  tracer            <- asks getTracer
  blockQueue        <- asks getBlockQueue
  mLedgerQueue      <- asks getLedgerQueue
  stateQueryVar     <- asks getStateQueryVar
  latestPointRef    <- asks getLatestPoint
  rollbackBoundary  <- asks getRollbackBoundary
  latestTipBlock    <- asks getLatestTipBlock
  liftIO $ do
    traceWith tracer $ LogMsg Info "Connection" ("Connecting to node via " <> toS socketPath)
    void $
      subscribe
        (localSnocket iomgr)
        networkMagic
        (supportedNodeToClientVersions (Proxy @(CardanoBlock StandardCrypto)))
        (subscriptionTracers tracer)
        subscriptionParams
        (nodeProtocols tracer codecConfig blockQueue mLedgerQueue stateQueryVar latestPointRef rollbackBoundary latestTipBlock kBlocks intersect)
  where
    codecConfig :: CodecConfig (CardanoBlock StandardCrypto)
    codecConfig = configCodec topLevelCfg

    -- Protocol security parameter — the largest possible rollback
    -- depth. Cardano mainnet has k = 2160; testnets vary.
    kBlocks :: Word64
    kBlocks = unNonZero (maxRollbacks (configSecurityParam topLevelCfg))

    subscriptionParams :: SubscriptionParams ()
    subscriptionParams =
      SubscriptionParams
        { spAddress = Snocket.localAddressFromPath socketPath
        , spReconnectionDelay = Nothing
        , spCompleteCb = \case
            Left e ->
              case fromException e of
                Just AsyncCancelled -> Abort
                _other -> Reconnect
            Right _ -> Reconnect
        }

    -- Wire up subscription tracer to see connection/disconnection events.
    -- The other tracers stay null (mux-level detail is too noisy).
    subscriptionTracers :: AppTracer -> SubscriptionTracers ()
    subscriptionTracers tracer =
      SubscriptionTracers
        { stMuxTracer = nullTracer
        , stHandshakeTracer = nullTracer
        , stSubscriptionTracer = contramap formatSubscriptionTrace tracer
        , stMuxChannelTracer = nullTracer
        , stMuxBearerTracer = nullTracer
        }

-- | Map subscription events to appropriate log severity and message.
--
-- Two startup-time connect failures get distinguished from genuine errors
-- so the operator can tell whether they're "still waiting for the node"
-- (benign) vs. "something is actually wrong" (worth investigating):
--
-- * Socket file does not exist yet (cardano-node hasn't bound the socket).
-- * Connection refused (socket exists but cardano-node isn't accepting).
--
-- Both demote to Info. The 'SubscriptionReconnect' event is also demoted
-- to Debug because it fires after every failure and is redundant given
-- the preceding error trace already explains what happened.
formatSubscriptionTrace :: SubscriptionTrace () -> LogMsg
formatSubscriptionTrace ev = case ev of
  SubscriptionReconnect ->
    LogMsg Debug "Connection" "Will retry connection in 5s"
  SubscriptionError e -> case classifyConnectError e of
    Just reason ->
      LogMsg Info "Connection" reason
    Nothing ->
      LogMsg Warning "Connection" ("Connection error: " <> show e)
  _ ->
    LogMsg Info "Connection" (show ev)

-- | Recognise transient cardano-node-startup connect() failures.
classifyConnectError :: SomeException -> Maybe Text
classifyConnectError se = case fromException se :: Maybe IOError of
  Just ioe
    | isDoesNotExistErrorType (ioeGetErrorType ioe) ->
        Just "Cardano-node socket file not yet present; retrying in 5s"
    | "refused" `Text.isInfixOf` show ioe ->
        Just "Cardano-node socket present but not accepting yet; retrying in 5s"
  _ -> Nothing

-- | Build the NodeToClient protocols bundle.
-- Only ChainSync is active — tx submission, state query, and tx monitor are null.
--
-- The @latestPointRef@ is read by 'blockFetchClient' on every
-- (re)connection, so a mid-run reconnect resumes at our current
-- position rather than the boot-time intersect.
nodeProtocols
  :: AppTracer
  -> CodecConfig (CardanoBlock StandardCrypto)
  -> TBQueue ChainSyncMsg
  -> Maybe (TBQueue ChainSyncMsg)
  -> StateQueryVar
  -> TVar (Maybe CardanoPoint)
  -> TVar (Maybe BlockNo)
  -> TVar (Maybe BlockNo)
  -> Word64
  -> IntersectionRequirement
  -> Network.NodeToClientVersion
  -> BlockNodeToClientVersion (CardanoBlock StandardCrypto)
  -> NodeToClientProtocols 'Mux.InitiatorMode LocalAddress BSL.ByteString IO () Void
nodeProtocols appTracer codecConfig blockQueue mLedgerQueue stateQueryVar latestPointRef rollbackBoundary latestTipBlock kBlocks intersect version blockVersion =
  NodeToClientProtocols
    { localChainSyncProtocol = chainSyncProtocol
    , localTxSubmissionProtocol = dummyTxSubmit
    , localStateQueryProtocol = dummyStateQuery
    , localTxMonitorProtocol = dummyTxMonitor
    }
  where
    codecs = clientCodecs codecConfig blockVersion version

    chainSyncProtocol :: RunMiniProtocolWithMinimalCtx 'Mux.InitiatorMode LocalAddress BSL.ByteString IO () Void
    chainSyncProtocol = InitiatorProtocolOnly $
      MiniProtocolCb $ \_ctx channel -> do
        void $
          runPipelinedPeer
            nullTracer
            (cChainSyncCodec codecs)
            channel
            ( chainSyncClientPeerPipelined $
                blockFetchClient appTracer blockQueue mLedgerQueue latestPointRef rollbackBoundary latestTipBlock kBlocks intersect
            )
        pure ((), Nothing)

    dummyTxSubmit :: RunMiniProtocolWithMinimalCtx 'Mux.InitiatorMode LocalAddress BSL.ByteString IO () Void
    dummyTxSubmit =
      InitiatorProtocolOnly $
        Mux.mkMiniProtocolCbFromPeer $
          const (nullTracer, cTxSubmissionCodec codecs, localTxSubmissionPeerNull)

    dummyStateQuery :: RunMiniProtocolWithMinimalCtx 'Mux.InitiatorMode LocalAddress BSL.ByteString IO () Void
    dummyStateQuery =
      InitiatorProtocolOnly $
        Mux.mkMiniProtocolCbFromPeerSt $
          const (nullTracer, cStateQueryCodec codecs, stateQueryInitState, localStateQueryClientPeer $ localStateQueryHandler stateQueryVar)
      where
        stateQueryInitState = LocalStateQuery.StateIdle

    dummyTxMonitor :: RunMiniProtocolWithMinimalCtx 'Mux.InitiatorMode LocalAddress BSL.ByteString IO () Void
    dummyTxMonitor =
      InitiatorProtocolOnly $
        Mux.mkMiniProtocolCbFromPeer $
          const (nullTracer, cTxMonitorCodec codecs, localTxMonitorPeerNull)

-- ---------------------------------------------------------------------------
-- * ChainSync pipelined client
-- ---------------------------------------------------------------------------

-- | Mutable bookkeeping the chainsync callbacks share across a single
-- session. Created fresh inside 'blockFetchClient' on every
-- (re)connection so both fields reset when a new session starts.
--
-- Both fields here are read and written only by the receiver thread.
data SessionState = SessionState
  { ssPostIntersect   :: !(IORef Bool)
    -- ^ 'True' once the first 'MsgRollForward' of this session has
    -- arrived. The node always sends a confirming 'MsgRollBackward'
    -- to the chosen intersection point right after
    -- 'MsgIntersectFound'; while this flag is 'False' the receiver
    -- treats that rollback as a protocol artefact (not a real
    -- reorg) and does not propagate it to downstream consumers.
  , ssBlocksLeftToLog :: !(IORef Int)
    -- ^ How many forward blocks remain to log at 'Info' (counts
    -- down from 'firstBlocksToLog'). Lets the operator confirm the
    -- new session is producing on reconnect / handoff, where the
    -- first block has a large BlockNo and the historical
    -- "blockNo == 1" trigger never fires.
  }

newSessionState :: IO SessionState
newSessionState =
  SessionState
    <$> newIORef False
    <*> newIORef firstBlocksToLog

-- | How many forward blocks to log at 'Info' at the start of each
-- session. Three gives the operator a clear pulse-check (one to
-- prove the receiver is alive, two more to confirm it's not a
-- one-shot artefact) without flooding the log.
firstBlocksToLog :: Int
firstBlocksToLog = 3

-- | Pipelined ChainSync client that writes blocks to a TQueue.
--
-- The intersection point is chosen at every (re)connection:
--
--   * If @latestPointRef@ holds a point, the receiver intersects
--     there. This is the reconnection path: the node sends forward
--     from where we last were, after a benign confirming rollback to
--     the intersection point.
--   * Otherwise it falls back to the boot-time @intersect@: either
--     'IntersectGenesis' (first connection on a fresh DB) or
--     'IntersectAt' (resume from snapshot candidates).
--
-- Without the TVar-tracked latest point, a @cardano-node@ restart
-- mid-sync would re-use the boot-time intersect — for a fresh sync
-- that is Origin, so the node rolls our chain pointer back to
-- genesis and the LedgerWorker crashes when the genesis block
-- arrives over its slot-N state.
--
-- 'IntersectGenesis' tolerates a not-found response (used on a fresh
-- start when the node also has no chain yet). 'IntersectAt' and the
-- IORef-tracked resume both treat not-found as fatal: the node's
-- chain has diverged from every candidate point we offered.
--
-- When @mLedgerQueue@ is 'Just', each block is also enqueued on it
-- after the main queue write succeeds.
blockFetchClient
  :: AppTracer
  -> TBQueue ChainSyncMsg                             -- ^ Main pipeline queue
  -> Maybe (TBQueue ChainSyncMsg)                     -- ^ Optional ledger worker queue
  -> TVar (Maybe CardanoPoint)                        -- ^ Latest received point, updated on each forward / rollback
  -> TVar (Maybe BlockNo)                             -- ^ Rollback boundary, updated on every tip observation
  -> TVar (Maybe BlockNo)                             -- ^ Latest server tip block number, updated on every tip observation
  -> Word64                                           -- ^ Protocol security parameter @k@
  -> IntersectionRequirement                          -- ^ Boot-time fallback intersection
  -> ChainSyncClientPipelined
       (CardanoBlock StandardCrypto)
       (Point (CardanoBlock StandardCrypto))
       (Tip (CardanoBlock StandardCrypto))
       IO
       ()
blockFetchClient appTracer blockQueue mLedgerQueue latestPointRef rollbackBoundary latestTipBlock kBlocks intersect =
  ChainSyncClientPipelined $ do
    -- Per-session mutable bookkeeping. Bundled so callbacks pass one
    -- record instead of N independent IORefs.
    ss <- newSessionState
    mLatest <- readTVarIO latestPointRef
    let (intersectPoints, isResume) = case mLatest of
          Just p  -> ([p], True)
          Nothing -> (bootIntersectPoints, False)
    when isResume $
      traceWith appTracer $ LogMsg Info "ChainSync"
        ("Resuming from last received point " <> show mLatest)
    pure $
      SendMsgFindIntersect
        intersectPoints
        ClientPipelinedStIntersect
          { recvMsgIntersectFound    = onIntersectFound ss
          , recvMsgIntersectNotFound = onIntersectNotFound isResume ss
          }
  where
    bootIntersectPoints = case intersect of
      IntersectGenesis -> [genesisPoint]
      IntersectAt ps   -> ps

    -- Log the chosen candidate so the operator can see which
    -- snapshot point the node selected — useful when the candidate
    -- list contains fallbacks beyond the newest snapshot.
    onIntersectFound ss chosen tip = do
      traceWith appTracer $ LogMsg Info "ChainSync"
        ("Intersected at " <> show chosen <> " (server tip " <> show tip <> ")")
      atomicWriteIORef (ssPostIntersect ss) False
      pure $ goTip ss policy Zero Origin tip

    onIntersectNotFound isResume ss tip
      | isResume =
          throwNetwork $
            "ChainSync reconnection: node could not intersect at our last "
              <> "received point. The node's chain has diverged from our "
              <> "current position while we were disconnected. "
              <> "Server tip: " <> show tip
      | otherwise = case intersect of
          IntersectGenesis -> do
            traceWith appTracer $ LogMsg Info "ChainSync"
              "Node also has no chain yet; following from origin"
            atomicWriteIORef (ssPostIntersect ss) False
            pure $ goTip ss policy Zero Origin tip
          IntersectAt ps ->
            throwNetwork $
              "ChainSync intersection not found on node at any of "
                <> show (length ps) <> " candidate points: " <> show ps
                <> " — node DB may be older than dbsync's resume point, or its "
                <> "chain has diverged from every known snapshot. "
                <> "Server tip: " <> show tip

    -- Pipeline depth limits: start requesting at 10 in-flight,
    -- cap at 50 in-flight. Balances throughput with memory/backpressure.
    -- Unlimited (0/maxBound) causes memory growth and TCP backpressure.
    policy :: MkPipelineDecision
    policy = pipelineDecisionLowHighMark 10 50

    goTip
      :: SessionState
      -> MkPipelineDecision
      -> Nat n
      -> WithOrigin BlockNo
      -> Tip (CardanoBlock StandardCrypto)
      -> ClientPipelinedStIdle n (CardanoBlock StandardCrypto) CardanoPoint (Tip (CardanoBlock StandardCrypto)) IO ()
    goTip ss mkDecision n clientTip serverTip =
      go ss mkDecision n clientTip (getTipBlockNo serverTip)

    go
      :: SessionState
      -> MkPipelineDecision
      -> Nat n
      -> WithOrigin BlockNo
      -> WithOrigin BlockNo
      -> ClientPipelinedStIdle n (CardanoBlock StandardCrypto) CardanoPoint (Tip (CardanoBlock StandardCrypto)) IO ()
    go ss mkDecision n clientTip serverTip =
      case (n, runPipelineDecision mkDecision n clientTip serverTip) of
        (_Zero, (Request, mkDecision')) ->
          SendMsgRequestNext (pure ()) (mkClientStNext ss mkDecision' n)
        (_, (Pipeline, mkDecision')) ->
          SendMsgRequestNextPipelined
            (pure ())
            (go ss mkDecision' (Succ n) clientTip serverTip)
        (Succ n', (CollectOrPipeline, mkDecision')) ->
          CollectResponse
            (Just . pure $ SendMsgRequestNextPipelined (pure ()) $ go ss mkDecision' (Succ n) clientTip serverTip)
            (mkClientStNext ss mkDecision' n')
        (Succ n', (Collect, mkDecision')) ->
          CollectResponse
            Nothing
            (mkClientStNext ss mkDecision' n')

    mkClientStNext
      :: SessionState
      -> MkPipelineDecision
      -> Nat n
      -> ClientStNext n (CardanoBlock StandardCrypto) CardanoPoint (Tip (CardanoBlock StandardCrypto)) IO ()
    mkClientStNext ss mkDecision n =
      ClientStNext
        { recvMsgRollForward = \blk tip -> do
            let bn = blockNo blk
                blkSlot = blockSlot blk
            -- The first three blocks of every session log at Info so
            -- the operator can confirm the post-intersect stream is
            -- producing — important on reconnect / handoff where the
            -- first block has a large BlockNo that the historical
            -- "block 1" trigger missed. No per-block Debug trace:
            -- the message Text would be built (then dropped by the
            -- phase filter) on every block of the bulk sync. The
            -- counter is only touched on this thread, so the plain
            -- read costs nothing once it reaches zero.
            remaining <- readIORef (ssBlocksLeftToLog ss)
            when (remaining > 0) $ do
              writeIORef (ssBlocksLeftToLog ss) (remaining - 1)
              traceWith appTracer $ LogMsg Info "ChainSync"
                ( "First post-intersect block at slot " <> show blkSlot
                    <> ", block " <> show bn
                )
            -- Mark the post-intersect handshake as complete; any
            -- subsequent rollback is a real chain reorganisation.
            atomicWriteIORef (ssPostIntersect ss) True
            -- The rollback boundary moves with the node tip; publish
            -- it before enqueuing so a slow consumer never sees a
            -- block whose ancestor has already passed the boundary.
            publishTipMarkers tip
            deliverForwardBlock blockQueue mLedgerQueue latestPointRef blk
            pure $ goTip ss mkDecision n (At bn) tip
        , recvMsgRollBackward = \point tip -> do
            -- The first MsgRollBackward after MsgIntersectFound is the
            -- node's confirming rollback to the chosen intersection
            -- point — a protocol artefact, not a chain reorganisation.
            -- Don't surface it to downstream consumers; just record
            -- the position and continue.
            isConfirmingRollback <- atomicModifyIORef' (ssPostIntersect ss)
              (\seen -> (True, not seen))
            -- Confirming rollbacks log at Debug (benign protocol
            -- step; same severity as the regular per-block trace);
            -- real chain reorgs log at Warning (downstream consumers
            -- are about to see DELETEs).
            let sev = if isConfirmingRollback then Debug else Warning
                logText
                  | isConfirmingRollback =
                      "Rollback to " <> show point
                        <> " (confirming intersect — protocol step, no rows deleted)"
                  | otherwise =
                      "Rollback to " <> show point
            traceWith appTracer $ LogMsg sev "ChainSync" logText
            publishTipMarkers tip
            deliverRollback blockQueue mLedgerQueue latestPointRef
              isConfirmingRollback point
            pure $ goTip ss mkDecision n Origin tip
        }

    -- Publish both the server's tip block number and the rollback
    -- boundary (@tipBlock − k@) on every tip observation. 'Nothing'
    -- on the boundary while the chain is still shorter than @k@
    -- blocks — everything is volatile in that case.
    publishTipMarkers :: Tip (CardanoBlock StandardCrypto) -> IO ()
    publishTipMarkers tip = do
      let mTip = case getTipBlockNo tip of
            Origin -> Nothing
            At bn  -> Just bn
          boundary = case mTip of
            Just (BlockNo n)
              | n >= kBlocks -> Just (BlockNo (n - kBlocks))
              | otherwise    -> Nothing
            Nothing -> Nothing
      atomically $ do
        writeTVar latestTipBlock mTip
        writeTVar rollbackBoundary boundary

-- | Hand one forward block to the consumer queues and record it as
-- the latest received point — all in a single STM transaction.
--
-- Atomicity is load-bearing, not style: the receiver is killed with
-- an async exception at the Ingest → Follow handoff (and on node
-- reconnects), and the ledger-queue write can block while the worker
-- is behind. With separate transactions a kill landing
-- after the main-queue write but before the point write leaves the
-- block queued yet unrecorded; the next session then re-requests it
-- and the Follow consumer applies it twice — the @tx@ unique
-- constraint aborts the app (the @block@ table has no unique
-- constraint to object sooner). In one transaction the kill either
-- lands before commit (nothing delivered, nothing recorded — the
-- next session re-fetches the block) or after (everything
-- consistent).
--
-- The transaction retries until every queue involved has space, so
-- backpressure into the receiver is preserved: it may not out-run
-- the slower of the two consumers.
deliverForwardBlock
  :: TBQueue ChainSyncMsg          -- ^ Main pipeline queue
  -> Maybe (TBQueue ChainSyncMsg)  -- ^ Ledger worker queue, when enabled
  -> TVar (Maybe CardanoPoint)     -- ^ Latest received point
  -> CardanoBlock StandardCrypto
  -> IO ()
deliverForwardBlock blockQueue mLedgerQueue latestPointVar blk =
  atomically $ do
    let msg = MsgForward blk
    writeTBQueue blockQueue msg
    for_ mLedgerQueue $ \ledgerQueue -> writeTBQueue ledgerQueue msg
    writeTVar latestPointVar (Just (blockPoint blk))

-- | Rollback counterpart of 'deliverForwardBlock': record the
-- rollback target as the latest received point and — for a real
-- chain reorganisation — enqueue the 'MsgRollback' marker on both
-- queues, in one STM transaction for the same kill-safety reason.
-- Losing the marker while still recording the point would leave
-- rolled-back rows in PG forever.
--
-- A confirming rollback (the protocol's echo of the chosen
-- intersection point) only moves the point; no marker is delivered.
deliverRollback
  :: TBQueue ChainSyncMsg
  -> Maybe (TBQueue ChainSyncMsg)
  -> TVar (Maybe CardanoPoint)
  -> Bool                          -- ^ confirming rollback (suppress marker)?
  -> CardanoPoint
  -> IO ()
deliverRollback blockQueue mLedgerQueue latestPointVar isConfirming point =
  atomically $ do
    unless isConfirming $ do
      let msg = MsgRollback point
      writeTBQueue blockQueue msg
      for_ mLedgerQueue $ \ledgerQueue -> writeTBQueue ledgerQueue msg
    writeTVar latestPointVar (Just point)
