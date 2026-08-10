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
    -- ^ Resume past a previous run. The receiver offers candidate
    -- points newest-first and the node picks the one on its chain.
    -- The list is non-empty by construction; the boot flow signals
    -- the empty case with 'IntersectGenesis'. If no candidate
    -- intersects, the connection fails fatally.

-- ---------------------------------------------------------------------------
-- * Network magic
-- ---------------------------------------------------------------------------

getNetworkMagic :: GenesisConfig -> NetworkMagic
getNetworkMagic gc = NetworkMagic $ sgNetworkMagic (scConfig $ gcShelley gc)

-- | Label for the well-known network magics; anything else renders as
-- @magic-\<N\>@. The magic is the identity. The name only feeds log
-- lines and the @dbsync_sync_state.network_name@ column.
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

-- | Connect to a cardano-node and run the ChainSync protocol. Writes
-- received blocks to the env's block queue and never returns; it
-- reconnects on failure.
--
-- Polymorphic over the env, so the same call drives Ingest and
-- Follow: only the consumer-side env changes at a phase boundary.
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

-- | Map subscription events to a severity and message.
--
-- Two startup-time connect failures demote to 'Info', so the operator
-- can separate "still waiting for the node" from a real fault: the
-- socket file is absent, or the socket refuses the connection.
-- 'SubscriptionReconnect' demotes to 'Debug' because it fires after
-- every failure and the preceding error trace already explains it.
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

-- | Build the NodeToClient protocols bundle. ChainSync and
-- LocalStateQuery are live; tx submission and tx monitor are null.
--
-- 'blockFetchClient' reads @latestPointRef@ on every (re)connection,
-- so a mid-run reconnect resumes at the current position instead of
-- the boot-time intersect.
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

-- | Per-session bookkeeping the chainsync callbacks share.
-- 'blockFetchClient' builds a fresh one on every (re)connection, so
-- both fields reset. Only the receiver thread touches them.
data SessionState = SessionState
  { ssPostIntersect   :: !(IORef Bool)
    -- ^ 'True' once this session's first 'MsgRollForward' arrives.
    -- The node always sends a confirming 'MsgRollBackward' to the
    -- chosen intersection point right after 'MsgIntersectFound';
    -- while the flag is 'False' the receiver treats that rollback as
    -- a protocol artefact and keeps it from downstream consumers.
  , ssBlocksLeftToLog :: !(IORef Int)
    -- ^ Forward blocks still to log at 'Info', counting down from
    -- 'firstBlocksToLog'.
  }

newSessionState :: IO SessionState
newSessionState =
  SessionState
    <$> newIORef False
    <*> newIORef firstBlocksToLog

-- | Forward blocks to log at 'Info' at the start of each session.
-- Three prove the new session produces without flooding the log. A
-- reconnect or phase handoff starts at a large BlockNo, so a
-- "block 1" trigger would never fire there.
firstBlocksToLog :: Int
firstBlocksToLog = 3

-- | Pipelined ChainSync client that writes blocks to the queues.
--
-- Each (re)connection picks its own intersection point. A point in
-- @latestPointRef@ wins; otherwise the boot-time @intersect@ applies.
-- Without the TVar-tracked point a mid-sync node restart would re-use
-- the boot-time intersect — Origin on a fresh sync — so the node
-- would roll the chain pointer back to genesis and the LedgerWorker
-- would crash when the genesis block arrived over its slot-N state.
--
-- 'IntersectGenesis' tolerates a not-found response, because a fresh
-- start may meet a node that has no chain either. 'IntersectAt' and
-- the tracked resume both treat not-found as fatal: the node's chain
-- has diverged from every candidate point.
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

    -- Log the chosen candidate: the list can hold fallbacks older
    -- than the newest snapshot, so the operator needs to see which
    -- one the node selected.
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

    -- Pipeline depth: request from 10 in-flight, cap at 50. An
    -- unlimited depth grows memory and stalls TCP backpressure.
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
            -- No per-block Debug trace here: the message Text would
            -- be built, then dropped by the phase filter, for every
            -- block of the bulk sync. Only this thread touches the
            -- counter, so the plain read costs nothing once it hits
            -- zero.
            remaining <- readIORef (ssBlocksLeftToLog ss)
            when (remaining > 0) $ do
              writeIORef (ssBlocksLeftToLog ss) (remaining - 1)
              traceWith appTracer $ LogMsg Info "ChainSync"
                ( "First post-intersect block at slot " <> show blkSlot
                    <> ", block " <> show bn
                )
            -- The handshake is complete; any later rollback is a real
            -- chain reorganisation.
            atomicWriteIORef (ssPostIntersect ss) True
            -- Publish the boundary before enqueuing, so a slow
            -- consumer never sees a block whose ancestor has already
            -- passed it.
            publishTipMarkers tip
            deliverForwardBlock blockQueue mLedgerQueue latestPointRef blk
            pure $ goTip ss mkDecision n (At bn) tip
        , recvMsgRollBackward = \point tip -> do
            -- The first MsgRollBackward after MsgIntersectFound is the
            -- node's confirming rollback to the chosen intersection
            -- point: a protocol artefact, not a reorganisation. Record
            -- the position and continue without telling consumers.
            isConfirmingRollback <- atomicModifyIORef' (ssPostIntersect ss)
              (\seen -> (True, not seen))
            -- A confirming rollback is benign, so it logs at Debug. A
            -- real reorg logs at Warning: consumers are about to see
            -- DELETEs.
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
-- the latest received point, in a single STM transaction.
--
-- The atomicity is load-bearing. An async exception kills the
-- receiver at the Ingest → Follow handoff and on node reconnects, and
-- the ledger-queue write can block behind a slow worker. In separate
-- transactions a kill between the queue write and the point write
-- leaves the block queued but unrecorded; the next session re-requests
-- it and the Follow consumer applies it twice, which aborts the app on
-- the @tx@ unique constraint. The @block@ table has no unique
-- constraint to object sooner. In one transaction the kill lands
-- either before the commit or after it, and both states are
-- consistent.
--
-- The transaction retries until every queue has space, so the
-- receiver cannot out-run the slower consumer.
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

-- | Rollback counterpart of 'deliverForwardBlock'. One transaction,
-- for the same kill-safety reason: losing the 'MsgRollback' marker
-- while still recording the point would leave rolled-back rows in PG
-- forever.
--
-- A confirming rollback echoes the chosen intersection point, so it
-- only moves the point and delivers no marker.
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
