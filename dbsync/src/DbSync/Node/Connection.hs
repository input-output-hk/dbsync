{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | ChainSync node connection.
--
-- Connects to a cardano-node via Unix socket, runs the ChainSync
-- mini-protocol, and pushes received blocks to the 'IngestEnv'\'s
-- block queue.
module DbSync.Node.Connection
  ( -- * Running
    connectToNode
  , getNetworkMagic
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
import Control.Concurrent.STM (TBQueue, writeTBQueue)
import Control.Concurrent.STM.TBQueue (isFullTBQueue)
import Control.Tracer (contramap, nullTracer, traceWith)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import System.IO.Error (IOError, ioeGetErrorType, isDoesNotExistErrorType)
import qualified Network.Mux as Mux
import Network.TypedProtocol.Peer (Nat (..))

import Ouroboros.Consensus.Block.Abstract (CodecConfig)
import Ouroboros.Consensus.Byron.Node ()
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Config (TopLevelConfig, configCodec)
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

import DbSync.AppM (IngestM)
import DbSync.Block.Types (CardanoPoint)
import DbSync.Config.Genesis (GenesisConfig (..), ShelleyConfig (..))
import DbSync.Env (IngestEnv (..))
import DbSync.Ingest.ReceiverStats (ReceiverStats, recordBlockReceived, recordWriteBlocked)
import DbSync.Ledger.Types (HasLedgerEnv (..), LedgerEnv (..))
import DbSync.StateQuery (StateQueryVar, localStateQueryHandler)
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- ---------------------------------------------------------------------------
-- * Network magic
-- ---------------------------------------------------------------------------

-- | Extract the 'NetworkMagic' from a 'GenesisConfig'.
-- Comes from the Shelley genesis 'sgNetworkMagic' field.
getNetworkMagic :: GenesisConfig -> NetworkMagic
getNetworkMagic gc = NetworkMagic $ sgNetworkMagic (scConfig $ gcShelley gc)

-- ---------------------------------------------------------------------------
-- * Connection
-- ---------------------------------------------------------------------------

-- | Connect to a cardano-node and run the ChainSync protocol.
--
-- Received blocks are written to the env's 'ieBlockQueue'. This function
-- blocks indefinitely (reconnects on failure).
--
-- The arguments that aren't on 'IngestEnv' ('IOManager', the
-- 'TopLevelConfig', the 'NetworkMagic', and the socket path) are
-- network-wiring concerns supplied by 'Main' once 'withIOManager' has
-- allocated the manager and the genesis config has been read.
connectToNode
  :: IOManager
  -> TopLevelConfig (CardanoBlock StandardCrypto)
  -> NetworkMagic
  -> FilePath                                    -- ^ Node socket path
  -> IngestM ()
connectToNode iomgr topLevelCfg networkMagic socketPath = do
  tracer         <- asks getTracer
  blockQueue     <- asks ieBlockQueue
  stateQueryVar  <- asks ieStateQueryVar
  receiverStats  <- asks ieReceiverStats
  hasLedgerEnv   <- asks ieHasLedgerEnv
  -- The ledger queue only exists in the enabled arm; pattern-matching
  -- it out here keeps blockFetchClient's optional second-target
  -- contract crisp (no LedgerDisabled-shaped sentinel queue).
  let mLedgerQueue =
        case hasLedgerEnv of
          LedgerEnabled lenv -> Just (leLedgerQueue lenv)
          LedgerDisabled _   -> Nothing
  liftIO $ do
    traceWith tracer $ LogMsg Info "Connection" ("Connecting to node via " <> toS socketPath) Nothing
    void $
      subscribe
        (localSnocket iomgr)
        networkMagic
        (supportedNodeToClientVersions (Proxy @(CardanoBlock StandardCrypto)))
        (subscriptionTracers tracer)
        subscriptionParams
        (nodeProtocols tracer codecConfig blockQueue mLedgerQueue receiverStats stateQueryVar)
  where
    codecConfig :: CodecConfig (CardanoBlock StandardCrypto)
    codecConfig = configCodec topLevelCfg

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
    LogMsg Debug "Connection" "Will retry connection in 5s" Nothing
  SubscriptionError e -> case classifyConnectError e of
    Just reason ->
      LogMsg Info "Connection" reason Nothing
    Nothing ->
      LogMsg Warning "Connection" ("Connection error: " <> show e) Nothing
  _ ->
    LogMsg Info "Connection" (show ev) Nothing

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
nodeProtocols
  :: AppTracer
  -> CodecConfig (CardanoBlock StandardCrypto)
  -> TBQueue (CardanoBlock StandardCrypto)
  -> Maybe (TBQueue (CardanoBlock StandardCrypto))
  -> ReceiverStats
  -> StateQueryVar
  -> Network.NodeToClientVersion
  -> BlockNodeToClientVersion (CardanoBlock StandardCrypto)
  -> NodeToClientProtocols 'Mux.InitiatorMode LocalAddress BSL.ByteString IO () Void
nodeProtocols appTracer codecConfig blockQueue mLedgerQueue receiverStats stateQueryVar version blockVersion =
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
                blockFetchClient appTracer blockQueue mLedgerQueue receiverStats
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

-- | Simple pipelined ChainSync client that writes blocks to a TQueue.
-- Starts from genesis (no intersection points) and pipelines aggressively.
--
-- When @mLedgerQueue@ is 'Just', each block is also enqueued on it
-- after the main queue write succeeds — this is the 'LedgerWorker'
-- fan-out per LEDGER-PLAN §5. The ledger queue write blocks if full
-- (matching the main queue's behaviour); the worker is intended to
-- keep up but if it falls behind we'd rather slow the receiver than
-- silently drop blocks.
blockFetchClient
  :: AppTracer
  -> TBQueue (CardanoBlock StandardCrypto)            -- ^ Main pipeline queue
  -> Maybe (TBQueue (CardanoBlock StandardCrypto))    -- ^ Optional ledger worker queue
  -> ReceiverStats
  -> ChainSyncClientPipelined
       (CardanoBlock StandardCrypto)
       (Point (CardanoBlock StandardCrypto))
       (Tip (CardanoBlock StandardCrypto))
       IO
       ()
blockFetchClient appTracer blockQueue mLedgerQueue receiverStats =
  ChainSyncClientPipelined $ pure $
    -- Start from genesis
    SendMsgFindIntersect
      [genesisPoint]
      ClientPipelinedStIntersect
        { recvMsgIntersectFound    = \_hdr tip -> pure $ goTip policy Zero Origin tip
        , recvMsgIntersectNotFound = \tip      -> pure $ goTip policy Zero Origin tip
        }
  where
    -- Pipeline depth limits: start requesting at 10 in-flight,
    -- cap at 50 in-flight. Balances throughput with memory/backpressure.
    -- Unlimited (0/maxBound) causes memory growth and TCP backpressure.
    policy :: MkPipelineDecision
    policy = pipelineDecisionLowHighMark 10 50

    goTip
      :: MkPipelineDecision
      -> Nat n
      -> WithOrigin BlockNo
      -> Tip (CardanoBlock StandardCrypto)
      -> ClientPipelinedStIdle n (CardanoBlock StandardCrypto) CardanoPoint (Tip (CardanoBlock StandardCrypto)) IO ()
    goTip mkDecision n clientTip serverTip =
      go mkDecision n clientTip (getTipBlockNo serverTip)

    go
      :: MkPipelineDecision
      -> Nat n
      -> WithOrigin BlockNo
      -> WithOrigin BlockNo
      -> ClientPipelinedStIdle n (CardanoBlock StandardCrypto) CardanoPoint (Tip (CardanoBlock StandardCrypto)) IO ()
    go mkDecision n clientTip serverTip =
      case (n, runPipelineDecision mkDecision n clientTip serverTip) of
        (_Zero, (Request, mkDecision')) ->
          SendMsgRequestNext (pure ()) (mkClientStNext mkDecision' n)
        (_, (Pipeline, mkDecision')) ->
          SendMsgRequestNextPipelined
            (pure ())
            (go mkDecision' (Succ n) clientTip serverTip)
        (Succ n', (CollectOrPipeline, mkDecision')) ->
          CollectResponse
            (Just . pure $ SendMsgRequestNextPipelined (pure ()) $ go mkDecision' (Succ n) clientTip serverTip)
            (mkClientStNext mkDecision' n')
        (Succ n', (Collect, mkDecision')) ->
          CollectResponse
            Nothing
            (mkClientStNext mkDecision' n')

    mkClientStNext
      :: MkPipelineDecision
      -> Nat n
      -> ClientStNext n (CardanoBlock StandardCrypto) CardanoPoint (Tip (CardanoBlock StandardCrypto)) IO ()
    mkClientStNext mkDecision n =
      ClientStNext
        { recvMsgRollForward = \blk tip -> do
            let bn@(BlockNo bnRaw) = blockNo blk
            -- Per-block trace at Debug; periodic Info at first block and
            -- every 10000 thereafter so the operator can confirm blocks
            -- are still arriving without flooding the log.
            traceWith appTracer $ LogMsg Debug "ChainSync"
              ("Block " <> show bn) Nothing
            when (bnRaw == 1 || bnRaw `mod` 10000 == 0) $
              traceWith appTracer $ LogMsg Info "ChainSync"
                ("Received block #" <> show bnRaw) Nothing
            recordBlockReceived receiverStats
            -- Try a non-blocking write first so we can tell whether the
            -- queue was full at the moment of arrival. A non-zero
            -- 'rsWritesBlocked' means the consumer is the bottleneck;
            -- a zero count with low drain averages means the upstream
            -- node is. ('stm' has 'tryReadTBQueue' but no 'tryWriteTBQueue',
            -- so we synthesise the same semantics from 'isFullTBQueue' +
            -- 'writeTBQueue' inside a single STM transaction.)
            ok <- atomically $ do
              full <- isFullTBQueue blockQueue
              if full
                then pure False
                else writeTBQueue blockQueue blk >> pure True
            unless ok $ do
              recordWriteBlocked receiverStats
              atomically $ writeTBQueue blockQueue blk
            -- Fan-out to the ledger worker (when enabled). The
            -- worker is a single consumer with a shallower queue, so
            -- we accept that the receiver may block here if the
            -- worker has fallen behind — preferable to dropping
            -- blocks silently.
            for_ mLedgerQueue $ \ledgerQueue ->
              atomically $ writeTBQueue ledgerQueue blk
            pure $ goTip mkDecision n (At bn) tip
        , recvMsgRollBackward = \point tip -> do
            traceWith appTracer $ LogMsg Warning "ChainSync"
              ("Rollback to " <> show point) Nothing
            pure $ goTip mkDecision n Origin tip
        }
