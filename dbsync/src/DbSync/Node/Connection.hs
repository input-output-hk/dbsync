{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | ChainSync node connection.
--
-- Connects to a cardano-node via Unix socket, runs the ChainSync
-- mini-protocol, and pushes received blocks to a 'TQueue'.
module DbSync.Node.Connection
  ( -- * Types
    CardanoBlock
  , CardanoPoint

    -- * Running
  , connectToNode
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
import qualified Codec.CBOR.Term as CBOR
import Control.Concurrent.Async (AsyncCancelled (..))
import Control.Concurrent.STM (TBQueue, writeTBQueue)
import Control.Tracer (Tracer, contramap, nullTracer, traceWith)
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Mux as Mux
import Network.TypedProtocol.Peer (N (..), Nat (..))

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
  ( ConnectionId
  , Handshake
  , IOManager
  , LocalAddress
  , NodeToClientProtocols (..)
  , TraceSendRecv
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
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import qualified Ouroboros.Network.Snocket as Snocket

import Cardano.Slotting.Slot (WithOrigin (..))

import DbSync.Config.Genesis (GenesisConfig (..), ShelleyConfig (..))
import DbSync.StateQuery (StateQueryVar, localStateQueryHandler)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- * Types

-- | A point on the Cardano blockchain.
type CardanoPoint = Point (CardanoBlock StandardCrypto)

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
-- Received blocks are written to the provided 'TQueue'.
-- This function blocks indefinitely (reconnects on failure).
connectToNode
  :: AppTracer
  -> IOManager
  -> TopLevelConfig (CardanoBlock StandardCrypto)
  -> NetworkMagic
  -> FilePath                                    -- ^ Node socket path
  -> TBQueue (CardanoBlock StandardCrypto)        -- ^ Block queue (bounded)
  -> StateQueryVar                               -- ^ For LocalStateQuery (epoch interpreter)
  -> IO ()
connectToNode tracer iomgr topLevelCfg networkMagic socketPath blockQueue stateQueryVar = do
  traceWith tracer $ LogMsg Info "Connection" ("Connecting to node via " <> toS socketPath) Nothing
  void $
    subscribe
      (localSnocket iomgr)
      networkMagic
      (supportedNodeToClientVersions (Proxy @(CardanoBlock StandardCrypto)))
      subscriptionTracers
      subscriptionParams
      (nodeProtocols tracer codecConfig blockQueue stateQueryVar)
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
    subscriptionTracers :: SubscriptionTracers ()
    subscriptionTracers =
      SubscriptionTracers
        { stMuxTracer = nullTracer
        , stHandshakeTracer = nullTracer
        , stSubscriptionTracer = subscriptionTracer
        , stMuxChannelTracer = nullTracer
        , stMuxBearerTracer = nullTracer
        }

    subscriptionTracer :: Tracer IO (SubscriptionTrace ())
    subscriptionTracer = contramap formatSubscriptionTrace tracer

-- | Map subscription events to appropriate log severity and message.
formatSubscriptionTrace :: SubscriptionTrace () -> LogMsg
formatSubscriptionTrace ev =
  let msg = show ev
  in case ev of
       SubscriptionReconnect ->
         LogMsg Warning "Connection" "Node disconnected. Reconnecting..." Nothing
       SubscriptionError e ->
         LogMsg Warning "Connection" ("Connection error: " <> show e) Nothing
       _ ->
         LogMsg Info "Connection" msg Nothing

-- | Build the NodeToClient protocols bundle.
-- Only ChainSync is active — tx submission, state query, and tx monitor are null.
nodeProtocols
  :: AppTracer
  -> CodecConfig (CardanoBlock StandardCrypto)
  -> TBQueue (CardanoBlock StandardCrypto)
  -> StateQueryVar
  -> Network.NodeToClientVersion
  -> BlockNodeToClientVersion (CardanoBlock StandardCrypto)
  -> NodeToClientProtocols 'Mux.InitiatorMode LocalAddress BSL.ByteString IO () Void
nodeProtocols appTracer codecConfig blockQueue stateQueryVar version blockVersion =
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
                blockFetchClient appTracer blockQueue
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
blockFetchClient
  :: AppTracer
  -> TBQueue (CardanoBlock StandardCrypto)
  -> ChainSyncClientPipelined
       (CardanoBlock StandardCrypto)
       (Point (CardanoBlock StandardCrypto))
       (Tip (CardanoBlock StandardCrypto))
       IO
       ()
blockFetchClient appTracer blockQueue =
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
            let bn = blockNo blk
            traceWith appTracer $ LogMsg Debug "ChainSync"
              ("Block " <> show bn) Nothing
            atomically $ writeTBQueue blockQueue blk
            pure $ goTip mkDecision n (At bn) tip
        , recvMsgRollBackward = \point tip -> do
            traceWith appTracer $ LogMsg Warning "ChainSync"
              ("Rollback to " <> show point) Nothing
            pure $ goTip mkDecision n Origin tip
        }
