{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Mock cardano-node for the dbsync test suite.
--
-- Builds on 'DbSync.Test.MockChain' (interpreter + topLevelCfg +
-- state-query seed) and forks the vendored
-- 'Cardano.Mock.ChainSync.Server' on a unique temp Unix socket. The
-- real dbsync receiver then connects to this socket exactly as it
-- would to a production cardano-node, so the whole pipeline runs
-- end-to-end with no shortcuts.
module DbSync.Test.MockNode
  ( -- * Environment
    MockNode (..)
  , withMockNode

    -- * Forging into the live server
  , forgeAndPush
  , forgeAndPushBlocks
  , forgeAndPushBlocksWith
  , forgeAndPushWithStakeCreds
  , rollbackMockNode

    -- * Inspection
  , currentTip
  , currentChainLength

    -- * Lifecycle helpers
  , restartMockNode
  , waitForReconnect
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Mary.Value (valueFromList)
import Ouroboros.Consensus.Block.Abstract (blockHash, blockNo, blockSlot)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Mempool ()
import qualified Ouroboros.Network.Block as Network
import Ouroboros.Network.Block (Tip (..))
import Ouroboros.Network.Magic (NetworkMagic (..))

import Cardano.Crypto.Init (cryptoInit)

import qualified Cardano.Mock.Chain as MockChainTypes
import qualified Cardano.Mock.ChainDB as MockChainDB
import qualified Cardano.Mock.ChainSync.Server as MockServer
import qualified Cardano.Mock.Forging.Interpreter as Mock
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import qualified Cardano.Mock.Forging.Types as Mock

import DbSync.Test.MockChain
  ( MockChain (..)
  , forgeNextBlock
  , registerStakeCreds
  , reseedStateQueryFromLedger
  , withMockChain
  )
import DbSync.Test.MockNode.Workload
  ( Workload (..)
  , paymentCredentialAt
  )

-- ---------------------------------------------------------------------------
-- * Environment
-- ---------------------------------------------------------------------------

-- | Wraps a 'MockChain' (interpreter, topLevelCfg, state-query seed)
-- with the ChainSync server fork + the socket the server is bound
-- to. The 'IOManager' is held so callers can pass it through to
-- @connectToNode@ if they want to drive a custom client.
--
-- 'mnAddressCounter' is a monotonically-increasing index used by
-- 'forgeAndPushBlocksWith' to derive fresh payment credentials for
-- transaction outputs. Persisting it on 'MockNode' keeps every call
-- inside the same test producing distinct addresses.
data MockNode = MockNode
  { mnChain          :: !MockChain
  , mnSocketPath     :: !FilePath
  , mnServer         :: !(MockServer.ServerHandle IO (CardanoBlock StandardCrypto))
  , mnIOManager      :: !MockServer.IOManager
  , mnNetworkMagic   :: !NetworkMagic
  , mnAddressCounter :: !(IORef Int)
  }

-- | Bracketed setup. Allocates: libsodium init, IO manager,
-- 'MockChain', temp Unix socket, and the ChainSync server thread.
-- Releases everything (including removing the socket file) on exit.
withMockNode :: FilePath -> (MockNode -> IO a) -> IO a
withMockNode configDir action = do
  cryptoInit
  MockServer.withIOManager $ \iom ->
    withMockChain configDir $ \mc ->
      withTempSocket $ \sockPath -> do
        let magic = NetworkMagic 42  -- matches the Conway test genesis
        initSt <- initialChainState mc
        -- Starts at 1 because 'createPaymentCredentials' / the
        -- dummy-hash helper index from 1 too.
        addrCounter <- newIORef 1
        MockServer.withServerHandle
          iom
          (mcTopLevelConfig mc)
          initSt
          magic
          sockPath
          $ \server ->
            action MockNode
              { mnChain          = mc
              , mnSocketPath     = sockPath
              , mnServer         = server
              , mnIOManager      = iom
              , mnNetworkMagic   = magic
              , mnAddressCounter = addrCounter
              }

-- | The interpreter's initial chain state, in the @(ExtLedgerState,
-- LedgerTables)@ tuple shape the server's 'ChainDB' wants. Read
-- /before any block is forged/ so the server's chain starts at the
-- same Genesis the interpreter does.
initialChainState
  :: MockChain
  -> IO (MockChainTypes.State (CardanoBlock StandardCrypto))
initialChainState mc = do
  s <- Mock.getCurrentInterpreterState (mcInterpreter mc)
  pure (MockChainDB.currentState (Mock.istChain s))

-- ---------------------------------------------------------------------------
-- * Forging into the live server
-- ---------------------------------------------------------------------------

-- | Forge one block via the interpreter and publish it on the
-- ChainSync server's chain DB. A connected dbsync receiver picks it
-- up on the next @MsgRequestNext@. Re-seeds the state-query handle
-- so 'parseBlock' downstream can resolve slot details without
-- waiting on the mock server's stubbed LocalStateQuery.
forgeAndPush :: MockNode -> [Mock.TxEra] -> IO (CardanoBlock StandardCrypto)
forgeAndPush mn txs = do
  blk <- forgeNextBlock (mnChain mn) txs
  atomically $ MockServer.addBlock (mnServer mn) blk
  reseedStateQueryFromLedger (mnChain mn)
  pure blk

-- | Forge @n@ empty blocks back-to-back. Re-seeds the state-query
-- handle once after the whole batch (cheaper than per-block but
-- still keeps the handle fresh for downstream parsing).
forgeAndPushBlocks :: MockNode -> Int -> IO [CardanoBlock StandardCrypto]
forgeAndPushBlocks mn n = do
  blks <- replicateM n $ do
    blk <- forgeNextBlock (mnChain mn) []
    atomically $ MockServer.addBlock (mnServer mn) blk
    pure blk
  reseedStateQueryFromLedger (mnChain mn)
  pure blks

-- | Forge @n@ blocks containing payment transactions matching the
-- supplied 'Workload'. Re-seeds the state-query handle once after
-- the whole batch.
--
-- Each block's @wTxsPerBlock@ payment txs are built against the
-- /pre-block/ ledger state and each spends a distinct
-- @UTxOIndex i@, so the live UTxO map must hold at least
-- @wTxsPerBlock@ entries when a block is forged. Outputs of size
-- @wOutputLovelace@ go to freshly-derived addresses pulled from
-- the monotonic 'mnAddressCounter'; the change output goes back
-- to the spent UTxO's own address.
forgeAndPushBlocksWith
  :: MockNode
  -> Int        -- ^ number of blocks
  -> Workload
  -> IO [CardanoBlock StandardCrypto]
forgeAndPushBlocksWith mn n wl = do
  blks <- replicateM n $ do
    txs <- buildWorkloadTxs mn wl
    blk <- forgeNextBlock (mnChain mn) txs
    atomically $ MockServer.addBlock (mnServer mn) blk
    pure blk
  reseedStateQueryFromLedger (mnChain mn)
  pure blks

-- | Build one block worth of payment txs from the interpreter's
-- current ledger state. Advances 'mnAddressCounter' by
-- @wTxsPerBlock * wOutputsPerTx@.
buildWorkloadTxs :: MockNode -> Workload -> IO [Mock.TxEra]
buildWorkloadTxs mn wl = do
  let txsPerBlock  = wTxsPerBlock wl
      outsPerTx    = wOutputsPerTx wl
      totalFresh   = txsPerBlock * outsPerTx
  startIx <- atomicModifyIORef' (mnAddressCounter mn) $
    \c -> (c + totalFresh, c)
  Mock.withConwayLedgerState (mcInterpreter (mnChain mn)) $ \state' -> do
    payments <- forM [0 .. txsPerBlock - 1] $ \i -> do
      let txOutputs =
            [ ( Mock.UTxOAddress
                  (Addr Testnet (paymentCredentialAt addrIx) StakeRefNull)
              , valueFromList (Coin (wOutputLovelace wl)) []
              )
            | j <- [0 .. outsPerTx - 1]
            , let addrIx = startIx + i * outsPerTx + j
            ]
      Conway.mkPaymentTx'
        (Mock.UTxOIndex i)
        txOutputs
        (wFeeLovelace wl)
        0
        state'
    pure (map Mock.TxConway payments)

-- | Forge the bulk-credential stake-registration block. Prerequisite
-- for reward / delegation / governance scenarios.
forgeAndPushWithStakeCreds :: MockNode -> IO (CardanoBlock StandardCrypto)
forgeAndPushWithStakeCreds mn = do
  blk <- registerStakeCreds (mnChain mn)
  atomically $ MockServer.addBlock (mnServer mn) blk
  reseedStateQueryFromLedger (mnChain mn)
  pure blk

-- | Roll back the server-side chain DB to a point. A connected
-- dbsync receiver sees a @MsgRollBackward@ on its next poll.
rollbackMockNode
  :: MockNode
  -> Network.Point (CardanoBlock StandardCrypto)
  -> IO ()
rollbackMockNode mn point =
  atomically $ MockServer.rollback (mnServer mn) point

-- ---------------------------------------------------------------------------
-- * Inspection
-- ---------------------------------------------------------------------------

-- | The server's view of the chain tip.
currentTip :: MockNode -> IO (Network.Tip (CardanoBlock StandardCrypto))
currentTip mn = chainTip <$> atomically (MockServer.readChain (mnServer mn))

-- | Number of blocks the server holds.
currentChainLength :: MockNode -> IO Int
currentChainLength mn =
  chainLength <$> atomically (MockServer.readChain (mnServer mn))

chainTip
  :: MockChainTypes.Chain (CardanoBlock StandardCrypto)
  -> Tip (CardanoBlock StandardCrypto)
chainTip (MockChainTypes.Genesis _) = TipGenesis
chainTip (_ MockChainTypes.:> (b, _)) = Tip (blockSlot b) (blockHash b) (blockNo b)

chainLength :: MockChainTypes.Chain block -> Int
chainLength = go 0
  where
    go !n (MockChainTypes.Genesis _) = n
    go !n (rest MockChainTypes.:> _) = go (n + 1) rest

-- ---------------------------------------------------------------------------
-- * Lifecycle helpers
-- ---------------------------------------------------------------------------

-- | Restart the server thread on the same socket. The previous
-- thread is cancelled and the socket file removed before the new
-- thread re-binds.
restartMockNode :: MockNode -> IO ()
restartMockNode = MockServer.restartServer . mnServer

-- | Block until dbsync has registered as a new follower after a
-- 'restartMockNode'. Avoids tests racing ahead of the reconnect.
waitForReconnect :: MockNode -> IO ()
waitForReconnect = MockServer.waitForNextConnection . mnServer

-- ---------------------------------------------------------------------------
-- * Internal: temp socket lifecycle
-- ---------------------------------------------------------------------------

-- | Allocate a unique temp Unix socket path and remove it on exit.
withTempSocket :: (FilePath -> IO a) -> IO a
withTempSocket = bracket alloc cleanup
  where
    alloc = do
      nonce <- mkNonce
      pure ("/tmp" </> ("dbsync-test-" <> nonce <> ".sock"))

    cleanup path = do
      exists <- doesFileExist path
      when exists $
        removeFile path `catch` \(_ :: SomeException) -> pure ()

-- | A short alphanumeric suffix unique within this process. Combines
-- a monotonic-ish microsecond timestamp with a per-process counter so
-- nested 'withMockNode' calls in the same test never collide.
mkNonce :: IO [Char]
mkNonce = do
  now   <- getCurrentTime
  count <- atomicModifyIORef' nonceCounter $ \n -> (n + 1, n)
  let micros = floor (utcTimeToPOSIXSeconds now * 1_000_000) :: Integer
  pure (show micros <> "-" <> show count)

-- | Module-local counter for socket-name uniqueness.
nonceCounter :: IORef Int
nonceCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE nonceCounter #-}
