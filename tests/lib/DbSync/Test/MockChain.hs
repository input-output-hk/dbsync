{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Multi-block / multi-epoch test harness for the new dbsync.
--
-- Bootstraps a forging 'Interpreter' (vendored in 'dbsync-mock' from
-- the upstream @cardano-chain-gen@ project) over a Conway-era test
-- config, then drives forged 'CardanoBlock's through our existing
-- 'parseBlock' + 'Follow.processBlocks' pipeline. The resulting rows
-- end up in the test PostgreSQL database, where assertion helpers
-- can query them.
--
-- Two-stage flow per scenario:
--
--   1. /Forge/ — hand-pick blocks (txs + skipped slots + epoch
--      boundaries) via the interpreter API. The interpreter advances
--      a real ledger state, so reward/stake/governance side effects
--      that lag two epochs behind their triggering tx still surface
--      correctly.
--   2. /Process/ — parse each forged 'CardanoBlock' via 'parseBlock'
--      and feed the resulting 'GenericBlock' into our extractor
--      pipeline. PG rows accumulate the same way they would during a
--      real chain sync, just without the network plumbing.
--
-- Skips the chain-sync layer entirely. A future iteration can plug
-- the vendored 'Cardano.Mock.ChainSync.Server' between the
-- interpreter and our 'DbSync.Node.Connection' for socket-level
-- coverage; the present harness is the minimum that gets us
-- ledger-derived data (rewards, epoch_stake, ada_pots) into PG.
module DbSync.Test.MockChain
  ( -- * Environment
    MockChain (..)
  , withMockChain

    -- * Forging
  , forgeNextBlock
  , forgeNextBlocks
  , forgeUntilNextEpoch
  , fillEpochs
  , registerStakeCreds

    -- * Realistic block content
  , RealisticBlockShape (..)
  , mainnetAverageShape
  , buildRealisticTxs

    -- * Pipeline integration
  , parseAndProcess
  , reseedStateQueryFromLedger

    -- * Helpers
  , currentEpochNo
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import System.FilePath ((</>))

import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Slotting.Slot as Slot
import qualified Cardano.Slotting.Time as Slot

import Cardano.Crypto.Init (cryptoInit)

import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Config (TopLevelConfig)
import Ouroboros.Consensus.HardFork.Combinator.Mempool ()
import Ouroboros.Consensus.Shelley.Node (ShelleyLeaderCredentials, sgNetworkId, sgSystemStart)
import qualified Ouroboros.Network.Block as Network

import qualified Cardano.Node.Protocol.Shelley as NodeShelley
import Cardano.Node.Types (ProtocolFilepaths (..))

import qualified Cardano.Mock.Forging.Interpreter as Mock
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import qualified Cardano.Mock.Forging.Types as Mock

import DbSync.Block.Parser (parseBlock)
import DbSync.Config.Genesis
  ( GenesisConfig (..)
  , ShelleyConfig (..)
  , mkProtocolInfoCardanoForging
  , mkTopLevelConfig
  , readCardanoGenesisConfig
  )
import DbSync.Config.Node (parseNodeConfig)
import DbSync.Config.Types (NodeConfig (..))
import DbSync.Extractor (ExtractorDef)
import DbSync.Extractor (emptyBlockLedgerData)
import DbSync.Block.Pipeline (processBlock)
import DbSync.Db.Phase (SyncPhase (..))
import DbSync.Phase.Following.Resolver (mkFollowResolver)
import DbSync.Test.PipelineEnv (mkTestPipelineEnvWith)
import qualified DbSync.Phase.Following.Writer as FollowingWriter
import DbSync.StateQuery
  ( StateQueryVar
  , getSlotDetailsIO
  , newStateQueryVar
  , seedInterpreterFromLedgerState
  )
import DbSync.Trace.Backend (mkNullTracer)

-- ---------------------------------------------------------------------------
-- * Environment
-- ---------------------------------------------------------------------------

-- | Everything a forging-and-processing scenario needs.
--
-- 'mcInterpreter' is the upstream chain-gen mock interpreter, which
-- holds the live ledger state and forging credentials. 'mcStateQueryVar'
-- is seeded from that ledger state on every 'parseAndProcess' call so
-- our 'parseBlock' can compute correct 'SlotDetails' (epoch, slot,
-- time) without round-tripping to a node.
data MockChain = MockChain
  { mcInterpreter    :: !Mock.Interpreter
  , mcNodeConfig     :: !NodeConfig
  , mcGenesisConfig  :: !GenesisConfig
  , mcTopLevelConfig :: !(TopLevelConfig (CardanoBlock StandardCrypto))
  , mcSystemStart    :: !Slot.SystemStart
  , mcStateQueryVar  :: !StateQueryVar
  , mcNetwork        :: !Ledger.Network
  }

-- | Bracketed setup — load genesis from @configDir@, build the Cardano
-- 'ProtocolInfo' (with leader credentials read from
-- @configDir/pools/bulk1.creds@), and initialise a forging
-- 'Interpreter'. The same call would also wire a chain-sync server
-- once we add the socket layer.
--
-- The vendored @config-conway@ fixtures (in
-- @tests/data/config-conway@) supply this; tests just pass
-- that path.
withMockChain :: FilePath -> (MockChain -> IO a) -> IO a
withMockChain configDir action = do
  -- libsodium initialisation. KES key operations segfault without
  -- this; matches the upstream 'withConfig' setup.
  cryptoInit

  -- 1. Load the cardano-node config + genesis files via our own loader.
  nodeCfg <- loadNodeConfig (configDir </> "test-config.json")
  genesisCfg <- loadGenesis nodeCfg configDir

  -- 2. Read the bulk credentials file vendored next to the genesis.
  -- The interpreter needs at least one set of leader credentials so
  -- it can pick a slot leader and forge a block.
  shelleyCreds <- loadShelleyCredentials configDir

  -- 3. Build a forging protocol info threaded with those credentials.
  -- Same genesis values as the production sync path; only the
  -- credential list differs.
  (pinfo, forgings) <- mkProtocolInfoCardanoForging nodeCfg genesisCfg shelleyCreds

  let topLevelCfg = mkTopLevelConfig nodeCfg genesisCfg
      systemStart = Slot.SystemStart (sgSystemStart (scConfig (gcShelley genesisCfg)))
      network     = sgNetworkId (scConfig (gcShelley genesisCfg))

  -- 4. Initialise the interpreter. It owns the mock chain DB and
  -- ledger state from now on.
  interpreter <- Mock.initInterpreter pinfo forgings mempty Nothing

  -- 5. Pre-seed our 'StateQueryVar' from the interpreter's initial
  -- ledger state. Keeps 'getSlotDetailsIO' off the (non-existent)
  -- network when we parse forged blocks.
  sqv <- newStateQueryVar topLevelCfg
  initState <- Mock.getCurrentLedgerState interpreter
  seedInterpreterFromLedgerState topLevelCfg initState sqv

  action MockChain
    { mcInterpreter    = interpreter
    , mcNodeConfig     = nodeCfg
    , mcGenesisConfig  = genesisCfg
    , mcTopLevelConfig = topLevelCfg
    , mcSystemStart    = systemStart
    , mcStateQueryVar  = sqv
    , mcNetwork        = network
    }

-- ---------------------------------------------------------------------------
-- * Forging
-- ---------------------------------------------------------------------------

-- | Forge the next block at whichever slot the next leader is.
-- Empty txs gives an empty block. Wraps 'Mock.forgeNextFindLeader'.
forgeNextBlock :: MockChain -> [Mock.TxEra] -> IO (CardanoBlock StandardCrypto)
forgeNextBlock mc = Mock.forgeNextFindLeader (mcInterpreter mc)

-- | Forge @n@ empty blocks. Equivalent to upstream's
-- 'Api.forgeAndSubmitBlocks' minus the chain-sync submit step.
forgeNextBlocks :: MockChain -> Int -> IO [CardanoBlock StandardCrypto]
forgeNextBlocks mc n = replicateM n (forgeNextBlock mc [])

-- | Forge empty blocks until the interpreter ticks over to the next
-- epoch (and includes the first block of that next epoch). Mirrors
-- upstream's 'Api.fillUntilNextEpoch' shape — useful for walking the
-- chain to a known epoch boundary.
forgeUntilNextEpoch :: MockChain -> IO [CardanoBlock StandardCrypto]
forgeUntilNextEpoch mc = do
  startEpoch <- Mock.getCurrentEpoch (mcInterpreter mc)
  go startEpoch []
  where
    go startEpoch acc = do
      blk <- forgeNextBlock mc []
      currentEpoch <- Mock.getCurrentEpoch (mcInterpreter mc)
      if currentEpoch == startEpoch
        then go startEpoch (blk : acc)
        else pure (reverse (blk : acc))

-- | Forge enough blocks to cross @n@ epoch boundaries.
fillEpochs :: MockChain -> Int -> IO [CardanoBlock StandardCrypto]
fillEpochs mc n = concat <$> replicateM n (forgeUntilNextEpoch mc)

-- | Forge a single block containing a stake-credential registration
-- for every test stake key. Mirrors upstream's
-- 'Api.registerAllStakeCreds'. The credentials registered here are
-- the prerequisite for any reward/delegation/governance scenario.
registerStakeCreds :: MockChain -> IO (CardanoBlock StandardCrypto)
registerStakeCreds mc = Mock.forgeWithStakeCreds (mcInterpreter mc)

-- ---------------------------------------------------------------------------
-- * Realistic block content
-- ---------------------------------------------------------------------------

-- | Descriptor for the realistic-block builder. Sized to roughly
-- match a mid-traffic mainnet block.
--
-- Each payment tx is a "rename": one input UTxO is consumed and one
-- change output at the same address is produced. No fresh target
-- outputs, so the UTxO set's value distribution stays predictable
-- across blocks (every entry holds roughly @genesisValue − N·fees@
-- after the @N@th touch). That's the property the perf test
-- depends on — small "target" outputs would otherwise drain to
-- negative change once a later block re-picked them.
--
-- The shape exists as a record so future enrichments (delegations,
-- multi-asset mints, governance txs) can be added as fields without
-- breaking call sites once the corresponding forging primitives no
-- longer require carrying state across blocks.
data RealisticBlockShape = RealisticBlockShape
  { rbsPaymentTxCount :: !Int
    -- ^ Payment txs per block. Bounded by the size of the live UTxO
    -- map at the time the block is built; on the Conway test genesis
    -- that starts at 10 and grows by zero per block (one in, one out
    -- per tx), so 10 is the safe ceiling.
  }
  deriving stock (Eq, Show)

-- | Default shape: 10 payment txs per block.
--
-- Per block: 10 tx rows, 10 tx_in rows, 10 tx_out rows (change only).
-- Comparable to a mid-traffic mainnet block in tx count and a clear
-- step up from the empty-block 'FollowPerfSpec'. Address diversity
-- is intentionally low (output goes back to source address) so the
-- UTxO set stays bounded; mainnet-shape address diversity needs a
-- richer forging primitive that's tracked in @FOLLOW-PERF.md@.
mainnetAverageShape :: RealisticBlockShape
mainnetAverageShape =
  RealisticBlockShape
    { rbsPaymentTxCount = 10
    }

-- | Per-tx fee in lovelace. Above the @minFeeA = 1@ per byte
-- minimum for a ~200-byte payment tx with comfortable slack.
realisticTxFee :: Integer
realisticTxFee = 1000

-- | Build the tx list for one realistic block against the
-- interpreter's current ledger state. Resolves all UTxO indices
-- inside a single 'withConwayLedgerState' so the txs share a
-- consistent snapshot — each spends a distinct @UTxOIndex@ value
-- and produces a same-address change output, so the live UTxO map
-- stays balanced across iterations.
buildRealisticTxs :: MockChain -> RealisticBlockShape -> IO [Mock.TxEra]
buildRealisticTxs mc shape =
  Mock.withConwayLedgerState (mcInterpreter mc) $ \state' -> do
    payments <- forM [0 .. rbsPaymentTxCount shape - 1] $ \i ->
      Conway.mkPaymentTx'
        (Mock.UTxOIndex i)
        []                          -- no fresh targets; change only
        realisticTxFee
        0                           -- no donation
        state'
    pure (map Mock.TxConway payments)

-- ---------------------------------------------------------------------------
-- * Pipeline integration
-- ---------------------------------------------------------------------------

-- | Parse each block and run it through the FollowingChainTip
-- pipeline against @conn@.
--
-- Re-seeds 'mcStateQueryVar' before each block to pick up any
-- new-era / new-epoch info introduced by the previous step. With the
-- seed always derived from the interpreter's live ledger state, no
-- network traffic is required.
parseAndProcess
  :: Conn.Connection
  -> MockChain
  -> [ExtractorDef]
  -> [CardanoBlock StandardCrypto]
  -> IO ()
parseAndProcess conn mc extractors blocks = do
  genericBlocks <- traverse toGeneric blocks
  resolver <- mkFollowResolver conn
  let writer = FollowingWriter.mkWriter conn
      env    =
        mkTestPipelineEnvWith
          (mcNetwork mc)
          resolver
          writer
          extractors
          (\_ -> pure emptyBlockLedgerData)
          FollowingChainTip
  for_ genericBlocks $ \gb -> runReaderT (processBlock gb) env
  where
    toGeneric blk = do
      latest <- Mock.getCurrentLedgerState (mcInterpreter mc)
      seedInterpreterFromLedgerState (mcTopLevelConfig mc) latest (mcStateQueryVar mc)
      slotDetails <- getSlotDetailsIO
        mkNullTracer
        (mcStateQueryVar mc)
        (mcSystemStart mc)
        (Network.blockSlot blk)
      pure (parseBlock slotDetails blk)

-- | Re-seed 'mcStateQueryVar' from the interpreter's current ledger
-- state. The seeded interpreter forecast only covers a bounded
-- window of future slots; call this after a batch of forging to
-- ensure later 'parseBlock' calls don't fall outside that window.
reseedStateQueryFromLedger :: MockChain -> IO ()
reseedStateQueryFromLedger mc = do
  latest <- Mock.getCurrentLedgerState (mcInterpreter mc)
  seedInterpreterFromLedgerState
    (mcTopLevelConfig mc) latest (mcStateQueryVar mc)

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | The current chain-tip epoch number, as known to the interpreter.
currentEpochNo :: MockChain -> IO Slot.EpochNo
currentEpochNo = Mock.getCurrentEpoch . mcInterpreter

-- ---------------------------------------------------------------------------
-- * Internal: config loading
-- ---------------------------------------------------------------------------

loadNodeConfig :: FilePath -> IO NodeConfig
loadNodeConfig fp = do
  result <- parseNodeConfig fp
  case result of
    Left err -> panic $ "MockChain: failed to load node config " <> show fp <> ": " <> show err
    Right cfg -> pure cfg

loadGenesis :: NodeConfig -> FilePath -> IO GenesisConfig
loadGenesis nc dir = do
  result <- readCardanoGenesisConfig nc dir
  case result of
    Left err -> panic $ "MockChain: failed to load genesis from " <> show dir <> ": " <> show err
    Right gc -> pure gc

-- | Read the Shelley bulk credentials from the vendored test
-- fixtures. The interpreter requires at least one credential set
-- so it has someone to nominate as slot leader.
loadShelleyCredentials
  :: FilePath
  -> IO [ShelleyLeaderCredentials StandardCrypto]
loadShelleyCredentials configDir = do
  let bulkFile = configDir </> "pools" </> "bulk1.creds"
      pfp = ProtocolFilepaths
        { byronCertFile = Nothing
        , byronKeyFile = Nothing
        , shelleyKESSource = Nothing
        , shelleyVRFFile = Nothing
        , shelleyCertFile = Nothing
        , shelleyBulkCredsFile = Just bulkFile
        }
  result <- runExceptT (NodeShelley.readLeaderCredentials (Just pfp))
  case result of
    Left err -> panic $
      "MockChain: failed to read leader credentials from "
        <> show bulkFile <> ": " <> show err
    Right creds -> pure creds
