{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Genesis configuration reading.
--
-- Reads the four era genesis files (Byron, Shelley, Alonzo, Conway),
-- verifies their hashes, and builds the 'TopLevelConfig' needed for
-- ChainSync codecs to deserialize blocks from the node.
module DbSync.Config.Genesis
  ( -- * Types
    GenesisConfig (..)
  , ShelleyConfig (..)

    -- * Reading
  , readCardanoGenesisConfig

    -- * Building consensus config
  , mkTopLevelConfig
  , mkProtocolInfoCardano
  , mkProtocolInfoCardanoForging
  ) where

import Cardano.Prelude

import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.Update as Byron.Update
import qualified Cardano.Crypto as Crypto.Legacy
import Cardano.Crypto (decodeAbstractHash)
import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Slotting.Slot (EpochNo (..))
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis)
import qualified Cardano.Ledger.Api.Transition as Ledger
import Cardano.Ledger.Binary.Version (natVersion)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis)
import Cardano.Node.Protocol.Dijkstra (emptyDijkstraGenesis)
import Control.Monad.Trans.Except.Extra
  ( firstExceptT
  , handleIOExceptT
  , hoistEither
  )
import Control.Tracer (Tracer, nullTracer)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import Ouroboros.Consensus.Block.Forging (BlockForging, MkBlockForging (..))
import Ouroboros.Consensus.Cardano (Nonce (..), ProtVer (ProtVer))
import qualified Ouroboros.Consensus.Cardano as Consensus
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Cardano.Node
import Ouroboros.Consensus.Config (TopLevelConfig, emptyCheckpointsMap)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Protocol.Praos.AgentClient (KESAgentClientTrace)
import Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..), ShelleyLeaderCredentials)
import System.FilePath ((</>))

import DbSync.Config.Types
  ( ConfigError (..)
  , NetworkMagicConfig (..)
  , NodeConfig (..)
  )

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Combined genesis configuration for all eras.
data GenesisConfig = GenesisCardano
  { gcByron   :: !Byron.Config
  , gcShelley :: !ShelleyConfig
  , gcAlonzo  :: !AlonzoGenesis
  , gcConway  :: !ConwayGenesis
  }

-- | Shelley genesis config with its hash (needed for PRAOS nonce).
data ShelleyConfig = ShelleyConfig
  { scConfig     :: !ShelleyGenesis
  , scGenesisHash :: !(Crypto.Hash Crypto.Blake2b_256 ByteString)
  }

-- ---------------------------------------------------------------------------
-- * Reading genesis configs
-- ---------------------------------------------------------------------------

-- | Read all four genesis files and combine into a 'GenesisConfig'.
-- The @FilePath@ is the directory containing the genesis files
-- (derived from the db-sync-config.json / node config location).
readCardanoGenesisConfig
  :: NodeConfig
  -> FilePath       -- ^ Directory containing genesis files
  -> IO (Either ConfigError GenesisConfig)
readCardanoGenesisConfig nc genesisDir = runExceptT $
  GenesisCardano
    <$> readByronGenesis nc genesisDir
    <*> readShelleyGenesis nc genesisDir
    <*> readAlonzoGenesis nc genesisDir
    <*> readConwayGenesis nc genesisDir

-- ---------------------------------------------------------------------------
-- * Building consensus config
-- ---------------------------------------------------------------------------

-- | Build the 'TopLevelConfig' from genesis data.
-- This gives us the codecs needed for ChainSync deserialization.
mkTopLevelConfig :: NodeConfig -> GenesisConfig -> TopLevelConfig (CardanoBlock StandardCrypto)
mkTopLevelConfig nc gc = Consensus.pInfoConfig $ mkProtocolInfoCardano nc gc

-- | Build the 'ProtocolInfo' from genesis data, with no leader
-- credentials. This is the production sync-only path.
mkProtocolInfoCardano
  :: NodeConfig
  -> GenesisConfig
  -> ProtocolInfo (CardanoBlock StandardCrypto)
mkProtocolInfoCardano nc gc =
  fst (second (\f -> f (nullTracer :: Tracer IO KESAgentClientTrace)) $
    protocolInfoCardano (cardanoProtocolParams nc gc []))

-- | Build the 'ProtocolInfo' /and/ resolve the matching
-- 'BlockForging' actions, given a list of Shelley leader
-- credentials.
--
-- Used by the test harness ('DbSync.Test.MockChain') to drive the
-- vendored chain-gen interpreter. Production sync uses the
-- credential-free 'mkProtocolInfoCardano' path above.
mkProtocolInfoCardanoForging
  :: NodeConfig
  -> GenesisConfig
  -> [ShelleyLeaderCredentials StandardCrypto]
  -> IO ( ProtocolInfo (CardanoBlock StandardCrypto)
        , [BlockForging IO (CardanoBlock StandardCrypto)]
        )
mkProtocolInfoCardanoForging nc gc creds = do
  let (pinfo, mkForgings) = protocolInfoCardano (cardanoProtocolParams nc gc creds)
  -- 'mkForgings' yields one 'MkBlockForging' per credential set;
  -- each one is an action allocating a 'BlockForging' (and any
  -- per-key resources like KES HotKeys). Tests are short-lived so
  -- we don't bother finalising on teardown — the OS reclaims on exit.
  mkForgingActions <- mkForgings (nullTracer :: Tracer IO KESAgentClientTrace)
  forgings <- traverse mkBlockForging mkForgingActions
  pure (pinfo, forgings)

-- | Shared 'CardanoProtocolParams' construction. The credentials
-- argument is what distinguishes the sync-only path (empty) from the
-- forging-test path (populated from a bulk credentials file).
cardanoProtocolParams
  :: NodeConfig
  -> GenesisConfig
  -> [ShelleyLeaderCredentials StandardCrypto]
  -> CardanoProtocolParams StandardCrypto
cardanoProtocolParams nc gc creds =
  CardanoProtocolParams
    { byronProtocolParams =
        Consensus.ProtocolParamsByron
          { Consensus.byronGenesis = gcByron gc
          , Consensus.byronPbftSignatureThreshold = Nothing
          , Consensus.byronProtocolVersion = Byron.Update.ProtocolVersion 0 2 0
          , Consensus.byronSoftwareVersion = mkByronSoftwareVersion
          , Consensus.byronLeaderCredentials = Nothing
          }
    , shelleyBasedProtocolParams =
        Consensus.ProtocolParamsShelleyBased
          { Consensus.shelleyBasedInitialNonce = shelleyPraosNonce (scGenesisHash $ gcShelley gc)
          , Consensus.shelleyBasedLeaderCredentials = creds
          }
    , cardanoProtocolVersion = ProtVer (natVersion @10) 0
    , cardanoLedgerTransitionConfig =
        Ledger.mkLatestTransitionConfig
          (scConfig $ gcShelley gc)
          (gcAlonzo gc)
          (gcConway gc)
          emptyDijkstraGenesis
    , cardanoHardForkTriggers = mkHardForkTriggers nc
    , cardanoCheckpoints = emptyCheckpointsMap
    }

-- ---------------------------------------------------------------------------
-- * Internal: per-era genesis readers
-- ---------------------------------------------------------------------------

-- | Read Byron genesis. Uses cardano-crypto's 'decodeAbstractHash' to parse
-- the hash from the text in our NodeConfig, then 'Byron.mkConfigFromFile'.
readByronGenesis :: NodeConfig -> FilePath -> ExceptT ConfigError IO Byron.Config
readByronGenesis nc genesisDir = do
  let file = genesisDir </> ncByronGenesisFile nc
  genHash <-
    firstExceptT (\e -> ConfigParseError $ "Byron genesis hash decode error: " <> show e)
      . hoistEither
      $ decodeAbstractHash (ncByronGenesisHash nc)
  let requiresMagic = toRequiresNetworkMagic (ncRequiresNetworkMagic nc)
  firstExceptT (\e -> ConfigParseError $ "Byron genesis error in " <> toS file <> ": " <> show e)
    $ Byron.mkConfigFromFile requiresMagic file genHash

-- | Read Shelley genesis — read bytes, hash, decode JSON.
readShelleyGenesis :: NodeConfig -> FilePath -> ExceptT ConfigError IO ShelleyConfig
readShelleyGenesis nc genesisDir = do
  let file = genesisDir </> ncShelleyGenesisFile nc
  content <- readFileOrError "Shelley" file
  let genesisHash = Crypto.hashWith identity content
  genesis <- decodeJsonOrError "Shelley" file content
  pure $ ShelleyConfig genesis genesisHash

-- | Read Alonzo genesis — read bytes, hash, decode JSON.
readAlonzoGenesis :: NodeConfig -> FilePath -> ExceptT ConfigError IO AlonzoGenesis
readAlonzoGenesis nc genesisDir = do
  let file = genesisDir </> ncAlonzoGenesisFile nc
  content <- readFileOrError "Alonzo" file
  decodeJsonOrError "Alonzo" file content

-- | Read Conway genesis — read bytes, hash, decode JSON.
readConwayGenesis :: NodeConfig -> FilePath -> ExceptT ConfigError IO ConwayGenesis
readConwayGenesis nc genesisDir = do
  let file = genesisDir </> ncConwayGenesisFile nc
  content <- readFileOrError "Conway" file
  decodeJsonOrError "Conway" file content

-- ---------------------------------------------------------------------------
-- * Internal: helpers
-- ---------------------------------------------------------------------------

-- | Read a file, wrapping IO errors in 'ConfigError'.
readFileOrError :: Text -> FilePath -> ExceptT ConfigError IO ByteString
readFileOrError eraName file =
  handleIOExceptT
    (\e -> ConfigParseError $ eraName <> " genesis read error (" <> toS file <> "): " <> show e)
    (BS.readFile file)

-- | JSON-decode a ByteString, wrapping decode errors in 'ConfigError'.
decodeJsonOrError :: (Aeson.FromJSON a) => Text -> FilePath -> ByteString -> ExceptT ConfigError IO a
decodeJsonOrError eraName file content =
  firstExceptT
    (\e -> ConfigParseError $ eraName <> " genesis decode error (" <> toS file <> "): " <> toS e)
    . hoistEither
    $ Aeson.eitherDecodeStrict' content

-- | Convert our 'NetworkMagicConfig' to cardano-crypto's 'RequiresNetworkMagic'.
toRequiresNetworkMagic :: NetworkMagicConfig -> Crypto.Legacy.RequiresNetworkMagic
toRequiresNetworkMagic RequiresNoMagic = Crypto.Legacy.RequiresNoMagic
toRequiresNetworkMagic RequiresMagic   = Crypto.Legacy.RequiresMagic

-- | Map our optional hard fork epoch fields to consensus 'CardanoHardForkTrigger' types.
-- On mainnet (all Nothing), every trigger defaults to 'AtDefaultVersion'.
-- On testnets, specific epochs can be set.
mkHardForkTriggers :: NodeConfig -> Consensus.CardanoHardForkTriggers
mkHardForkTriggers nc =
  Consensus.CardanoHardForkTriggers'
    { triggerHardForkShelley  = toTrigger (ncTestShelleyHardForkAtEpoch nc)
    , triggerHardForkAllegra  = toTrigger (ncTestAllegraHardForkAtEpoch nc)
    , triggerHardForkMary     = toTrigger (ncTestMaryHardForkAtEpoch nc)
    , triggerHardForkAlonzo   = toTrigger (ncTestAlonzoHardForkAtEpoch nc)
    , triggerHardForkBabbage  = toTrigger (ncTestBabbageHardForkAtEpoch nc)
    , triggerHardForkConway   = toTrigger (ncTestConwayHardForkAtEpoch nc)
    , triggerHardForkDijkstra = CardanoTriggerHardForkAtDefaultVersion
    }
  where
    toTrigger :: Maybe Word64 -> CardanoHardForkTrigger blk
    toTrigger Nothing      = CardanoTriggerHardForkAtDefaultVersion
    toTrigger (Just epoch) = CardanoTriggerHardForkAtEpoch (EpochNo epoch)

-- | Derive the PRAOS nonce from the Shelley genesis hash.
shelleyPraosNonce :: Crypto.Hash Crypto.Blake2b_256 ByteString -> Nonce
shelleyPraosNonce hsh = Nonce (Crypto.castHash hsh)

-- | Byron software version required by protocol params.
mkByronSoftwareVersion :: Byron.Update.SoftwareVersion
mkByronSoftwareVersion = Byron.Update.SoftwareVersion (Byron.Update.ApplicationName "cardano-sl") 1
