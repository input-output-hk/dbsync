{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Reads the four era genesis files, verifies their hashes, and
-- builds the 'TopLevelConfig' the ChainSync codecs need to
-- deserialise blocks.
module DbSync.App.Config.Genesis
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

import DbSync.App.Config.Types
  ( ConfigError (..)
  , NetworkMagicConfig (..)
  , NodeConfig (..)
  )

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

data GenesisConfig = GenesisCardano
  { gcByron   :: !Byron.Config
  , gcShelley :: !ShelleyConfig
  , gcAlonzo  :: !AlonzoGenesis
  , gcConway  :: !ConwayGenesis
  }

-- | The hash seeds the PRAOS initial nonce.
data ShelleyConfig = ShelleyConfig
  { scConfig     :: !ShelleyGenesis
  , scGenesisHash :: !(Crypto.Hash Crypto.Blake2b_256 ByteString)
  }

-- ---------------------------------------------------------------------------
-- * Reading
-- ---------------------------------------------------------------------------

readCardanoGenesisConfig
  :: NodeConfig
  -> FilePath       -- ^ Directory containing the genesis files
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

-- | Carries the codecs ChainSync needs to deserialise blocks.
mkTopLevelConfig :: NodeConfig -> GenesisConfig -> TopLevelConfig (CardanoBlock StandardCrypto)
mkTopLevelConfig nc gc = Consensus.pInfoConfig $ mkProtocolInfoCardano nc gc

-- | The production sync-only path: no leader credentials.
mkProtocolInfoCardano
  :: NodeConfig
  -> GenesisConfig
  -> ProtocolInfo (CardanoBlock StandardCrypto)
mkProtocolInfoCardano nc gc =
  fst (second (\f -> f (nullTracer :: Tracer IO KESAgentClientTrace)) $
    protocolInfoCardano (cardanoProtocolParams nc gc []))

-- | Test-only. Also resolves the 'BlockForging' actions for the given
-- Shelley leader credentials.
mkProtocolInfoCardanoForging
  :: NodeConfig
  -> GenesisConfig
  -> [ShelleyLeaderCredentials StandardCrypto]
  -> IO ( ProtocolInfo (CardanoBlock StandardCrypto)
        , [BlockForging IO (CardanoBlock StandardCrypto)]
        )
mkProtocolInfoCardanoForging nc gc creds = do
  let (pinfo, mkForgings) = protocolInfoCardano (cardanoProtocolParams nc gc creds)
  -- Each 'MkBlockForging' allocates per-key resources (KES HotKeys).
  -- Tests are short-lived so we skip teardown; the OS reclaims on exit.
  mkForgingActions <- mkForgings (nullTracer :: Tracer IO KESAgentClientTrace)
  forgings <- traverse mkBlockForging mkForgingActions
  pure (pinfo, forgings)

-- | An empty credentials list gives the sync-only path; a populated
-- one gives the forging-test path.
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

-- | Byron parses its own hash with 'decodeAbstractHash' and goes
-- through 'Byron.mkConfigFromFile', not a plain JSON decode.
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

-- | Blake2b_256-hashes the file contents; the digest seeds the PRAOS
-- initial nonce.
readShelleyGenesis :: NodeConfig -> FilePath -> ExceptT ConfigError IO ShelleyConfig
readShelleyGenesis nc genesisDir = do
  let file = genesisDir </> ncShelleyGenesisFile nc
  content <- readFileOrError "Shelley" file
  let genesisHash = Crypto.hashWith identity content
  genesis <- decodeJsonOrError "Shelley" file content
  pure $ ShelleyConfig genesis genesisHash

-- | Plain JSON decode; no hash required.
readAlonzoGenesis :: NodeConfig -> FilePath -> ExceptT ConfigError IO AlonzoGenesis
readAlonzoGenesis nc genesisDir = do
  let file = genesisDir </> ncAlonzoGenesisFile nc
  content <- readFileOrError "Alonzo" file
  decodeJsonOrError "Alonzo" file content

-- | Plain JSON decode; no hash required.
readConwayGenesis :: NodeConfig -> FilePath -> ExceptT ConfigError IO ConwayGenesis
readConwayGenesis nc genesisDir = do
  let file = genesisDir </> ncConwayGenesisFile nc
  content <- readFileOrError "Conway" file
  decodeJsonOrError "Conway" file content

-- ---------------------------------------------------------------------------
-- * Internal: helpers
-- ---------------------------------------------------------------------------

readFileOrError :: Text -> FilePath -> ExceptT ConfigError IO ByteString
readFileOrError eraName file =
  handleIOExceptT
    (\e -> ConfigParseError $ eraName <> " genesis read error (" <> toS file <> "): " <> show e)
    (BS.readFile file)

decodeJsonOrError :: (Aeson.FromJSON a) => Text -> FilePath -> ByteString -> ExceptT ConfigError IO a
decodeJsonOrError eraName file content =
  firstExceptT
    (\e -> ConfigParseError $ eraName <> " genesis decode error (" <> toS file <> "): " <> toS e)
    . hoistEither
    $ Aeson.eitherDecodeStrict' content

toRequiresNetworkMagic :: NetworkMagicConfig -> Crypto.Legacy.RequiresNetworkMagic
toRequiresNetworkMagic RequiresNoMagic = Crypto.Legacy.RequiresNoMagic
toRequiresNetworkMagic RequiresMagic   = Crypto.Legacy.RequiresMagic

-- | On mainnet every field is 'Nothing', so every trigger falls back
-- to 'CardanoTriggerHardForkAtDefaultVersion'. Testnets set epochs.
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

shelleyPraosNonce :: Crypto.Hash Crypto.Blake2b_256 ByteString -> Nonce
shelleyPraosNonce hsh = Nonce (Crypto.castHash hsh)

mkByronSoftwareVersion :: Byron.Update.SoftwareVersion
mkByronSoftwareVersion = Byron.Update.SoftwareVersion (Byron.Update.ApplicationName "cardano-sl") 1
