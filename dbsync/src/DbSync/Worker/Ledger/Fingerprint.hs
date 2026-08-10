{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Chain-identity fingerprint for the @dbsync-ledger/@ directory.
--
-- A small JSON file, written on first init and checked on every boot.
-- A refusal to start on a mismatch of network magic or system start
-- stops a stale ledger directory corrupting a different chain.
module DbSync.Worker.Ledger.Fingerprint
  ( -- * Type
    LedgerStateFingerprint (..)
  , currentFormatVersion

    -- * Check result
  , FingerprintCheck (..)

    -- * Operations
  , fingerprintPath
  , computeFingerprint
  , checkFingerprint
  , writeFingerprint

    -- * Rendering
  , renderFingerprint
  ) where

import Cardano.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Time.Clock (UTCTime)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.FilePath ((</>))

import Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))

import DbSync.App.Config.Genesis (GenesisConfig (..), ShelleyConfig (..))

-- ---------------------------------------------------------------------------
-- * Type
-- ---------------------------------------------------------------------------

-- | The chain identity baked into a @dbsync-ledger/@ directory.
--
-- 'lsfFormatVersion' surfaces a future schema change as a clean
-- mismatch rather than a JSON-parse error.
data LedgerStateFingerprint = LedgerStateFingerprint
  { lsfFormatVersion :: !Word8
  , lsfNetworkMagic  :: !Word32
  , lsfSystemStart   :: !UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON LedgerStateFingerprint where
  toJSON fp = Aeson.object
    [ "format_version" Aeson..= lsfFormatVersion fp
    , "network_magic"  Aeson..= lsfNetworkMagic fp
    , "system_start"   Aeson..= lsfSystemStart fp
    ]

instance Aeson.FromJSON LedgerStateFingerprint where
  parseJSON = Aeson.withObject "LedgerStateFingerprint" $ \o ->
    LedgerStateFingerprint
      <$> o Aeson..: "format_version"
      <*> o Aeson..: "network_magic"
      <*> o Aeson..: "system_start"

-- | Format version this build writes. A different value on disk
-- reports as a mismatch.
currentFormatVersion :: Word8
currentFormatVersion = 1

-- ---------------------------------------------------------------------------
-- * Check result
-- ---------------------------------------------------------------------------

-- | Outcome of comparing the on-disk fingerprint to the expected one.
data FingerprintCheck
  = FingerprintFresh
    -- ^ Ledger directory empty or absent. The caller writes the
    -- fingerprint after ledger init succeeds.
  | FingerprintMatch
    -- ^ On-disk identity equals the expected one; boot continues.
  | FingerprintMismatch !LedgerStateFingerprint !LedgerStateFingerprint
    -- ^ On-disk fingerprint, then expected fingerprint.
  | FingerprintMissing !FilePath
    -- ^ Directory has content but no readable fingerprint file.
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Operations
-- ---------------------------------------------------------------------------

fingerprintPath :: FilePath -> FilePath
fingerprintPath dir = dir </> "state.fingerprint.json"

-- | The fingerprint a fresh @dbsync-ledger/@ should carry for this
-- genesis config.
computeFingerprint :: GenesisConfig -> LedgerStateFingerprint
computeFingerprint genesisCfg =
  let sg = scConfig (gcShelley genesisCfg)
  in LedgerStateFingerprint
       { lsfFormatVersion = currentFormatVersion
       , lsfNetworkMagic  = sgNetworkMagic sg
       , lsfSystemStart   = sgSystemStart sg
       }

-- | Compare the on-disk fingerprint against the expected value.
-- Unreadable JSON reports 'FingerprintMissing', so the operator sees
-- one message for both cases.
checkFingerprint :: FilePath -> LedgerStateFingerprint -> IO FingerprintCheck
checkFingerprint dir expected = do
  dirExists <- doesDirectoryExist dir
  if not dirExists
    then pure FingerprintFresh
    else do
      let fpPath = fingerprintPath dir
      hasFile <- doesFileExist fpPath
      if hasFile
        then do
          eDecoded <- Aeson.eitherDecode <$> BSL.readFile fpPath
          case eDecoded of
            Right (onDisk :: LedgerStateFingerprint)
              | onDisk == expected -> pure FingerprintMatch
              | otherwise          -> pure $ FingerprintMismatch onDisk expected
            Left _ -> pure $ FingerprintMissing dir
        else do
          entries <- listDirectory dir
          if null entries
            then pure FingerprintFresh
            else pure $ FingerprintMissing dir

-- | Write the fingerprint to disk. Creates the directory if it is absent.
writeFingerprint :: FilePath -> LedgerStateFingerprint -> IO ()
writeFingerprint dir fp = do
  createDirectoryIfMissing True dir
  BSL.writeFile (fingerprintPath dir) (Aeson.encode fp)

-- ---------------------------------------------------------------------------
-- * Rendering
-- ---------------------------------------------------------------------------

-- | One-line render for operator-facing error messages.
renderFingerprint :: LedgerStateFingerprint -> Text
renderFingerprint fp =
  "format_version=" <> show (lsfFormatVersion fp)
    <> ", network_magic=" <> show (lsfNetworkMagic fp)
    <> ", system_start=" <> show (lsfSystemStart fp)
