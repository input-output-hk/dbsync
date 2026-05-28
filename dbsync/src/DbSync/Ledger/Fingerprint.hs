{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Chain-identity fingerprint for the @dbsync-ledger/@ directory.
--
-- A small JSON file written on first init and checked on every
-- subsequent boot. Refusing to start when the on-disk identity
-- (network magic + system start) doesn't match the current config
-- prevents silent corruption from pointing a stale ledger dir at a
-- different chain.
module DbSync.Ledger.Fingerprint
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
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.FilePath ((</>))

import Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))

import DbSync.Config.Genesis (GenesisConfig (..), ShelleyConfig (..))

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
    -- ^ Ledger directory empty or absent; the caller writes the
    -- fingerprint after ledger init succeeds.
  | FingerprintMatch
  | FingerprintMismatch !LedgerStateFingerprint !LedgerStateFingerprint
    -- ^ Fields: @(onDisk, expected)@.
  | FingerprintMissing !FilePath
    -- ^ Directory has content but no readable fingerprint file.
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Operations
-- ---------------------------------------------------------------------------

-- | Path of the fingerprint file inside the ledger state directory.
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

-- | Compare the on-disk fingerprint (if any) against the expected
-- value. Unreadable JSON falls into 'FingerprintMissing' so the
-- operator-facing message stays the same.
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

-- | Write the fingerprint to disk, creating the directory if needed.
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
  "format_version=" <> tshow (lsfFormatVersion fp)
    <> ", network_magic=" <> tshow (lsfNetworkMagic fp)
    <> ", system_start=" <> tshow (lsfSystemStart fp)
  where
    tshow :: Show a => a -> Text
    tshow = T.pack . show
