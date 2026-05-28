{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'DbSync.Ledger.Fingerprint'.
module DbSync.Ledger.FingerprintSpec
  ( spec
  ) where

import Cardano.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import qualified System.Directory as Dir
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Ledger.Fingerprint
  ( FingerprintCheck (..)
  , LedgerStateFingerprint (..)
  , checkFingerprint
  , currentFormatVersion
  , fingerprintPath
  , writeFingerprint
  )

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

mainnetFingerprint :: LedgerStateFingerprint
mainnetFingerprint = LedgerStateFingerprint
  { lsfFormatVersion = currentFormatVersion
  , lsfNetworkMagic  = 764824073
  , lsfSystemStart   = UTCTime (fromGregorian 2017 9 23) (21 * 3600 + 44 * 60 + 51)
  }

testnetFingerprint :: LedgerStateFingerprint
testnetFingerprint = LedgerStateFingerprint
  { lsfFormatVersion = currentFormatVersion
  , lsfNetworkMagic  = 1
  , lsfSystemStart   = UTCTime (fromGregorian 2022 10 25) 0
  }

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "DbSync.Ledger.Fingerprint" $ do

  describe "JSON round-trip" $ do
    it "decode . encode = id" $ do
      let bs = Aeson.encode mainnetFingerprint
      Aeson.eitherDecode bs `shouldBe` Right mainnetFingerprint

    it "stable JSON keys" $ do
      let bs = BSL.toStrict (Aeson.encode mainnetFingerprint)
      BS.isInfixOf "format_version" bs `shouldBe` True
      BS.isInfixOf "network_magic"  bs `shouldBe` True
      BS.isInfixOf "system_start"   bs `shouldBe` True

  describe "checkFingerprint" $ do
    it "absent directory → FingerprintFresh" $ withTmpDir $ \root -> do
      result <- checkFingerprint (root </> "missing") mainnetFingerprint
      result `shouldBe` FingerprintFresh

    it "empty directory → FingerprintFresh" $ withTmpDir $ \dir -> do
      result <- checkFingerprint dir mainnetFingerprint
      result `shouldBe` FingerprintFresh

    it "matching file → FingerprintMatch" $ withTmpDir $ \dir -> do
      writeFingerprint dir mainnetFingerprint
      result <- checkFingerprint dir mainnetFingerprint
      result `shouldBe` FingerprintMatch

    it "network_magic differs → FingerprintMismatch" $ withTmpDir $ \dir -> do
      writeFingerprint dir mainnetFingerprint
      result <- checkFingerprint dir testnetFingerprint
      result `shouldBe` FingerprintMismatch mainnetFingerprint testnetFingerprint

    it "system_start differs → FingerprintMismatch" $ withTmpDir $ \dir -> do
      let earlier = mainnetFingerprint
            { lsfSystemStart = UTCTime (fromGregorian 2015 1 1) 0 }
      writeFingerprint dir earlier
      result <- checkFingerprint dir mainnetFingerprint
      result `shouldBe` FingerprintMismatch earlier mainnetFingerprint

    it "unknown format_version → FingerprintMismatch" $ withTmpDir $ \dir -> do
      let bumped = mainnetFingerprint { lsfFormatVersion = 99 }
      writeFingerprint dir bumped
      result <- checkFingerprint dir mainnetFingerprint
      result `shouldBe` FingerprintMismatch bumped mainnetFingerprint

    it "non-empty directory with no fingerprint → FingerprintMissing" $ withTmpDir $ \dir -> do
      Dir.createDirectoryIfMissing True (dir </> "snapshot-headers" </> "12345")
      result <- checkFingerprint dir mainnetFingerprint
      result `shouldBe` FingerprintMissing dir

    it "unreadable JSON → FingerprintMissing" $ withTmpDir $ \dir -> do
      Dir.createDirectoryIfMissing True dir
      BSL.writeFile (fingerprintPath dir) "{ this is not json"
      result <- checkFingerprint dir mainnetFingerprint
      result `shouldBe` FingerprintMissing dir

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Bracket an action with a fresh temp directory removed on exit.
withTmpDir :: (FilePath -> IO a) -> IO a
withTmpDir action = do
  sysTmp <- Dir.getTemporaryDirectory
  tag    <- nextTempDirTag
  let dir = sysTmp </> ("dbsync-test-fingerprint-" <> show tag)
  bracket_ (Dir.createDirectoryIfMissing True dir) (removeIfExists dir) (action dir)
  where
    removeIfExists dir =
      Dir.removeDirectoryRecursive dir `catch` \e ->
        if isDoesNotExistError e then pure () else throwIO e

nextTempDirTag :: IO Int
nextTempDirTag = atomicModifyIORef' tempDirCounter (\n -> (n + 1, n + 1))

{-# NOINLINE tempDirCounter #-}
tempDirCounter :: IORef Int
tempDirCounter = unsafePerformIO (newIORef 0)
