{-# LANGUAGE OverloadedStrings #-}

-- | Test helpers for the ingest-phase LSM session and the tables on
-- top of it.
--
-- The helpers run each action against a fresh session under
-- @\<system tmp\>/dbsync-test-lsm-\<unique\>/@ and remove the
-- directory afterwards. Tests that need a real 'UtxoStore' use
-- 'withTestUtxoStore'; tests that need the bare session for their
-- own tables use 'withTestLsmSession'.
module DbSync.Test.Lsm
  ( withTestLsmSession
  , withTestUtxoStore
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified System.Directory as Dir
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)

import DbSync.Phase.Ingest.LsmSession
  ( LsmSession
  , closeLsmSession
  , nullLsmSessionTracer
  , openLsmSession
  )
import DbSync.Phase.Ingest.UtxoStore
  ( UtxoStore
  , closeUtxoStore
  , openUtxoStore
  )

-- | Bracket an action with a fresh LSM session in a temporary
-- directory. The directory is removed on exit whether the action
-- succeeds or throws.
withTestLsmSession :: (LsmSession -> IO a) -> IO a
withTestLsmSession action =
  withTempDir $ \dir ->
    bracket (openLsmSession nullLsmSessionTracer dir) closeLsmSession action

-- | Bracket an action with a fresh 'UtxoStore' on top of a temp-dir
-- LSM session. Both are closed on exit.
withTestUtxoStore :: (UtxoStore -> IO a) -> IO a
withTestUtxoStore action =
  withTestLsmSession $ \lsm ->
    bracket (openUtxoStore lsm) closeUtxoStore action

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

-- | Allocate a unique subdirectory under the system temp dir and
-- remove it (recursively) when the action exits.
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
  sysTmp <- Dir.getTemporaryDirectory
  tag    <- nextTempDirTag
  let dir = sysTmp </> ("dbsync-test-lsm-" <> show tag)
  bracket_ (Dir.createDirectoryIfMissing True dir) (removeDirIfExists dir)
           (action dir)
  where
    removeDirIfExists dir =
      Dir.removeDirectoryRecursive dir
        `catch` \e ->
          if isDoesNotExistError e then pure () else throwIO e

-- | Per-process incrementing counter for unique temp-dir suffixes.
-- Avoids the @temporary@ package dependency.
nextTempDirTag :: IO Int
nextTempDirTag = atomicModifyIORef' tempDirCounter (\n -> (n + 1, n + 1))

{-# NOINLINE tempDirCounter #-}
tempDirCounter :: IORef Int
tempDirCounter = unsafePerformIO (newIORef 0)
