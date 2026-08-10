{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | LSM session that backs Ingest-phase working state — the
-- 'DbSync.Phase.Ingest.UtxoStore' and
-- 'DbSync.Phase.Ingest.DedupStore' tables. The Follow phase never
-- opens it.
--
-- @lsm-tree@ sessions allow concurrent reads but race on writes, so
-- only the single consumer thread writes these tables.
module DbSync.Phase.Ingest.LsmSession
  ( -- * Types
    LsmSession (..)
  , LsmSessionTracer

    -- * Lifecycle
  , openLsmSession
  , closeLsmSession
  , closeAndDeleteLsmSession

    -- * Tracing
  , nullLsmSessionTracer
  , lsmSessionTracerFromApp

    -- * Shared table configuration
  , defaultIngestTableConfig

    -- * Snapshot naming
  , ingestSnapshotLabel
  , currentSnapshotName

    -- * Filesystem helpers
  , ingestLsmDirName
  , ingestLsmRootDir
  ) where

import Cardano.Prelude

import Control.Tracer (Tracer (..), nullTracer, traceWith)
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Database.LSMTree as LSMTree
import qualified System.Directory as Dir
import qualified System.FS.API as FsApi
import qualified System.FS.BlockIO.API as BlockApi
import qualified System.FS.BlockIO.IO as BlockIO
import System.FS.IO (HandleIO)
import System.FilePath ((</>))
import System.Random (randomIO)

import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Tracer @lsm-tree@ writes its internal events to. Wire it to
-- 'nullLsmSessionTracer' or 'lsmSessionTracerFromApp'.
type LsmSessionTracer = Tracer IO LSMTree.LSMTreeTrace

-- | Handle owned by 'DbSync.App.Env.IngestEnv'.
data LsmSession = LsmSession
  { lsmHandle     :: !(LSMTree.Session IO)
    -- ^ Passed to the per-table constructors, e.g.
    -- @Phase.Ingest.UtxoStore.openUtxoStore@.
  , lsmHasBlockIO :: !(BlockApi.HasBlockIO IO HandleIO)
    -- ^ 'lsmClose' closes this after the session — 'BlockApi.close'
    -- is not idempotent on its own.
  , lsmRootDir    :: !FilePath
    -- ^ Absolute path to @\<state-dir\>/dbsync-ledger/ingest-lsm/@.
  , lsmClose      :: !(IO ())
    -- ^ Idempotent shutdown action. An 'IORef Bool' guards
    -- @closeSession@ and @BlockApi.close@, so a second call does
    -- nothing.
  }

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Open or restore the session. An empty directory means a fresh
-- boot, and @lsm-tree@ creates a new session; otherwise it restores
-- the existing one. The caller must release the result through
-- 'closeLsmSession' or 'closeAndDeleteLsmSession'.
openLsmSession
  :: LsmSessionTracer
  -> FilePath
  -- ^ Parent directory, e.g. @\<ledgerStateDir\>/dbsync-ledger/@.
  -> IO LsmSession
openLsmSession tracer parentDir = do
  let rootDir = ingestLsmRootDir parentDir
  Dir.createDirectoryIfMissing True rootDir

  (hasFS, hasBlockIO) <-
    BlockIO.ioHasBlockIO (FsApi.MountPoint rootDir) BlockIO.defaultIOCtxParams

  -- Only consulted when the directory is empty; ignored on restore.
  salt <- randomIO

  session <-
    LSMTree.openSession tracer hasFS hasBlockIO salt (FsApi.mkFsPath [])
      `onException` BlockApi.close hasBlockIO

  closedRef <- newIORef False
  let closer = mask_ $ do
        alreadyClosed <- atomicModifyIORef' closedRef (\c -> (True, c))
        unless alreadyClosed $ do
          LSMTree.closeSession session
          BlockApi.close hasBlockIO

  pure LsmSession
    { lsmHandle     = session
    , lsmHasBlockIO = hasBlockIO
    , lsmRootDir    = rootDir
    , lsmClose      = closer
    }

-- | Idempotent close. Keeps the on-disk session, so a later boot can
-- resume from it.
closeLsmSession :: LsmSession -> IO ()
closeLsmSession = lsmClose

-- | Close the session and remove the @ingest-lsm/@ directory.
-- Precondition: Prep finished cleanly. A call during Ingest or Prep
-- destroys the restart anchor.
closeAndDeleteLsmSession :: LsmSession -> IO ()
closeAndDeleteLsmSession s = do
  lsmClose s
  exists <- Dir.doesDirectoryExist (lsmRootDir s)
  when exists $ Dir.removeDirectoryRecursive (lsmRootDir s)

-- ---------------------------------------------------------------------------
-- Tracing
-- ---------------------------------------------------------------------------

nullLsmSessionTracer :: LsmSessionTracer
nullLsmSessionTracer = nullTracer

-- | Forward each @lsm-tree@ event to the application tracer as a
-- Debug 'LogMsg' under the @"LsmIngest"@ component.
-- 'isHotPathLsmTrace' drops the per-operation table events.
lsmSessionTracerFromApp :: AppTracer -> LsmSessionTracer
lsmSessionTracerFromApp inner = Tracer $ \e ->
  unless (isHotPathLsmTrace e) $
    traceWith inner (LogMsg Debug "LsmIngest" (show e))

-- | True for events that fire on every batched table operation.
-- These flood the log during ingest and add no diagnostic value.
isHotPathLsmTrace :: LSMTree.LSMTreeTrace -> Bool
isHotPathLsmTrace (LSMTree.TraceTable _ tt) = case tt of
  LSMTree.TraceLookups{}     -> True
  LSMTree.TraceRangeLookup{} -> True
  LSMTree.TraceUpdates{}     -> True
  LSMTree.TraceUpdated{}     -> True
  _                          -> False
isHotPathLsmTrace _ = False

-- ---------------------------------------------------------------------------
-- Shared table configuration
-- ---------------------------------------------------------------------------

-- | Config shared by every ingest-phase table. Each override away
-- from 'LSMTree.defaultTableConfig':
--
--   * 200_000 write-buffer entries keep the transient level-0 run
--     count low under the insert rate of
--     'DbSync.Phase.Ingest.UtxoStore.recordTx'.
--   * 'LSMTree.CompactIndex' suits these keys: each one is a blake2b
--     hash plus a 2-byte output index, so the high 64 bits stay
--     uniformly distributed.
--   * 'LSMTree.Incremental' spreads merge work across operations
--     instead of doing it all at one level overflow.
--   * 'LSMTree.DiskCacheAll' admits every on-disk level to the OS
--     page cache, so deep-level lookups avoid raw disk reads on
--     macOS's serial blockio.
defaultIngestTableConfig :: LSMTree.TableConfig
defaultIngestTableConfig = LSMTree.defaultTableConfig
  { LSMTree.confWriteBufferAlloc  = LSMTree.AllocNumEntries 200_000
  , LSMTree.confBloomFilterAlloc  = LSMTree.AllocRequestFPR 1e-3
  , LSMTree.confFencePointerIndex = LSMTree.CompactIndex
  , LSMTree.confDiskCachePolicy   = LSMTree.DiskCacheAll
  , LSMTree.confMergeSchedule     = LSMTree.Incremental
  }

-- ---------------------------------------------------------------------------
-- Snapshot naming
-- ---------------------------------------------------------------------------

-- | Snapshot label every ingest-phase table saves under.
-- @lsm-tree@ rejects opens whose label differs from the save label.
ingestSnapshotLabel :: LSMTree.SnapshotLabel
ingestSnapshotLabel = LSMTree.SnapshotLabel "dbsync-ingest"

-- | Snapshot name used by every save / load in the session.
currentSnapshotName :: LSMTree.SnapshotName
currentSnapshotName = LSMTree.toSnapshotName "current"

-- ---------------------------------------------------------------------------
-- Filesystem helpers
-- ---------------------------------------------------------------------------

-- | Subdirectory name under @dbsync-ledger/@.
ingestLsmDirName :: FilePath
ingestLsmDirName = "ingest-lsm"

-- | @\<parentDir\>/ingest-lsm@.
ingestLsmRootDir :: FilePath -> FilePath
ingestLsmRootDir parentDir = parentDir </> ingestLsmDirName
