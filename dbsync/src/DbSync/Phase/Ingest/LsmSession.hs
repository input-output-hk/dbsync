{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | LSM session that backs Ingest-phase working state.
--
-- Hosts every LSM table used during 'IngestChainHistory':
-- 'DbSync.Phase.Ingest.UtxoStore' and the five
-- 'DbSync.Phase.Ingest.DedupStore' tables.
--
-- == Lifecycle
--
-- * 'openLsmSession' is called once when 'DbSync.Env.IngestEnv' is
--   built. If the directory exists it is restored, otherwise a
--   fresh session is created.
-- * 'lsmClose' is the idempotent shutdown action stored in the
--   record (same shape as 'DbSync.Ledger.Types.leClose'). Called
--   either via 'closeLsmSession' (close only — preserves the
--   on-disk session for the next boot) or
--   'closeAndDeleteLsmSession' (close + remove
--   @ingest-lsm/@).
-- * The Follow phase does not open this session — it uses the
--   PG-sequence + per-block resolver in
--   'DbSync.Phase.Following.Resolver'. The post-Prep delete in
--   'DbSync.Phase.Preparing.Run' is what makes that safe.
--
-- == Threading
--
-- @lsm-tree@ sessions are safe for concurrent reads but races on
-- write + anything else. Every ingest-phase table on top of this
-- session is written by the single consumer thread; match that or
-- add explicit serialisation.
--
-- == On disk
--
-- @
-- \<state-dir\>/dbsync-ledger/ingest-lsm/    -- 'lsmRootDir'
--   active/                                 -- managed by lsm-tree
--   snapshots/\<name\>/                     -- one per saved snapshot
--   lock                                    -- session-dir lock file
-- @
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

import Control.Tracer (Tracer, contramap, nullTracer)
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

-- | Tracer @lsm-tree@ writes its internal events to. The session
-- can be wired to 'nullLsmSessionTracer' (suppress everything) or
-- to 'lsmSessionTracerFromApp' (forward each event as a Debug
-- 'LogMsg').
type LsmSessionTracer = Tracer IO LSMTree.LSMTreeTrace

-- | Handle owned by 'DbSync.Env.IngestEnv'. Carries every resource
-- that needs releasing on shutdown plus the directory the session
-- writes to (needed by 'closeAndDeleteLsmSession').
data LsmSession = LsmSession
  { lsmHandle     :: !(LSMTree.Session IO)
    -- ^ The opened session. Passed to per-table constructors —
    -- e.g. @Phase.Ingest.UtxoStore.openUtxoStore@.
  , lsmHasBlockIO :: !(BlockApi.HasBlockIO IO HandleIO)
    -- ^ Underlying block-IO context. Closed after the session by
    -- 'lsmClose' — 'BlockApi.close' is not idempotent on its own.
  , lsmRootDir    :: !FilePath
    -- ^ Absolute path to @\<state-dir\>/dbsync-ledger/ingest-lsm/@.
  , lsmClose      :: !(IO ())
    -- ^ Idempotent shutdown action. Mirrors
    -- 'DbSync.Ledger.Types.leClose' so the App-level cleanup treats
    -- both LSM sessions uniformly. Internally guards @closeSession@
    -- and @BlockApi.close@ behind an 'IORef Bool' so a second call
    -- is a no-op.
  }

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Open or restore the session.
--
-- The directory is created if missing. On a fresh boot the
-- directory is empty and @lsm-tree@ creates a new session using the
-- supplied salt; on a resumed boot it contains an existing session,
-- which is restored and the salt is ignored.
--
-- The session returned must be released via 'closeLsmSession' or
-- 'closeAndDeleteLsmSession'.
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
  let closer = do
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

-- | Idempotent close. Preserves the on-disk session so a later boot
-- can resume from it.
closeLsmSession :: LsmSession -> IO ()
closeLsmSession = lsmClose

-- | Close the session and remove the @ingest-lsm/@ directory.
--
-- Precondition: Prep has completed cleanly. Calling this
-- mid-Ingest or mid-Prep destroys the restart anchor.
closeAndDeleteLsmSession :: LsmSession -> IO ()
closeAndDeleteLsmSession s = do
  lsmClose s
  exists <- Dir.doesDirectoryExist (lsmRootDir s)
  when exists $ Dir.removeDirectoryRecursive (lsmRootDir s)

-- ---------------------------------------------------------------------------
-- Tracing
-- ---------------------------------------------------------------------------

-- | Drop every @lsm-tree@ event on the floor.
nullLsmSessionTracer :: LsmSessionTracer
nullLsmSessionTracer = nullTracer

-- | Forward each @lsm-tree@ event into the application tracer as a
-- Debug-level 'LogMsg' under the @"LsmIngest"@ component.
lsmSessionTracerFromApp :: AppTracer -> LsmSessionTracer
lsmSessionTracerFromApp = contramap toLogMsg
  where
    toLogMsg e = LogMsg Debug "LsmIngest" (show e) Nothing

-- ---------------------------------------------------------------------------
-- Shared table configuration
-- ---------------------------------------------------------------------------

-- | 'LSMTree.TableConfig' used by every ingest-phase table.
--
-- Overrides from 'LSMTree.defaultTableConfig':
--
--   * 'LSMTree.AllocNumEntries' 200_000 — large write buffer to
--     keep transient level-0 run counts low under the sustained
--     insert rate of 'DbSync.Phase.Ingest.UtxoStore.recordTx'.
--   * 'LSMTree.AllocRequestFPR' 1e-3 — bloom-filter false-positive
--     target.
--   * 'LSMTree.CompactIndex' — every key in this session is a
--     blake2b hash concatenated with a 2-byte output index; the
--     high 64 bits remain uniformly distributed.
--   * 'LSMTree.DiskCacheAll' — opt in to the OS page cache for the
--     entire table.
--   * 'LSMTree.Incremental' — spread merge work across operations
--     instead of doing it all at one level overflow.
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
