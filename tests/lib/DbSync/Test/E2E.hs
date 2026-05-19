{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cross-spec helpers for end-to-end tests that drive 'runApp'
-- against the mock chainsync server.
--
-- Composes the lower-level harness building blocks ('AppArgs'
-- construction, shutdown signal, mock-node lifecycle, PG probes)
-- into the bracket pattern every e2e spec uses: start 'runApp', do
-- some work, fire shutdown, await exit.
module DbSync.Test.E2E
  ( -- * Common constants
    conwayConfigDir

    -- * App-session brackets
  , withAppSession
  , withAppSessionResume

    -- * Wait helpers
  , awaitShutdown
  , syncCompleteTrue
  , forgeAndWaitForBlocks
  , waitForLogMatch

    -- * Filesystem probes
  , listLedgerSnapshots
  ) where

import Cardano.Prelude

import Data.IORef (IORef, readIORef)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.Timeout (timeout)

import DbSync.App.Args (AppArgs)
import DbSync.App.Run (runApp)
import DbSync.Config.Types (SyncConfig)
import DbSync.Db.Schema.Core (blockTableDef)
import DbSync.Db.Schema.SyncState (syncStateTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Test.AppHarness
  ( mkAppArgsFromMockNode
  , mkAppArgsFromMockNodeResume
  , newShutdown
  )
import DbSync.Test.Database (execTestDb, queryTestDb)
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockNode (MockNode, forgeAndPushBlocks)
import DbSync.Test.PgAssertions (countRows, tableColumn)
import DbSync.Trace.Types (AppTracer, LogMsg)

-- | Conway test config bundle. Short epoch (500 slots) and small
-- security parameter (k=10) so e2e specs cross both quickly.
conwayConfigDir :: FilePath
conwayConfigDir = "data/config-conway"

-- ---------------------------------------------------------------------------
-- * App-session brackets
-- ---------------------------------------------------------------------------

-- | Run 'runApp' in a background async wired to a shutdown signal,
-- run @body@ against the still-alive sync, then fire the signal and
-- wait for 'runApp' to return.
--
-- @body@ receives the linked 'Async' so it can poll status or
-- cancel manually; most callers ignore the parameter.
--
-- @aaResyncFromGenesis = True@. Use 'withAppSessionResume' for the
-- second leg of a restart test.
--
-- Defensively clears any leftover @sync_complete=true@ from a prior
-- spec before starting the async. Without this, @body@\'s
-- 'waitForSyncComplete' can return immediately against the prior
-- state before our 'runApp' has reached its 'dropSchema' call,
-- racing with the schema being torn down underneath the body.
withAppSession
  :: AppTracer
  -> SyncConfig
  -> MockNode
  -> FilePath
  -> (Async () -> IO a)
  -> IO a
withAppSession tracer profile mn ledgerDir body = do
  clearSyncCompleteFlag
  runApp' mkAppArgsFromMockNode tracer profile mn ledgerDir body

-- | Same as 'withAppSession' but with @aaResyncFromGenesis = False@.
-- Does not clear any state — resume mode relies on the existing
-- @dbsync_sync_state@ row left by the previous session.
withAppSessionResume
  :: AppTracer
  -> SyncConfig
  -> MockNode
  -> FilePath
  -> (Async () -> IO a)
  -> IO a
withAppSessionResume = runApp' mkAppArgsFromMockNodeResume

-- | Set @sync_complete = false@ on the singleton sync-state row, if
-- it exists. Silent no-op when the table or row is absent (fresh DB,
-- or prior spec dropped the schema).
clearSyncCompleteFlag :: IO ()
clearSyncCompleteFlag =
  execTestDb
    ( "UPDATE " <> tdName syncStateTableDef
        <> " SET " <> tableColumn syncStateTableDef "sync_complete" <> " = false"
        <> " WHERE " <> tableColumn syncStateTableDef "id" <> " = 1"
    )
    `catch` \(_ :: SomeException) -> pure ()

-- | Shared bracket body used by 'withAppSession' /
-- 'withAppSessionResume'. The only piece that varies is the
-- 'AppArgs' builder.
runApp'
  :: (SyncConfig -> MockNode -> FilePath -> Maybe (IO ()) -> AppArgs)
  -> AppTracer
  -> SyncConfig
  -> MockNode
  -> FilePath
  -> (Async () -> IO a)
  -> IO a
runApp' mkArgs tracer profile mn ledgerDir body = do
  (fire, waitSig) <- newShutdown
  let args = mkArgs profile mn ledgerDir (Just waitSig)
  withAsync (runApp tracer args) $ \app -> do
    link app
    a <- body app
    fire
    awaitShutdown "runApp" app
    pure a

-- ---------------------------------------------------------------------------
-- * Wait helpers
-- ---------------------------------------------------------------------------

-- | Wait at most 30 s for an 'Async' to terminate. Panics on
-- timeout. Use after firing the shutdown signal that the async is
-- racing against.
awaitShutdown :: Text -> Async () -> IO ()
awaitShutdown name app = do
  mResult <- timeout 30_000_000 (wait app)
  case mResult of
    Just () -> pure ()
    Nothing -> panic $ name <> " did not return within 30s of shutdown signal"

-- | 'True' when @dbsync_sync_state.sync_complete@ reads @t@. Swallows
-- any DB error and returns 'False', so the predicate composes with
-- 'waitFor' against a database that's mid-init.
syncCompleteTrue :: IO Bool
syncCompleteTrue = do
  t <-
    ( T.strip <$> queryTestDb
        ( "SELECT " <> tableColumn syncStateTableDef "sync_complete"
            <> " FROM " <> tdName syncStateTableDef <> " LIMIT 1"
        )
    )
      `catch` \(_ :: SomeException) -> pure ""
  pure (t == "t")

-- | Forge @n@ blocks on the mock node and block until the @block@
-- table reports at least @minTotal@ rows. Lets specs express
-- "advance the chain and wait for Follow to catch up" as one call.
forgeAndWaitForBlocks
  :: MockNode
  -> Int          -- ^ how many blocks to forge
  -> Int          -- ^ minimum total block-table rows to wait for
  -> Int          -- ^ timeout in seconds
  -> IO ()
forgeAndWaitForBlocks mn n minTotal timeoutSec = do
  _ <- forgeAndPushBlocks mn n
  waitFor
    (tdName blockTableDef <> " table to reach " <> show minTotal <> " rows")
    (do tot <- countRows (tdName blockTableDef); pure (tot >= minTotal))
    timeoutSec

-- | Poll the captured-log 'IORef' until any 'LogMsg' satisfies
-- @predicate@, or panic after @timeoutSec@ seconds. Generalises the
-- "wait for the app to emit a particular line" pattern used by
-- several e2e specs.
--
-- The @label@ is the "what" being waited on and is interpolated
-- into the timeout message — pick something readable like
-- @"phase flip to FollowingChainTip"@.
waitForLogMatch
  :: IORef [LogMsg]
  -> Text
  -> (LogMsg -> Bool)
  -> Int
  -> IO ()
waitForLogMatch ref label predicate =
  waitFor ("log line for " <> label) matches
  where
    matches = do
      msgs <- readIORef ref
      pure (any predicate msgs)

-- ---------------------------------------------------------------------------
-- * Filesystem probes
-- ---------------------------------------------------------------------------

-- | Entries in the ledger snapshot-headers directory. Empty list
-- when the directory doesn't exist (e.g. ledger disabled).
listLedgerSnapshots :: FilePath -> IO [FilePath]
listLedgerSnapshots ledgerDir = do
  let snapDir = ledgerDir </> "dbsync-ledger" </> "snapshot-headers"
  exists <- doesDirectoryExist snapDir
  if exists then listDirectory snapDir else pure []
