{-# LANGUAGE ScopedTypeVariables #-}

-- | Generic background loop for off-chain metadata fetching.
--
-- Each cycle the loop loads pending refs from PG, fetches them via
-- a pluggable hook, and persists the per-ref result. Pool and vote
-- workers each supply their own load / fetch / persist triple.
module DbSync.Worker.OffChain.Fetcher
  ( -- * Worker handle
    OffChainWorker (..)

    -- * Per-domain hooks
  , OffChainHooks (..)

    -- * Lifecycle
  , mkOffChainWorker
  , closeOffChainWorker

    -- * Single-cycle entry point (exported for tests)
  , runOneCycle
  ) where

import Cardano.Prelude

import Control.Tracer (traceWith)
import qualified Hasql.Connection as Conn
import qualified Hasql.Connection.Settings as Settings

import DbSync.Error (throwDb)
import DbSync.Error.Render (logThreadExit)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Worker handle. The 'Async' is linked to the parent so a worker
-- exception propagates at the cancel boundary.
data OffChainWorker ref = OffChainWorker
  { owAsync :: !(Async ())
  , owConn  :: !Conn.Connection
  }

-- | Per-domain plug-in points. The loop is otherwise identical
-- between pool and vote workers.
data OffChainHooks ref result = OffChainHooks
  { ohLoadPending :: !(Conn.Connection -> Int32 -> IO [ref])
    -- ^ Load up to N refs that are due to be (re)fetched this cycle.
  , ohFetch       :: !(ref -> IO (Either Text result))
    -- ^ Attempt one fetch; @Left err@ records as an error row,
    -- @Right ok@ records as a success row.
  , ohPersist     :: !(Conn.Connection -> ref -> Either Text result -> IO ())
    -- ^ Write the per-ref outcome to PG.
  }

-- ---------------------------------------------------------------------------
-- * Lifecycle
-- ---------------------------------------------------------------------------

-- | Spawn the worker on a dedicated PG connection.
mkOffChainWorker
  :: forall ref result.
     AppTracer
  -> Text                       -- ^ component label for logging
  -> Settings.Settings
  -> Int                        -- ^ inter-cycle sleep (microseconds)
  -> Int32                      -- ^ max refs to load per cycle
  -> OffChainHooks ref result
  -> IO (OffChainWorker ref)
mkOffChainWorker tracer component settings sleepMicros batchSize hooks = do
  conn   <- openConn component settings
  worker <- async $
    loopForever tracer component conn sleepMicros batchSize hooks
  link worker
  pure OffChainWorker
    { owAsync = worker
    , owConn  = conn
    }

closeOffChainWorker :: OffChainWorker ref -> IO ()
closeOffChainWorker ow = do
  cancel (owAsync ow)
  Conn.release (owConn ow)

-- ---------------------------------------------------------------------------
-- * Loop
-- ---------------------------------------------------------------------------

loopForever
  :: forall ref result.
     AppTracer
  -> Text
  -> Conn.Connection
  -> Int
  -> Int32
  -> OffChainHooks ref result
  -> IO ()
loopForever tracer component conn sleepMicros batchSize hooks =
  body `catch` \(e :: SomeException) -> do
    logThreadExit component e tracer
    throwIO e
  where
    body = forever $ do
      threadDelay sleepMicros
      runOneCycle tracer component conn batchSize hooks

-- | Run a single cycle: load pending refs, fetch each, persist the
-- outcome. Exposed so integration tests can drive the worker
-- deterministically without going through the sleeping loop.
runOneCycle
  :: AppTracer
  -> Text
  -> Conn.Connection
  -> Int32
  -> OffChainHooks ref result
  -> IO ()
runOneCycle tracer component conn batchSize hooks = do
  refs <- ohLoadPending hooks conn batchSize
  unless (null refs) $
    traceWith tracer $
      LogMsg Info component
        ("processing " <> show (length refs) <> " ref(s)")
  for_ refs $ \r -> do
    outcome <- ohFetch hooks r
    ohPersist hooks conn r outcome

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

openConn :: Text -> Settings.Settings -> IO Conn.Connection
openConn component settings = do
  r <- Conn.acquire settings
  case r of
    Right c -> pure c
    Left e  ->
      throwDb $ component <> ": failed to acquire PG connection: " <> show e
