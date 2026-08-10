-- | Bracketed 'Hasql.Pool.Pool' opener for the parallel Prep steps,
-- plus the 'PoolM' monad the bracketed action runs in. Inside the
-- bracket both the pool and the tracer come from env.
module DbSync.Db.Pool
  ( -- * Pool env + monad
    PoolEnv (..)
  , PoolM
  , HasPool (..)

    -- * Bracket
  , withPrepPool
  , withPrepPoolIO

    -- * Session runner
  , usePool

    -- * Bounded fan-out
  , forPooled_
  ) where

import Cardano.Prelude

import Control.Concurrent.Async (replicateConcurrently_)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Time.Clock (DiffTime)
import qualified Hasql.Connection.Settings as ConnSettings
import qualified Hasql.Pool as Pool
import qualified Hasql.Pool.Config as PoolConfig
import qualified Hasql.Session as Sess

import DbSync.AppM (AppM, runAppM)
import DbSync.Db.Run (usePoolSession)
import DbSync.Phase.Preparing.Tuning
  ( PrepTuning
  , prepSessionGUCsSession
  )
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (AppTracer)

class HasPool env where
  getPool :: env -> Pool.Pool

-- | Reader env inside a 'withPrepPool' bracket.
data PoolEnv = PoolEnv
  { pePool   :: !Pool.Pool
  , peTracer :: !AppTracer
  }

instance HasPool PoolEnv where
  getPool = pePool

instance HasTracer PoolEnv where
  getTracer = peTracer

type PoolM = AppM PoolEnv

-- | Acquire a pool, run @action@ in 'PoolM', release the pool on
-- exit. Each backend boots with the 'PrepTuning' GUCs applied through
-- @initSession@.
withPrepPool
  :: (HasTracer env, MonadReader env m, MonadIO m)
  => ConnSettings.Settings
  -> PrepTuning
  -> Int
  -- ^ Pool size. Different Prep steps have different resource
  -- profiles (the flip is bandwidth-bound; the index build is
  -- RAM-bound), so the caller picks.
  -> PoolM a
  -> m a
withPrepPool connSettings tuning poolSize action = do
  tracer <- asks getTracer
  liftIO (withPrepPoolIO tracer connSettings tuning poolSize action)

-- | As 'withPrepPool', but for call sites that carry no 'HasTracer'
-- env.
withPrepPoolIO
  :: AppTracer
  -> ConnSettings.Settings
  -> PrepTuning
  -> Int
  -> PoolM a
  -> IO a
withPrepPoolIO tracer connSettings tuning poolSize action =
  bracket (Pool.acquire poolConfig) Pool.release $ \pool ->
    runAppM (PoolEnv pool tracer) action
  where
    poolConfig = PoolConfig.settings
      [ PoolConfig.staticConnectionSettings connSettings
      , PoolConfig.size poolSize
      , PoolConfig.initSession (prepSessionGUCsSession tuning)
      , PoolConfig.acquisitionTimeout prepAcquisitionTimeout
      ]

-- | Pool acquisition timeout for Prep. The hasql-pool default of 10s
-- suits user-facing request paths, where a hung pool is worse than
-- failing fast. Prep is batch DDL instead: every enabled table fans
-- out to a 4-backend pool, so a small table queued behind the
-- @tx_out@ flip waits far longer than 10s. This value must not trip
-- on a healthy Prep, but must still surface a genuine deadlock.
prepAcquisitionTimeout :: DiffTime
prepAcquisitionTimeout = 6 * 3600  -- 6 hours

-- | Run a 'Sess.Session' on the env's pool. Driver failures surface
-- as 'AppDatabaseError'; Prep is one-shot DDL with no useful retry
-- strategy.
usePool
  :: (HasPool env, MonadReader env m, MonadIO m)
  => Text
  -> Sess.Session a
  -> m a
usePool ctx session = do
  pool <- asks getPool
  usePoolSession ("DbSync.Db.Pool." <> ctx) pool session

-- | Run one action per item on exactly @n@ worker threads that pop
-- items in list order.
--
-- A plain @forConcurrently_@ over the whole list loses the caller's
-- priority order and parks one blocked thread per item on the pool's
-- acquisition queue. Size @n@ to the pool, so every worker holds a
-- backend while work remains.
forPooled_ :: MonadUnliftIO m => Int -> [a] -> (a -> m ()) -> m ()
forPooled_ n items run = do
  queue <- liftIO (newIORef items)
  withRunInIO $ \runInIO ->
    replicateConcurrently_ (max 1 n) (runInIO (worker queue))
  where
    worker queue = loop
      where
        loop = do
          next <- liftIO $ atomicModifyIORef' queue $ \case
            []       -> ([], Nothing)
            (x : xs) -> (xs, Just x)
          for_ next $ \x -> run x >> loop
