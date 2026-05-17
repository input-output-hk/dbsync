-- | Bracketed 'Hasql.Pool.Pool' opener for the parallel Prep steps,
-- plus the 'PoolM' monad the bracketed action runs in.
--
-- Inside the bracket the pool is read from env (via 'HasPool'), not
-- threaded through every 'usePool' call. Tracing also delegates
-- through the env so per-table log lines work the same as outside.
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
  ) where

import Cardano.Prelude

import Data.Time.Clock (DiffTime)
import qualified Hasql.Connection.Settings as ConnSettings
import qualified Hasql.Pool as Pool
import qualified Hasql.Pool.Config as PoolConfig
import qualified Hasql.Session as Sess

import DbSync.AppM (AppM, runAppM)
import DbSync.Phase.Preparing.Tuning
  ( PrepTuning
  , prepSessionGUCsSession
  )
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (AppTracer)

-- | Access a 'Hasql.Pool.Pool' from env.
class HasPool env where
  getPool :: env -> Pool.Pool

-- | Reader env inside a 'withPrepPool' bracket: just the pool plus
-- whatever the caller needs for logging.
data PoolEnv = PoolEnv
  { pePool   :: !Pool.Pool
  , peTracer :: !AppTracer
  }

instance HasPool PoolEnv where
  getPool = pePool

instance HasTracer PoolEnv where
  getTracer = peTracer

-- | The monad inside a 'withPrepPool' bracket.
type PoolM = AppM PoolEnv

-- | Acquire a pool, run @action@ in 'PoolM' with the pool bound on
-- env, release the pool on exit. Each backend boots with the
-- 'PrepTuning' GUCs applied via @initSession@.
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

-- | As 'withPrepPool' but takes the tracer explicitly. Used by call
-- sites that don't (yet) carry a 'HasTracer' env.
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
-- is sized for user-facing request paths where a hung pool is worse
-- than failing fast. Prep is batch DDL: with ~30 UNLOGGED tables
-- fanning out to a 4-backend pool, a small table queued behind a
-- multi-minute @tx_out@ flip easily waits longer than 10s. Pick a
-- value that won't realistically trip on a mainnet-shaped Prep but
-- still surfaces a genuine deadlock.
prepAcquisitionTimeout :: DiffTime
prepAcquisitionTimeout = 6 * 3600  -- 6 hours

-- | Run a 'Sess.Session' on the env's pool, panicking on driver
-- failure. Prep is one-shot DDL with no useful retry strategy;
-- surfacing the actual hasql error is the most useful behaviour.
usePool
  :: (HasPool env, MonadReader env m, MonadIO m)
  => Text
  -> Sess.Session a
  -> m a
usePool ctx session = do
  pool <- asks getPool
  result <- liftIO (Pool.use pool session)
  case result of
    Right a -> pure a
    Left  e -> panic $ "DbSync.Db.Pool." <> ctx <> ": " <> show e
