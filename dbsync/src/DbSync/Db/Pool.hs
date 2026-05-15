{-# LANGUAGE OverloadedStrings #-}

-- | Bracketed 'Hasql.Pool.Pool' opener for the parallel-capable Prep
-- steps.
--
-- Steps in 'DbSync.Phase.PreparingForChainTip.run' that touch
-- disjoint tables (the @ALTER … SET LOGGED@ flip, the
-- non-concurrent index build) parallelise cleanly across separate
-- backends. The single-statement steps stay on the dedicated
-- control connection; only the parallel ones go through a pool.
--
-- Each pool backend boots with the 'PrepTuning' GUCs applied via
-- 'Hasql.Pool.Config.initSession', so workers acquired from the
-- pool run with the same @maintenance_work_mem@,
-- @max_parallel_maintenance_workers@, and @synchronous_commit@ as
-- the main Prep connection.
module DbSync.Db.Pool
  ( withPrepPool
  , usePool
  ) where

import Cardano.Prelude

import Data.Time.Clock (DiffTime)
import qualified Hasql.Connection.Settings as ConnSettings
import qualified Hasql.Pool as Pool
import qualified Hasql.Pool.Config as PoolConfig
import qualified Hasql.Session as Sess

import DbSync.Phase.PreparingForChainTip.Tuning
  ( PrepTuning
  , prepSessionGUCsSession
  )

-- | Open a Hasql pool, hand it to the inner action, release on exit.
-- The pool is bounded to 'ptPoolSize' backends; each backend gets
-- the 'PrepTuning' GUCs applied exactly once when it is first
-- acquired (via @initSession@).
withPrepPool
  :: ConnSettings.Settings
  -> PrepTuning
  -> Int
  -- ^ Pool size. The caller decides because different Prep steps
  -- have different resource profiles (the flip is bandwidth-bound;
  -- the index build is RAM-bound).
  -> (Pool.Pool -> IO a)
  -> IO a
withPrepPool connSettings tuning poolSize =
  bracket (Pool.acquire poolConfig) Pool.release
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

-- | Run a 'Sess.Session' on a pool backend, panicking on driver
-- failure. Prep is one-shot DDL; there is no retry strategy that
-- makes sense, and surfacing the actual hasql error message is the
-- most useful behaviour.
usePool :: Pool.Pool -> Text -> Sess.Session a -> IO a
usePool pool ctx session = do
  result <- Pool.use pool session
  case result of
    Right a -> pure a
    Left  e -> panic $ "DbSync.Db.Pool." <> ctx <> ": " <> show e
