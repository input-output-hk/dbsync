{-# LANGUAGE OverloadedStrings #-}

-- | Pool-specific glue for the off-chain fetch worker.
--
-- The worker discovers refs by polling PG via the work-queue
-- statements in 'DbSync.Db.Statement.OffChainPool'. Each ref is
-- then fetched via a pluggable 'OffChainFetcher'. For this slice
-- the production fetcher is a stub that always returns an HTTP
-- error so the @off_chain_pool_fetch_error@ path is exercised
-- end-to-end.
module DbSync.Worker.OffChain.Pool
  ( OffChainPoolWorker
  , OffChainPoolConfig (..)
  , defaultOffChainPoolConfig

    -- * Lifecycle
  , mkOffChainPoolWorker
  , closeOffChainPoolWorker

    -- * Single-cycle entry (exported for tests)
  , runOnePoolCycle
  , poolHooks

    -- * Stub fetcher (used in this slice)
  , stubPoolFetcher
  ) where

import Cardano.Prelude

import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import qualified Hasql.Connection as Conn
import qualified Hasql.Connection.Settings as Settings
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (PoolHashId, PoolMetadataRefId)
import DbSync.Db.Schema.OffChainPool
  ( OffChainPoolData (..)
  , OffChainPoolFetchError (..)
  )
import DbSync.Db.Statement.OffChainPool
  ( PendingPoolFetch (..)
  , insertOffChainPoolDataRowStmt
  , insertOffChainPoolFetchErrorRowStmt
  , queryNewPendingPoolFetchesStmt
  , queryRetryPendingPoolFetchesStmt
  , selectMaxRetryCountStmt
  )
import DbSync.Error (throwDb)
import DbSync.Trace.Types (AppTracer)
import DbSync.Worker.OffChain.Fetcher
  ( OffChainHooks (..)
  , OffChainWorker
  , closeOffChainWorker
  , mkOffChainWorker
  , runOneCycle
  )
import DbSync.Worker.OffChain.Retry (Retry (..), retryAgain)
import DbSync.Worker.OffChain.Types
  ( FetchError (..)
  , OffChainFetcher (..)
  , PoolMetadata (..)
  , PoolMetadataRef (..)
  )

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

type OffChainPoolWorker = OffChainWorker PendingPoolFetch

-- | Knobs the consumer wires from config / test harness.
data OffChainPoolConfig = OffChainPoolConfig
  { opcSleepMicros :: !Int
  , opcBatchSize   :: !Int32
  }

-- | Production defaults: 5-minute cycle, 100 refs per cycle. Tests
-- override 'opcSleepMicros' with a much shorter value.
defaultOffChainPoolConfig :: OffChainPoolConfig
defaultOffChainPoolConfig = OffChainPoolConfig
  { opcSleepMicros = 5 * 60 * 1_000_000
  , opcBatchSize   = 100
  }

componentLabel :: Text
componentLabel = "OffChainPoolWorker"

-- ---------------------------------------------------------------------------
-- * Lifecycle
-- ---------------------------------------------------------------------------

mkOffChainPoolWorker
  :: AppTracer
  -> Settings.Settings
  -> OffChainPoolConfig
  -> OffChainFetcher
  -> IO OffChainPoolWorker
mkOffChainPoolWorker tracer settings cfg fetcher =
  mkOffChainWorker
    tracer
    componentLabel
    settings
    (opcSleepMicros cfg)
    (opcBatchSize cfg)
    (poolHooks fetcher)

closeOffChainPoolWorker :: OffChainPoolWorker -> IO ()
closeOffChainPoolWorker = closeOffChainWorker

-- | Drive a single cycle against an externally-owned connection.
-- Used by integration tests so they do not need the sleeping loop.
runOnePoolCycle
  :: AppTracer
  -> Conn.Connection
  -> Int32
  -> OffChainFetcher
  -> IO ()
runOnePoolCycle tracer conn batchSize fetcher =
  runOneCycle tracer componentLabel conn batchSize (poolHooks fetcher)

-- ---------------------------------------------------------------------------
-- * Hooks
-- ---------------------------------------------------------------------------

-- | The complete hook triple for a pool worker.
poolHooks :: OffChainFetcher -> OffChainHooks PendingPoolFetch PoolMetadata
poolHooks fetcher = OffChainHooks
  { ohLoadPending = loadPendingPoolFetches
  , ohFetch       = poolFetch fetcher
  , ohPersist     = persistPoolResult
  }

-- | Read the work queue from PG: new refs first, then retry-due
-- failures filtered by the backoff schedule.
loadPendingPoolFetches :: Conn.Connection -> Int32 -> IO [PendingPoolFetch]
loadPendingPoolFetches conn maxCount = do
  now     <- utcTimeToPOSIXSeconds <$> getCurrentTime
  fresh   <- runStmt conn maxCount queryNewPendingPoolFetchesStmt
  retried <- runStmt conn maxCount queryRetryPendingPoolFetchesStmt
  pure $ fresh <> filter (isDue now) retried

isDue :: POSIXTime -> PendingPoolFetch -> Bool
isDue now ppf = case ppfPrevFetchTime ppf of
  Nothing -> True
  Just t  ->
    let r = retryAgain (utcTimeToPOSIXSeconds t) (ppfPrevRetryCount ppf)
    in retryRetryTime r <= now

-- | Project a 'PendingPoolFetch' into the public 'PoolMetadataRef'
-- shape that 'OffChainFetcher' speaks.
toFetchRef :: PendingPoolFetch -> PoolMetadataRef
toFetchRef ppf = PoolMetadataRef
  { pmrPoolId   = mempty
  , pmrUrl      = ppfUrl ppf
  , pmrMetaHash = ppfHash ppf
  }

-- | Invoke the fetcher and normalise its 'FetchError' to a short
-- text for the @fetch_error@ column.
poolFetch
  :: OffChainFetcher
  -> PendingPoolFetch
  -> IO (Either Text PoolMetadata)
poolFetch fetcher ppf = do
  res <- ofFetchPoolMetadata fetcher (toFetchRef ppf)
  pure $ case res of
    Right ok -> Right ok
    Left  e  -> Left (renderFetchError e)

renderFetchError :: FetchError -> Text
renderFetchError = \case
  FetchErrorHttp t           -> "http: " <> t
  FetchErrorHashMismatch _ _ -> "hash mismatch"
  FetchErrorDecode t         -> "decode: " <> t
  FetchErrorTooLarge n       -> "too large: " <> show n

-- ---------------------------------------------------------------------------
-- * Persistence
-- ---------------------------------------------------------------------------

persistPoolResult
  :: Conn.Connection
  -> PendingPoolFetch
  -> Either Text PoolMetadata
  -> IO ()
persistPoolResult conn ppf outcome = do
  now <- getCurrentTime
  case outcome of
    Right pm  -> writeSuccess conn (ppfPoolId ppf) (ppfPmrId ppf) pm
    Left  err -> writeError conn (ppfPoolId ppf) (ppfPmrId ppf) now err

writeSuccess
  :: Conn.Connection
  -> PoolHashId
  -> PoolMetadataRefId
  -> PoolMetadata
  -> IO ()
writeSuccess conn phId pmrId pm = do
  let row = OffChainPoolData
        { offChainPoolDataPoolId     = phId
        , offChainPoolDataTickerName = pmTicker pm
        , offChainPoolDataHash       = pmRawJson pm
        , offChainPoolDataJson       =
            TE.decodeUtf8With (\_ _ -> Just '\xFFFD') (pmRawJson pm)
        , offChainPoolDataBytes      = pmRawJson pm
        , offChainPoolDataPmrId      = pmrId
        }
  runStmt conn row insertOffChainPoolDataRowStmt

writeError
  :: Conn.Connection
  -> PoolHashId
  -> PoolMetadataRefId
  -> UTCTime
  -> Text
  -> IO ()
writeError conn phId pmrId now err = do
  retryNo <- nextRetryCount conn phId pmrId
  let row = OffChainPoolFetchError
        { offChainPoolFetchErrorPoolId     = phId
        , offChainPoolFetchErrorFetchTime  = now
        , offChainPoolFetchErrorPmrId      = pmrId
        , offChainPoolFetchErrorFetchError = err
        , offChainPoolFetchErrorRetryCount = retryNo
        }
  runStmt conn row insertOffChainPoolFetchErrorRowStmt

-- | @prev + 1@ for the next attempt, or @0@ if no prior error has
-- been recorded for this @(pool_id, pmr_id)@ pair.
nextRetryCount
  :: Conn.Connection
  -> PoolHashId
  -> PoolMetadataRefId
  -> IO Word64
nextRetryCount conn phId pmrId = do
  mPrev <- runStmt conn (phId, pmrId) selectMaxRetryCountStmt
  pure $ case mPrev of
    Just n  -> n + 1
    Nothing -> 0

-- ---------------------------------------------------------------------------
-- * Stub fetcher
-- ---------------------------------------------------------------------------

-- | Always returns an HTTP error. Exercises the @off_chain_pool_fetch_error@
-- path end-to-end without leaving PG.
stubPoolFetcher :: OffChainFetcher
stubPoolFetcher = OffChainFetcher
  { ofFetchPoolMetadata = \_ ->
      pure $ Left (FetchErrorHttp "stub: HTTP fetcher not yet implemented")
  , ofFetchVoteMetadata = \_ ->
      pure $ Left (FetchErrorHttp "stub: HTTP fetcher not yet implemented")
  , ofGetPendingPools = pure []
  , ofGetPendingVotes = pure []
  , ofSavePoolResult  = \_ _ -> pure ()
  , ofSaveVoteResult  = \_ _ -> pure ()
  }

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

runStmt :: Conn.Connection -> a -> Stmt.Statement a b -> IO b
runStmt conn p stmt = do
  res <- Conn.use conn (Sess.statement p stmt)
  case res of
    Right b -> pure b
    Left e  -> throwDb $ "OffChainPoolWorker session failed: " <> show e
