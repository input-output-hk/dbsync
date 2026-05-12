{-# LANGUAGE ScopedTypeVariables #-}

-- | Background worker that drains the per-epoch
-- 'EpochAddressBuffer's produced by the main extractor thread and
-- writes the corresponding @address@ rows + @tx_out.address_id@\/
-- @collateral_tx_out.address_id@ FKs to PostgreSQL.
--
-- Lifecycle:
--
--   1. 'mkAddressResolver' allocates a job queue and an 'Async'
--      running the worker loop on a dedicated PG connection.
--   2. The consumer calls 'enqueueResolveJob' at each epoch
--      boundary; back-pressure stops the main pipeline if the
--      worker falls more than 'addressResolverQueueBound' epochs
--      behind.
--   3. 'awaitDrained' blocks until every queued job has been
--      processed — used at the 'IngestChainHistory' \/
--      'PreparingForChainTip' transition.
--   4. 'closeAddressResolver' cancels the worker thread and
--      releases the PG connection.
module DbSync.Resolver.AddressWorker
  ( -- * Types
    AddressResolver
  , ResolveJob (..)
  , WorkerHooks (..)

    -- * Lifecycle
  , mkAddressResolver
  , closeAddressResolver
  , addressResolverQueueBound

    -- * Job submission
  , enqueueResolveJob
  , awaitDrained

    -- * Counter access
  , readAddressIdCounter

    -- * Hook-based entry points (exported for tests)
  , runAddressResolverWith
  , realWorkerHooks
  ) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo (..))
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (TBQueue, TVar, newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Tracer (traceWith)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Hasql.Connection as Conn
import qualified Hasql.Connection.Settings as Settings
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Address
  ( Address (..)
  )
import DbSync.Db.Schema.Ids
  ( AddressId (..)
  , CollateralTxOutId (..)
  , StakeAddressId (..)
  , TxOutId (..)
  )
import DbSync.Db.Statement.Address
  ( BulkAddressInsert (..)
  , bulkInsertAddressesStmt
  , bulkSelectAddressIdsStmt
  )
import DbSync.Db.Statement.CollateralTxOut
  ( bulkUpdateCollateralTxOutAddressIdsStmt
  )
import DbSync.Db.Statement.TxOut
  ( bulkUpdateTxOutAddressIdsStmt
  )
import DbSync.Error (throwDb)
import DbSync.Resolver.AddressBuffer
  ( EpochAddressBuffer (..)
  )
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | A single epoch's worth of address-resolution work.
data ResolveJob = ResolveJob
  { rjEpoch  :: !EpochNo
  , rjBuffer :: !EpochAddressBuffer
  }

-- | Handle held by the consumer so it can enqueue jobs, wait for the
-- worker to catch up, and cancel it at shutdown.
data AddressResolver = AddressResolver
  { arQueue   :: !(TBQueue ResolveJob)
  , arInFlight :: !(TVar Int)
    -- ^ Number of jobs queued but not yet completed. The worker
    -- decrements after each job; 'awaitDrained' waits for it to
    -- reach 0.
  , arIdCounter :: !(IORef Int64)
    -- ^ Source of truth for the next @address.id@ to assign during
    -- 'IngestChainHistory'. PG sequences are not created until
    -- 'PreparingForChainTip' (see 'DbSync.Db.Schema.Init'), so the
    -- worker allocates IDs in-process and 'mkBoundarySyncStateRow'
    -- persists the next-to-assign value into 'ssrAddressIdCounter'
    -- so a crash + resume can pick up where the worker left off.
  , arWorker  :: !(Async ())
  , arConn    :: !Conn.Connection
  }

-- | Bounded queue depth. The main pipeline blocks if the worker
-- falls more than this many epochs behind.
addressResolverQueueBound :: Natural
addressResolverQueueBound = 4

-- ---------------------------------------------------------------------------
-- * Hooks
-- ---------------------------------------------------------------------------

-- | Side-effect operations the worker performs per job, factored
-- out so tests can stub them with in-memory equivalents.
--
-- The bulk shape lets one epoch's worth of address resolution fold
-- into a constant number of PG round-trips (one bulk SELECT, one
-- bulk INSERT, one bulk UPDATE per child table) instead of one
-- round-trip per address \/ tx_out \/ collateral row.
data WorkerHooks = WorkerHooks
  { whBulkResolveAddresses
      :: !([(ShortByteString, Address)] -> IO (Map ShortByteString AddressId))
    -- ^ Return the canonical 'AddressId' for every input raw:
    -- existing rows are looked up; missing rows are allocated from
    -- the in-process counter and inserted in bulk.
  , whBulkUpdateTxOut
      :: !([(TxOutId, AddressId)] -> IO ())
    -- ^ Fill in @tx_out.address_id@ for each @(tx_out.id, address.id)@
    -- pair in one statement.
  , whBulkUpdateCollateral
      :: !([(CollateralTxOutId, AddressId)] -> IO ())
    -- ^ Same as 'whBulkUpdateTxOut' for @collateral_tx_out@.
  }

-- | Production hook set, talking to PG via the worker's dedicated
-- connection. The @IORef Int64@ is the in-process source of truth
-- for the next @address.id@ to assign during 'IngestChainHistory' —
-- PG sequences don't exist yet at this phase.
realWorkerHooks :: Conn.Connection -> IORef Int64 -> WorkerHooks
realWorkerHooks conn idRef = WorkerHooks
  { whBulkResolveAddresses = resolveBulk conn idRef
  , whBulkUpdateTxOut = \pairs ->
      unless (null pairs) $
        let (txOutIds, aids) = unzip
              [ (getTxOutId tid, getAddressId aid) | (tid, aid) <- pairs ]
        in run conn (txOutIds, aids) bulkUpdateTxOutAddressIdsStmt
  , whBulkUpdateCollateral = \pairs ->
      unless (null pairs) $
        let (outIds, aids) = unzip
              [ (getCollateralTxOutId oid, getAddressId aid) | (oid, aid) <- pairs ]
        in run conn (outIds, aids) bulkUpdateCollateralTxOutAddressIdsStmt
  }

-- | Look up existing addresses, allocate ids for the missing ones,
-- bulk-insert the new rows, and return the full @raw -> AddressId@
-- map covering every input entry.
--
-- 1 or 2 PG round-trips: always a bulk SELECT, plus one bulk INSERT
-- when there are any new addresses to add.
resolveBulk
  :: Conn.Connection
  -> IORef Int64
  -> [(ShortByteString, Address)]
  -> IO (Map ShortByteString AddressId)
resolveBulk _ _ [] = pure Map.empty
resolveBulk conn idRef entries = do
  let rawList = map (SBS.fromShort . fst) entries
  existing <- run conn rawList bulkSelectAddressIdsStmt
  let existingMap :: Map ShortByteString AddressId
      existingMap = Map.fromList [ (SBS.toShort raw, aid) | (raw, aid) <- existing ]
      missing = [ (key, addr) | (key, addr) <- entries
                              , not (Map.member key existingMap) ]
  if null missing
    then pure existingMap
    else do
      let n         = length missing
          missingAddrs = map snd missing
          missingKeys  = map fst missing
      startId <- atomicModifyIORef' idRef $ \i -> (i + fromIntegral n, i)
      let newIds    = [ startId + i | i <- [0 .. fromIntegral n - 1] ]
          insertCols = BulkAddressInsert
            { baiIds            = newIds
            , baiAddresses      = map addressAddress missingAddrs
            , baiRaws           = map SBS.fromShort missingKeys
            , baiHasScript      = map addressHasScript missingAddrs
            , baiPaymentCreds   = map addressPaymentCred missingAddrs
            , baiStakeAddressId =
                map (fmap getStakeAddressId . addressStakeAddressId) missingAddrs
            }
      run conn insertCols bulkInsertAddressesStmt
      let newMap = Map.fromList (zip missingKeys (map AddressId newIds))
      pure (Map.union existingMap newMap)

-- ---------------------------------------------------------------------------
-- * Lifecycle
-- ---------------------------------------------------------------------------

-- | Spawn the worker with a dedicated PG connection. The 'Async' is
-- 'link'ed to the calling thread, so any worker exception propagates
-- to its parent.
--
-- The @initialAddressId@ is the next @address.id@ to assign. For a
-- fresh run it is @1@; for a resume it is @ssrAddressIdCounter@ from
-- 'dbsync_sync_state'.
mkAddressResolver :: AppTracer -> Settings.Settings -> Int64 -> IO AddressResolver
mkAddressResolver tracer settings initialAddressId = do
  conn <- openConn settings
  queue <- newTBQueueIO addressResolverQueueBound
  inFlight <- STM.newTVarIO 0
  idRef <- newIORef initialAddressId
  let hooks = realWorkerHooks conn idRef
  worker <- async $
    runAddressResolverWith (Just tracer) hooks queue inFlight
  link worker
  pure AddressResolver
    { arQueue     = queue
    , arInFlight  = inFlight
    , arIdCounter = idRef
    , arWorker    = worker
    , arConn      = conn
    }

-- | Cancel the worker and close its PG connection.
closeAddressResolver :: AddressResolver -> IO ()
closeAddressResolver ar = do
  cancel (arWorker ar)
  Conn.release (arConn ar)

-- ---------------------------------------------------------------------------
-- * Job submission
-- ---------------------------------------------------------------------------

-- | Push a job onto the queue. Blocks if the queue is full
-- (back-pressure: main pipeline waits for the worker to catch up).
enqueueResolveJob :: AddressResolver -> ResolveJob -> IO ()
enqueueResolveJob ar job = atomically $ do
  STM.modifyTVar' (arInFlight ar) (+ 1)
  writeTBQueue (arQueue ar) job

-- | Block until every queued job has been processed.
awaitDrained :: AddressResolver -> IO ()
awaitDrained ar = atomically $ do
  n <- STM.readTVar (arInFlight ar)
  when (n /= 0) STM.retry

-- ---------------------------------------------------------------------------
-- * Counter access
-- ---------------------------------------------------------------------------

-- | Snapshot the next-to-assign @address.id@. Safe to call only after
-- 'awaitDrained' returns at an epoch boundary: the worker is then
-- idle and the counter reflects exactly the rows it has inserted.
readAddressIdCounter :: AddressResolver -> IO Int64
readAddressIdCounter = readIORef . arIdCounter

-- ---------------------------------------------------------------------------
-- * Worker loop
-- ---------------------------------------------------------------------------

-- | Generic worker loop, parameterised by the per-job hooks. The
-- production path uses 'realWorkerHooks'; tests inject in-memory
-- equivalents.
runAddressResolverWith
  :: Maybe AppTracer
  -> WorkerHooks
  -> TBQueue ResolveJob
  -> TVar Int
  -> IO ()
runAddressResolverWith mTracer hooks queue inFlight =
  loop `catch` \(e :: SomeException) -> do
    for_ mTracer $ \tracer ->
      traceWith tracer $ LogMsg Error "AddressResolver"
        ("crashed: " <> show e) Nothing
    throwIO e
  where
    loop = forever $ do
      job <- atomically $ readTBQueue queue
      processJob hooks job
      atomically $ STM.modifyTVar' inFlight (\n -> n - 1)
      for_ mTracer $ \tracer ->
        traceWith tracer $ LogMsg Info "AddressResolver"
          ("resolved epoch " <> show (unEpochNo (rjEpoch job))) Nothing

-- | Resolve all addresses in a single epoch's buffer in three bulk
-- statements:
--
--   1. 'whBulkResolveAddresses' returns the @raw -> AddressId@ map
--      for every unique raw in the buffer (existing + freshly
--      allocated\/inserted).
--   2. One bulk UPDATE fills @tx_out.address_id@ for every
--      @(tx_out_id, raw)@ pair.
--   3. One bulk UPDATE fills @collateral_tx_out.address_id@ likewise.
processJob :: WorkerHooks -> ResolveJob -> IO ()
processJob hooks job = do
  let buf      = rjBuffer job
      addrPairs = Map.toList (eabAddresses buf)

  rawToId <- whBulkResolveAddresses hooks addrPairs

  let lookupOr msg key = case Map.lookup key rawToId of
        Just aid -> aid
        Nothing  -> panic msg

      txOutPairs =
        [ (txOutId, lookupOr "AddressResolver: tx_out raw missing from buffer address map" key)
        | (txOutId, key) <- eabTxOutAddresses buf
        ]
      collPairs =
        [ (outId, lookupOr "AddressResolver: collateral raw missing from buffer address map" key)
        | (outId, key) <- eabCollateralTxOutAddresses buf
        ]

  whBulkUpdateTxOut hooks txOutPairs
  whBulkUpdateCollateral hooks collPairs

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

openConn :: Settings.Settings -> IO Conn.Connection
openConn settings = do
  r <- Conn.acquire settings
  case r of
    Right c -> pure c
    Left e  -> throwDb $ "AddressResolver: failed to acquire PG connection: " <> show e

run :: Conn.Connection -> a -> Stmt.Statement a b -> IO b
run conn p stmt = do
  result <- Conn.use conn (Sess.statement p stmt)
  case result of
    Right b -> pure b
    Left e  -> throwDb $ "AddressResolver session failed: " <> show e
