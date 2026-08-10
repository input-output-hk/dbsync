{-# LANGUAGE ScopedTypeVariables #-}

-- | Background worker for the post-COPY FK fills on the UTxO tables.
--
-- It drains the per-epoch buffers that the extractor thread fills, then
-- writes @address@ rows, @tx_out.address_id@,
-- @collateral_tx_out.address_id@ and @tx_out.consumed_by_tx_id@.
--
-- The worker owns one PG connection and runs its hooks in sequence on
-- it. It therefore cannot deadlock against itself when one @tx_out@
-- row takes both an @address_id@ and a @consumed_by_tx_id@ write in
-- the same epoch.
module DbSync.Worker.TxOut.Worker
  ( -- * Types
    TxOutWorker
  , TxOutJob (..)
  , TxOutHooks (..)

    -- * Lifecycle
  , mkTxOutWorker
  , closeTxOutWorker
  , txOutWorkerQueueBound

    -- * Job submission
  , enqueueTxOutJob
  , awaitTxOutDrained

    -- * Counter access
  , readAddressIdCounter

    -- * Diagnostics
  , txOutWorkerDepths

    -- * Hook-based entry points (exported for tests)
  , runTxOutWorkerWith
  , realTxOutHooks
  ) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo (..))
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (TBQueue, TVar, newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Tracer (traceWith)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import qualified Data.Foldable as Foldable
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import qualified Hasql.Connection as Conn
import qualified Hasql.Connection.Settings as Settings
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Address
  ( Address (..)
  , addressFromRaw
  )
import DbSync.Db.Schema.Ids
  ( AddressId (..)
  , CollateralTxOutId (..)
  , StakeAddressId (..)
  , TxId (..)
  , TxOutId (..)
  )
import DbSync.Db.Statement.UTxO
  ( BulkAddressInsert (..)
  , bulkInsertAddressesStmt
  , bulkSelectAddressIdsStmt
  )
import DbSync.Db.Statement.UTxO
  ( bulkUpdateCollateralTxOutAddressIdsStmt
  )
import DbSync.Db.Statement.Worker.ConsumedBy
  ( bulkUpdateConsumedByTxIdStmt
  )
import DbSync.Db.Statement.UTxO
  ( bulkUpdateTxOutAddressIdsStmt
  )
import DbSync.Error (throwDb)
import DbSync.Worker.TxOut.AddressBuffer
  ( EpochAddressBuffer (..)
  )
import DbSync.Worker.TxOut.ConsumedByBuffer
  ( EpochConsumedByBuffer (..)
  )
import DbSync.Error.Render (logThreadExit)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | One epoch's worth of post-COPY UTxO work.
--
-- 'tjConsumedBy' is 'Nothing' when the @utxo.consumed_by_tx_id@
-- feature is off. The worker then skips that bulk UPDATE.
data TxOutJob = TxOutJob
  { tjEpoch      :: !EpochNo
  , tjAddress    :: !EpochAddressBuffer
  , tjConsumedBy :: !(Maybe EpochConsumedByBuffer)
  }

-- | Handle held by the consumer so it can enqueue jobs, wait for the
-- worker to catch up, and cancel it at shutdown.
data TxOutWorker = TxOutWorker
  { twQueue     :: !(TBQueue TxOutJob)
  , twInFlight  :: !(TVar Int)
    -- ^ Number of jobs queued but not yet completed. The worker
    -- decrements after each job; 'awaitTxOutDrained' waits for it
    -- to reach 0.
  , twIdCounter :: !(IORef Int64)
    -- ^ Next @address.id@ to assign. 'DbSync.Db.Schema.Init' creates
    -- the PG sequences only at 'PreparingForVolatileTail', so the
    -- worker allocates ids in-process. 'mkBoundarySyncStateRow' saves
    -- the value to 'ssrAddressIdCounter' so a resume continues from it.
  , twAsync     :: !(Async ())
  , twConn      :: !Conn.Connection
  }

-- | Bounded queue depth. The main pipeline blocks if the worker
-- falls more than this many epochs behind.
txOutWorkerQueueBound :: Natural
txOutWorkerQueueBound = 4

-- ---------------------------------------------------------------------------
-- * Hooks
-- ---------------------------------------------------------------------------

-- | Per-job side effects, factored out so tests can stub them.
--
-- The bulk shape folds one epoch of work into a constant number of PG
-- round-trips instead of one round-trip per row.
data TxOutHooks = TxOutHooks
  { thBulkResolveAddresses
      :: !([(ShortByteString, Maybe StakeAddressId)] -> IO (Map ShortByteString AddressId))
    -- ^ Return the 'AddressId' for every input raw. It looks up
    -- existing rows and bulk-inserts missing ones, with ids from the
    -- in-process counter.
  , thBulkUpdateTxOut
      :: !([(TxOutId, AddressId)] -> IO ())
    -- ^ Fill @tx_out.address_id@ in one statement.
  , thBulkUpdateCollateral
      :: !([(CollateralTxOutId, AddressId)] -> IO ())
    -- ^ Same as 'thBulkUpdateTxOut' for @collateral_tx_out@.
  , thBulkUpdateConsumedBy
      :: !([(TxOutId, TxId)] -> IO ())
    -- ^ Fill @tx_out.consumed_by_tx_id@ for each
    -- @(producer_tx_out_id, consumer_tx_id)@ pair.
  }

-- | Production hooks, on the worker's dedicated connection. The
-- 'IORef' is the 'twIdCounter' address-id source.
realTxOutHooks :: Conn.Connection -> IORef Int64 -> TxOutHooks
realTxOutHooks conn idRef = TxOutHooks
  { thBulkResolveAddresses = resolveBulk conn idRef
  , thBulkUpdateTxOut = \pairs ->
      unless (null pairs) $
        let (txOutIds, aids) = unzip
              [ (getTxOutId tid, getAddressId aid) | (tid, aid) <- pairs ]
        in run conn (txOutIds, aids) bulkUpdateTxOutAddressIdsStmt
  , thBulkUpdateCollateral = \pairs ->
      unless (null pairs) $
        let (outIds, aids) = unzip
              [ (getCollateralTxOutId oid, getAddressId aid) | (oid, aid) <- pairs ]
        in run conn (outIds, aids) bulkUpdateCollateralTxOutAddressIdsStmt
  , thBulkUpdateConsumedBy = \pairs ->
      unless (null pairs) $
        let (outIds, consumerIds) = unzip
              [ (getTxOutId oid, getTxId cid) | (oid, cid) <- pairs ]
        in run conn (outIds, consumerIds) bulkUpdateConsumedByTxIdStmt
  }

-- | Return the @raw -> AddressId@ map for every input entry.
--
-- 1 or 2 PG round-trips: always a bulk SELECT, plus one bulk INSERT
-- when there are new addresses to add.
resolveBulk
  :: Conn.Connection
  -> IORef Int64
  -> [(ShortByteString, Maybe StakeAddressId)]
  -> IO (Map ShortByteString AddressId)
resolveBulk _ _ [] = pure Map.empty
resolveBulk conn idRef entries = do
  let rawList = map (SBS.fromShort . fst) entries
  existing <- run conn rawList bulkSelectAddressIdsStmt
  let existingMap :: Map ShortByteString AddressId
      existingMap = Map.fromList [ (SBS.toShort raw, aid) | (raw, aid) <- existing ]
      missing = [ (key, mStakeId) | (key, mStakeId) <- entries
                                  , not (Map.member key existingMap) ]
  if null missing
    then pure existingMap
    else do
      let n            = length missing
          missingKeys  = map fst missing
          missingAddrs = [ addressFromRaw (SBS.fromShort key) mStakeId
                         | (key, mStakeId) <- missing ]
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
mkTxOutWorker :: AppTracer -> Settings.Settings -> Int64 -> IO TxOutWorker
mkTxOutWorker tracer settings initialAddressId = do
  conn <- openConn settings
  queue <- newTBQueueIO txOutWorkerQueueBound
  inFlight <- STM.newTVarIO 0
  idRef <- newIORef initialAddressId
  let hooks = realTxOutHooks conn idRef
  worker <- async $
    runTxOutWorkerWith (Just tracer) hooks queue inFlight
  link worker
  pure TxOutWorker
    { twQueue     = queue
    , twInFlight  = inFlight
    , twIdCounter = idRef
    , twAsync     = worker
    , twConn      = conn
    }

closeTxOutWorker :: TxOutWorker -> IO ()
closeTxOutWorker tw = do
  cancel (twAsync tw)
  Conn.release (twConn tw)

-- ---------------------------------------------------------------------------
-- * Job submission
-- ---------------------------------------------------------------------------

-- | Push a job onto the queue. Blocks while the queue is full, which
-- makes the main pipeline wait for the worker to catch up.
enqueueTxOutJob :: TxOutWorker -> TxOutJob -> IO ()
enqueueTxOutJob tw job = atomically $ do
  STM.modifyTVar' (twInFlight tw) (+ 1)
  writeTBQueue (twQueue tw) job

-- | Block until every queued job has been processed.
awaitTxOutDrained :: TxOutWorker -> IO ()
awaitTxOutDrained tw = atomically $ do
  n <- STM.readTVar (twInFlight tw)
  when (n /= 0) STM.retry

-- ---------------------------------------------------------------------------
-- * Counter access
-- ---------------------------------------------------------------------------

-- | Snapshot the next-to-assign @address.id@. Safe to call only after
-- 'awaitTxOutDrained' returns at an epoch boundary: the worker is
-- then idle and the counter reflects exactly the rows it has inserted.
readAddressIdCounter :: TxOutWorker -> IO Int64
readAddressIdCounter = readIORef . twIdCounter

-- ---------------------------------------------------------------------------
-- * Diagnostics
-- ---------------------------------------------------------------------------

-- | @(in-flight jobs, queued jobs)@ snapshot for diagnostics.
txOutWorkerDepths :: TxOutWorker -> IO (Int, Int)
txOutWorkerDepths tw = STM.atomically $ do
  inflight <- STM.readTVar (twInFlight tw)
  queued   <- STM.lengthTBQueue (twQueue tw)
  pure (inflight, fromIntegral queued)

-- ---------------------------------------------------------------------------
-- * Worker loop
-- ---------------------------------------------------------------------------

-- | Worker loop, parameterised by the per-job hooks.
runTxOutWorkerWith
  :: Maybe AppTracer
  -> TxOutHooks
  -> TBQueue TxOutJob
  -> TVar Int
  -> IO ()
runTxOutWorkerWith mTracer hooks queue inFlight =
  loop `catch` \(e :: SomeException) -> do
    for_ mTracer (logThreadExit "TxOutWorker" e)
    throwIO e
  where
    loop = forever $ do
      job <- atomically $ readTBQueue queue
      processTxOutJob mTracer hooks job
      atomically $ STM.modifyTVar' inFlight (\n -> n - 1)
      for_ mTracer $ \tracer ->
        traceWith tracer $ LogMsg Info "TxOutWorker"
          ("resolved epoch " <> show (unEpochNo (tjEpoch job)))

-- | Resolve one epoch's buffers: one address resolve, then up to three
-- bulk UPDATEs.
processTxOutJob :: Maybe AppTracer -> TxOutHooks -> TxOutJob -> IO ()
processTxOutJob mTracer hooks job = do
  let addr      = tjAddress job
      epoch     = tjEpoch job
      addrPairs = Map.toList (eabAddresses addr)

  rawToId <- probeStep mTracer epoch "resolveAddresses" $
    thBulkResolveAddresses hooks addrPairs

  let lookupOr msg key = case Map.lookup key rawToId of
        Just aid -> aid
        Nothing  -> panic msg

      txOutPairs =
        [ (txOutId, lookupOr "TxOutWorker: tx_out raw missing from buffer address map" key)
        | (txOutId, key) <- Foldable.toList (eabTxOutAddresses addr)
        ]
      collPairs =
        [ (outId, lookupOr "TxOutWorker: collateral raw missing from buffer address map" key)
        | (outId, key) <- Foldable.toList (eabCollateralTxOutAddresses addr)
        ]

  probeStep mTracer epoch "updateTxOut" $ thBulkUpdateTxOut hooks txOutPairs
  probeStep mTracer epoch "updateCollateral" $ thBulkUpdateCollateral hooks collPairs

  for_ (tjConsumedBy job) $ \cb -> do
    let consumedPairs = zip
          (Foldable.toList (ecbProducerTxOutIds cb))
          (Foldable.toList (ecbConsumerTxIds cb))
    probeStep mTracer epoch "updateConsumedBy" $
      thBulkUpdateConsumedBy hooks consumedPairs

-- | Bracket a per-job step with RTS allocation and live-heap counters,
-- then emit a Debug line.
--
-- This is a no-op without @+RTS -T@, as in the in-process e2e test
-- binary. 'getRTSStats' throws there, and this worker thread is
-- linked, so an unguarded call tears the whole pipeline down.
probeStep :: Maybe AppTracer -> EpochNo -> Text -> IO a -> IO a
probeStep Nothing _ _ act = act
probeStep (Just tracer) epoch label act = do
  enabled <- getRTSStatsEnabled
  if not enabled
    then act
    else do
      before <- getRTSStats
      r <- act
      after <- getRTSStats
      let allocDelta = allocated_bytes after - allocated_bytes before
          liveNow    = gcdetails_live_bytes (gc after)
      traceWith tracer $ LogMsg Debug "TxOutProbe"
        ( "epoch " <> show (unEpochNo epoch) <> " " <> label
          <> " alloc=" <> mb allocDelta <> " live=" <> mb liveNow
        )
      pure r

mb :: Word64 -> Text
mb b = show (b `div` 1048576) <> "MB"

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

openConn :: Settings.Settings -> IO Conn.Connection
openConn settings = do
  r <- Conn.acquire settings
  case r of
    Right c -> pure c
    Left e  -> throwDb $ "TxOutWorker: failed to acquire PG connection: " <> show e

run :: Conn.Connection -> a -> Stmt.Statement a b -> IO b
run conn p stmt = do
  result <- Conn.use conn (Sess.statement p stmt)
  case result of
    Right b -> pure b
    Left e  -> throwDb $ "TxOutWorker session failed: " <> show e
