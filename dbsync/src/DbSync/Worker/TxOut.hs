{-# LANGUAGE ScopedTypeVariables #-}

-- | Background worker that handles the post-COPY FK fills on the
-- UTxO-feature tables. Drains per-epoch buffers produced by the main
-- extractor thread and writes @address@ rows, @tx_out.address_id@,
-- @collateral_tx_out.address_id@, and (when the feature is on)
-- @tx_out.consumed_by_tx_id@ to PostgreSQL.
--
-- One worker per writable table is the rule (see
-- PLANS/WORKER-CONVENTIONS.md). The four hook calls run on a single
-- dedicated PG connection in sequence, so the worker cannot deadlock
-- against itself on overlapping @tx_out@ rows even when the same row
-- gets both @address_id@ and @consumed_by_tx_id@ writes in one epoch.
--
-- Lifecycle:
--
--   1. 'mkTxOutWorker' allocates the queue and an 'Async' running
--      the loop on a dedicated PG connection.
--   2. The consumer calls 'enqueueTxOutJob' at each epoch boundary;
--      back-pressure stops the main pipeline if the worker falls
--      more than 'txOutWorkerQueueBound' epochs behind.
--   3. 'awaitTxOutDrained' blocks until every queued job has been
--      processed — used at the 'IngestChainHistory' \/
--      'PreparingForVolatileTail' transition.
--   4. 'closeTxOutWorker' cancels the worker thread and releases
--      the PG connection.
module DbSync.Worker.TxOut
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
  , TxId (..)
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
import DbSync.Db.Statement.ConsumedBy
  ( bulkUpdateConsumedByTxIdStmt
  )
import DbSync.Db.Statement.TxOut
  ( bulkUpdateTxOutAddressIdsStmt
  )
import DbSync.Error (throwDb)
import DbSync.Worker.TxOut.AddressBuffer
  ( EpochAddressBuffer (..)
  )
import DbSync.Worker.TxOut.ConsumedByBuffer
  ( EpochConsumedByBuffer (..)
  )
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..), logThreadExit)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | One epoch's worth of post-COPY UTxO work.
--
-- 'tjConsumedBy' is 'Nothing' when the @utxo.consumed_by_tx_id@
-- feature flag is off; the worker then skips the corresponding
-- bulk UPDATE.
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
    -- ^ Source of truth for the next @address.id@ to assign during
    -- 'IngestChainHistory'. PG sequences are not created until
    -- 'PreparingForVolatileTail' (see 'DbSync.Db.Schema.Init'), so the
    -- worker allocates IDs in-process and 'mkBoundarySyncStateRow'
    -- persists the next-to-assign value into 'ssrAddressIdCounter'
    -- so a crash + resume can pick up where the worker left off.
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

-- | Side-effect operations the worker performs per job, factored
-- out so tests can stub them with in-memory equivalents.
--
-- The bulk shape lets one epoch's worth of work fold into a constant
-- number of PG round-trips (one bulk SELECT, one bulk INSERT, one
-- bulk UPDATE per child table) instead of one round-trip per row.
data TxOutHooks = TxOutHooks
  { thBulkResolveAddresses
      :: !([(ShortByteString, Address)] -> IO (Map ShortByteString AddressId))
    -- ^ Return the canonical 'AddressId' for every input raw:
    -- existing rows are looked up; missing rows are allocated from
    -- the in-process counter and inserted in bulk.
  , thBulkUpdateTxOut
      :: !([(TxOutId, AddressId)] -> IO ())
    -- ^ Fill in @tx_out.address_id@ for each @(tx_out.id, address.id)@
    -- pair in one statement.
  , thBulkUpdateCollateral
      :: !([(CollateralTxOutId, AddressId)] -> IO ())
    -- ^ Same as 'thBulkUpdateTxOut' for @collateral_tx_out@.
  , thBulkUpdateConsumedBy
      :: !([(TxOutId, TxId)] -> IO ())
    -- ^ Fill in @tx_out.consumed_by_tx_id@ for each
    -- @(producer_tx_out_id, consumer_tx_id)@ pair. No-op when the
    -- consumed-by feature is off (worker is handed
    -- @tjConsumedBy = Nothing@ and the hook is never called).
  }

-- | Production hook set, talking to PG via the worker's dedicated
-- connection. The @IORef Int64@ is the in-process source of truth
-- for the next @address.id@ to assign during 'IngestChainHistory' —
-- PG sequences don't exist yet at this phase.
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

-- | Cancel the worker and close its PG connection.
closeTxOutWorker :: TxOutWorker -> IO ()
closeTxOutWorker tw = do
  cancel (twAsync tw)
  Conn.release (twConn tw)

-- ---------------------------------------------------------------------------
-- * Job submission
-- ---------------------------------------------------------------------------

-- | Push a job onto the queue. Blocks if the queue is full
-- (back-pressure: main pipeline waits for the worker to catch up).
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
-- * Worker loop
-- ---------------------------------------------------------------------------

-- | Generic worker loop, parameterised by the per-job hooks. The
-- production path uses 'realTxOutHooks'; tests inject in-memory
-- equivalents.
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
      processTxOutJob hooks job
      atomically $ STM.modifyTVar' inFlight (\n -> n - 1)
      for_ mTracer $ \tracer ->
        traceWith tracer $ LogMsg Info "TxOutWorker"
          ("resolved epoch " <> show (unEpochNo (tjEpoch job))) Nothing

-- | Resolve one epoch's buffers in (up to) four bulk statements:
--
--   1. 'thBulkResolveAddresses' returns the @raw -> AddressId@ map
--      for every unique raw in the address buffer (existing +
--      freshly allocated\/inserted).
--   2. One bulk UPDATE fills @tx_out.address_id@ for every
--      @(tx_out_id, raw)@ pair.
--   3. One bulk UPDATE fills @collateral_tx_out.address_id@.
--   4. When 'tjConsumedBy' is 'Just', one bulk UPDATE fills
--      @tx_out.consumed_by_tx_id@ from the producer/consumer pairs.
processTxOutJob :: TxOutHooks -> TxOutJob -> IO ()
processTxOutJob hooks job = do
  let addr      = tjAddress job
      addrPairs = Map.toList (eabAddresses addr)

  rawToId <- thBulkResolveAddresses hooks addrPairs

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

  thBulkUpdateTxOut hooks txOutPairs
  thBulkUpdateCollateral hooks collPairs

  for_ (tjConsumedBy job) $ \cb -> do
    let consumedPairs = zip
          (Foldable.toList (ecbProducerTxOutIds cb))
          (Foldable.toList (ecbConsumerTxIds cb))
    thBulkUpdateConsumedBy hooks consumedPairs

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
