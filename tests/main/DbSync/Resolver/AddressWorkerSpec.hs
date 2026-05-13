{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the 'AddressResolver' worker loop using in-memory
-- hooks. They cover the queue/in-flight coordination and the
-- per-job processing: bulk address resolution, bulk tx_out FK
-- updates, and bulk collateral_tx_out FK updates. No PostgreSQL is
-- required.
module DbSync.Resolver.AddressWorkerSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo (..))
import qualified Control.Concurrent.STM as STM
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Address (Address (..))
import DbSync.Db.Schema.Ids (AddressId (..), CollateralTxOutId (..), TxOutId (..))
import DbSync.Resolver.AddressBuffer
  ( EpochAddressBuffer (..)
  , emptyEpochAddressBuffer
  , newAddressBufferRef
  , recordCollateralTxOut
  , recordTxOut
  , takeAndReset
  )
import DbSync.Resolver.AddressWorker
  ( ResolveJob (..)
  , WorkerHooks (..)
  , runAddressResolverWith
  )

import qualified Data.ByteString as BS

-- ---------------------------------------------------------------------------
-- * Test doubles
-- ---------------------------------------------------------------------------

-- | Capture every bulk-hook invocation in an 'IORef' so tests can
-- inspect the worker's effect after a job completes.
data Captured = Captured
  { capResolveCalls :: ![[(ShortByteString, Address)]]
    -- ^ One entry per bulk-resolve call, in invocation order. Each
    -- entry is the full @(key, addr)@ list the hook received.
  , capTxOutCalls   :: ![[(TxOutId, AddressId)]]
  , capCollCalls    :: ![[(CollateralTxOutId, AddressId)]]
  }
  deriving stock (Eq, Show)

emptyCaptured :: Captured
emptyCaptured = Captured [] [] []

-- | Total number of unique addresses resolved across all bulk calls.
capUniqueAddresses :: Captured -> Int
capUniqueAddresses = sum . map length . capResolveCalls

-- | Total number of tx_out updates across all bulk calls.
capTxOutUpdates :: Captured -> Int
capTxOutUpdates = sum . map length . capTxOutCalls

-- | Total number of collateral_tx_out updates across all bulk calls.
capCollUpdates :: Captured -> Int
capCollUpdates = sum . map length . capCollCalls

mkCapturingHooks :: IORef Captured -> IORef Int64 -> WorkerHooks
mkCapturingHooks capRef idCounter = WorkerHooks
  { whBulkResolveAddresses = \entries -> do
      atomicModifyIORef' capRef $ \c ->
        (c { capResolveCalls = capResolveCalls c ++ [entries] }, ())
      -- Mirror the production hook's semantics: allocate a fresh
      -- AddressId for every input key (the fake doesn't simulate
      -- "existing rows in PG", every key is treated as new).
      let assign key acc = do
            aid <- atomicModifyIORef' idCounter $ \n -> (n + 1, AddressId n)
            pure $! Map.insert key aid acc
      foldrM assign Map.empty (map fst entries)
  , whBulkUpdateTxOut = \pairs ->
      atomicModifyIORef' capRef $ \c ->
        (c { capTxOutCalls = capTxOutCalls c ++ [pairs] }, ())
  , whBulkUpdateCollateral = \pairs ->
      atomicModifyIORef' capRef $ \c ->
        (c { capCollCalls = capCollCalls c ++ [pairs] }, ())
  }

-- ---------------------------------------------------------------------------
-- * Fixtures
-- ---------------------------------------------------------------------------

addr1, addr2 :: ByteString
addr1 = BS.pack [0xaa, 0x01]
addr2 = BS.pack [0xaa, 0x02]

mkAddr :: ByteString -> Address
mkAddr raw = Address
  { addressAddress        = "test-addr"
  , addressRaw            = raw
  , addressHasScript      = False
  , addressPaymentCred    = Nothing
  , addressStakeAddressId = Nothing
  }

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "EpochAddressBuffer" $ do
    it "recordTxOut adds a unique address and one tx_out pair" $ do
      ref <- newAddressBufferRef
      recordTxOut ref (TxOutId 1) addr1 (mkAddr addr1)
      buf <- takeAndReset ref
      Map.size (eabAddresses buf) `shouldBe` 1
      length (eabTxOutAddresses buf) `shouldBe` 1
      length (eabCollateralTxOutAddresses buf) `shouldBe` 0

    it "two tx_outs with the same raw share one address entry" $ do
      ref <- newAddressBufferRef
      recordTxOut ref (TxOutId 1) addr1 (mkAddr addr1)
      recordTxOut ref (TxOutId 2) addr1 (mkAddr addr1)
      buf <- takeAndReset ref
      Map.size (eabAddresses buf) `shouldBe` 1
      length (eabTxOutAddresses buf) `shouldBe` 2

    it "two tx_outs with different raws produce two address entries" $ do
      ref <- newAddressBufferRef
      recordTxOut ref (TxOutId 1) addr1 (mkAddr addr1)
      recordTxOut ref (TxOutId 2) addr2 (mkAddr addr2)
      buf <- takeAndReset ref
      Map.size (eabAddresses buf) `shouldBe` 2
      length (eabTxOutAddresses buf) `shouldBe` 2

    it "takeAndReset leaves the buffer empty" $ do
      ref <- newAddressBufferRef
      recordTxOut ref (TxOutId 1) addr1 (mkAddr addr1)
      _ <- takeAndReset ref
      buf <- takeAndReset ref
      buf `shouldBe` emptyEpochAddressBuffer

    it "recordCollateralTxOut appends to the collateral list" $ do
      ref <- newAddressBufferRef
      recordCollateralTxOut ref (CollateralTxOutId 7) addr1 (mkAddr addr1)
      buf <- takeAndReset ref
      length (eabCollateralTxOutAddresses buf) `shouldBe` 1

  describe "AddressResolver worker loop (bulk hooks)" $ do
    it "folds one job into one bulk-resolve + one bulk-update call" $ do
      bufRef <- newAddressBufferRef
      recordTxOut bufRef (TxOutId 10) addr1 (mkAddr addr1)
      recordTxOut bufRef (TxOutId 11) addr2 (mkAddr addr2)
      buf <- takeAndReset bufRef

      capRef <- newIORef emptyCaptured
      idCounter <- newIORef 1
      let hooks = mkCapturingHooks capRef idCounter

      runOneJob hooks (ResolveJob (EpochNo 5) buf)

      cap <- readIORef capRef
      length (capResolveCalls cap) `shouldBe` 1
      length (capTxOutCalls cap)   `shouldBe` 1
      length (capCollCalls cap)    `shouldBe` 1
      capUniqueAddresses cap       `shouldBe` 2
      capTxOutUpdates cap          `shouldBe` 2
      capCollUpdates cap           `shouldBe` 0

    it "reuses the same AddressId for repeated raws in the same job" $ do
      bufRef <- newAddressBufferRef
      recordTxOut bufRef (TxOutId 20) addr1 (mkAddr addr1)
      recordTxOut bufRef (TxOutId 21) addr1 (mkAddr addr1)
      buf <- takeAndReset bufRef

      capRef <- newIORef emptyCaptured
      idCounter <- newIORef 1
      let hooks = mkCapturingHooks capRef idCounter

      runOneJob hooks (ResolveJob (EpochNo 1) buf)

      cap <- readIORef capRef
      capUniqueAddresses cap       `shouldBe` 1
      capTxOutUpdates cap          `shouldBe` 2
      -- Both tx_outs are updated to the same AddressId since the buffer
      -- only carries one address entry for the shared raw.
      let assignedIds = case capTxOutCalls cap of
            [single] -> map snd single
            _        -> []
      case assignedIds of
        [a, b] -> a `shouldBe` b
        _      -> panic ("expected two tx_out updates, got: " <> show (length assignedIds))

    it "drains and decrements inFlight per processed job" $ do
      queue    <- STM.newTBQueueIO 4
      inFlight <- STM.newTVarIO 0
      capRef   <- newIORef emptyCaptured
      idCounter <- newIORef 1
      let hooks = mkCapturingHooks capRef idCounter

      -- Enqueue two trivial jobs (empty buffers; the worker should
      -- still mark them complete).
      mapM_ (\e -> STM.atomically $ do
               STM.modifyTVar' inFlight (+ 1)
               STM.writeTBQueue queue (ResolveJob (EpochNo e) emptyEpochAddressBuffer))
        [1, 2]

      worker <- async (runAddressResolverWith Nothing hooks queue inFlight)

      -- Wait until both have been consumed.
      STM.atomically $ do
        n <- STM.readTVar inFlight
        when (n /= 0) STM.retry

      cancel worker
      _ <- waitCatch worker
      pure ()

    it "passes the buffer's unique raws to the bulk-resolve hook" $ do
      bufRef <- newAddressBufferRef
      recordTxOut bufRef (TxOutId 30) addr1 (mkAddr addr1)
      recordTxOut bufRef (TxOutId 31) addr2 (mkAddr addr2)
      recordCollateralTxOut bufRef (CollateralTxOutId 32) addr1 (mkAddr addr1)
      buf <- takeAndReset bufRef

      capRef <- newIORef emptyCaptured
      idCounter <- newIORef 100
      let hooks = mkCapturingHooks capRef idCounter

      runOneJob hooks (ResolveJob (EpochNo 9) buf)

      cap <- readIORef capRef
      case capResolveCalls cap of
        [entries] -> do
          let keys = sort (map fst entries)
          keys `shouldBe` sort [SBS.toShort addr1, SBS.toShort addr2]
        other -> panic ("expected one resolve call, got: " <> show (length other))
      capTxOutUpdates cap `shouldBe` 2
      capCollUpdates cap  `shouldBe` 1

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | Run the worker for exactly one job by enqueueing the job, kicking
-- the worker, and waiting for inFlight to drop to 0.
runOneJob :: WorkerHooks -> ResolveJob -> IO ()
runOneJob hooks job = do
  queue    <- STM.newTBQueueIO 1
  inFlight <- STM.newTVarIO 1
  STM.atomically $ STM.writeTBQueue queue job
  worker <- async (runAddressResolverWith Nothing hooks queue inFlight)
  STM.atomically $ do
    n <- STM.readTVar inFlight
    when (n /= 0) STM.retry
  cancel worker
  _ <- waitCatch worker
  pure ()
