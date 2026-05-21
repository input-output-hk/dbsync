{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the slot-details resolution fallback chain and the node-fetch
-- retry loop in 'DbSync.StateQuery.getSlotDetailsIOWith'.
--
-- The tests build two kinds of 'CardanoInterpreter':
--
-- * /wide/ — produced by walking the observed summary through the full
--   mainnet era progression. Its current era is 'EraUnbounded' so it
--   answers slot queries past Conway start.
--
-- * /stale/ — a hand-built 'History.Summary' with Byron bounded at
--   epoch 1 and no following era. Mimics the snapshot a still-replaying
--   cardano-node returns from @GetInterpreter@. Queries for slots past
--   the bound fail with 'PastHorizonException'.
--
-- These reproduce the failure mode from the field bug report: while
-- the node's LedgerDB is replaying from genesis, its 'GetInterpreter'
-- response is too narrow for the slots dbsync's consumer is
-- processing. The retry loop must not poison the cache with that
-- response and must recover when either the local sources or the node
-- catch up.
module DbSync.StateQuery.SlotDetailsSpec
  ( spec
  ) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import qualified Cardano.Slotting.Time as Slot
import Control.Concurrent.STM
  ( putTMVar
  , readTVar
  , takeTMVar
  , writeTVar
  )
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.SOP.NonEmpty as SOPNE
import qualified Data.Time.Clock.POSIX as POSIX
import Ouroboros.Consensus.Config (configLedger)
import Ouroboros.Consensus.HardFork.Combinator.Basics (hardForkLedgerConfigShape)
import qualified Ouroboros.Consensus.HardFork.History as History
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LSQ

import DbSync.Config.Genesis (mkTopLevelConfig, readCardanoGenesisConfig)
import DbSync.Config.Node (parseNodeConfig)
import DbSync.StateQuery
  ( CardanoInterpreter
  , RetryConfig (..)
  , SlotDetails (..)
  , StateQueryVar (..)
  , defaultRetryConfig
  , getSlotDetailsIOWith
  , newStateQueryVar
  )
import DbSync.StateQuery.ObservedSummary
  ( CardanoEraParams (..)
  , EraIdx (..)
  , ObservedSummary
  , currentInterpreter
  , extractCardanoEraParams
  , initObservedSummary
  , observeAt
  )
import DbSync.Trace.Backend (mkNullTracer)

import Test.Hspec
  ( Spec
  , describe
  , expectationFailure
  , it
  , shouldBe
  , shouldSatisfy
  )

-- ---------------------------------------------------------------------------
-- * Fixtures
-- ---------------------------------------------------------------------------

mainnetDir :: FilePath
mainnetDir = "fixtures/mainnet"

-- | An arbitrary 'SystemStart'. The tests assert on epoch / slot fields
-- which are derived purely from the interpreter; they do not assert on
-- the wall-clock time, so the exact reference point is irrelevant.
testSystemStart :: Slot.SystemStart
testSystemStart = Slot.SystemStart (POSIX.posixSecondsToUTCTime 0)

-- | Load mainnet configs once per test (the parse + decode is cheap).
-- Returns the params plus the initial Byron-only observed summary.
loadMainnetFixture :: IO (CardanoEraParams, ObservedSummary)
loadMainnetFixture = do
  Right nc <- parseNodeConfig (mainnetDir <> "/config.json")
  Right gc <- readCardanoGenesisConfig nc mainnetDir
  let tlc    = mkTopLevelConfig nc gc
      shape  = hardForkLedgerConfigShape (configLedger tlc)
      params = extractCardanoEraParams shape
      os0    = initObservedSummary tlc
  pure (params, os0)

-- ---------------------------------------------------------------------------
-- * Interpreter builders
-- ---------------------------------------------------------------------------

-- | Walk an observed summary through every mainnet era transition.
walkToConway :: ObservedSummary -> ObservedSummary
walkToConway =
    step ConwayIdx  (SlotNo 133_660_800)
  . step BabbageIdx (SlotNo  72_316_800)
  . step AlonzoIdx  (SlotNo  39_916_800)
  . step MaryIdx    (SlotNo  23_068_800)
  . step AllegraIdx (SlotNo  16_588_800)
  . step ShelleyIdx (SlotNo   4_492_800)
  where
    step e s os = snd (observeAt e s os)

-- | A "wide" interpreter — current era ('ConwayIdx') is unbounded, so
-- slot queries past Conway start answer correctly.
wideInterpreter :: ObservedSummary -> CardanoInterpreter
wideInterpreter = currentInterpreter . walkToConway

-- | A "stale" interpreter — a hand-built 'History.Summary' with Byron
-- bounded at epoch 1 and /no/ following era. Queries past slot 21,600
-- (the Byron→Shelley boundary on mainnet) fail with 'PastHorizonException'.
--
-- Models the @pastHorizonSummary@ in the field bug report: the
-- cardano-node's LedgerDB has reached the end of an era but has not yet
-- transitioned to the next one, so 'GetInterpreter' returns a Summary
-- with no unbounded entry.
staleInterpreter :: CardanoEraParams -> CardanoInterpreter
staleInterpreter params =
  case SOPNE.nonEmptyFromList [byronEra] of
    Just ne -> History.mkInterpreter (History.Summary ne)
    Nothing -> panic "staleInterpreter: nonEmptyFromList Nothing"
  where
    byronEnd :: History.Bound
    byronEnd =
      History.mkUpperBound (cepByron params) History.initBound (EpochNo 1)
    byronEra :: History.EraSummary
    byronEra = History.EraSummary
      { History.eraStart  = History.initBound
      , History.eraEnd    = History.EraEnd byronEnd
      , History.eraParams = cepByron params
      }

-- | Slot past Byron's bound (slot 21,600 on mainnet), past Shelley
-- start, well into Babbage. Only the wide interpreter answers.
slotInBabbage :: SlotNo
slotInBabbage = SlotNo 100_000_000

-- | Break an observed summary by observing a too-far-ahead era. The
-- resulting state has 'isObservationBroken' set, mimicking the
-- preview\/Mithril resume case where the first observed block is
-- already Babbage (jumping past Byron + 1).
--
-- A broken observed summary's current era is still 'EraUnbounded', so
-- it would happily answer queries with the /wrong/ era — which is
-- why 'tryLocalInterpreters' skips it.
breakObserved :: ObservedSummary -> ObservedSummary
breakObserved = snd . observeAt BabbageIdx (SlotNo 1000)

-- ---------------------------------------------------------------------------
-- * Mock LSQ handler
-- ---------------------------------------------------------------------------

data MockLsq = MockLsq
  { mlThread  :: !(Async ())
  , mlCallRef :: !(IORef Int)
  }

-- | Spawn a thread that responds to LSQ requests with the result of
-- @responder n@, where @n@ is the zero-based index of the call. Wraps
-- the production protocol: read the @(query, respVar)@ tuple from
-- 'sqvRequestVar', then put the response into @respVar@.
spawnMockLsq
  :: StateQueryVar
  -> (Int -> Either LSQ.AcquireFailure CardanoInterpreter)
  -> IO MockLsq
spawnMockLsq sqv responder = do
  callRef <- newIORef 0
  thread  <- async $ forever $ do
    (_query, respVar) <- atomically $ takeTMVar (sqvRequestVar sqv)
    n <- atomicModifyIORef' callRef (\c -> (c + 1, c))
    atomically $ putTMVar respVar (responder n)
  pure MockLsq { mlThread = thread, mlCallRef = callRef }

withMockLsq
  :: StateQueryVar
  -> (Int -> Either LSQ.AcquireFailure CardanoInterpreter)
  -> (IORef Int -> IO a)
  -> IO a
withMockLsq sqv responder action = do
  ml <- spawnMockLsq sqv responder
  action (mlCallRef ml) `finally` cancel (mlThread ml)

-- ---------------------------------------------------------------------------
-- * Retry policies
-- ---------------------------------------------------------------------------

-- | Fast retry: 5 attempts, 1ms between each. Used in tests where we
-- need the retry loop to spin quickly to completion.
fastRetry :: RetryConfig
fastRetry = RetryConfig
  { rcMaxAttempts   = 5
  , rcBackoffMicros = const 1_000
  }

-- | 3 attempts, 1ms between each — used by the "exhausted retries
-- throws" test to keep the suite under timeout.
threeAttemptRetry :: RetryConfig
threeAttemptRetry = RetryConfig
  { rcMaxAttempts   = 3
  , rcBackoffMicros = const 1_000
  }

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "DbSync.StateQuery.getSlotDetailsIOWith" $ do

  describe "defaultRetryConfig" $ do
    it "schedules 10 attempts totalling ~30 minutes of backoff" $ do
      let n = rcMaxAttempts defaultRetryConfig
          totalSecs =
            sum [ rcBackoffMicros defaultRetryConfig i | i <- [0 .. n - 2] ]
              `div` 1_000_000
      n `shouldBe` 10
      -- 9 backoffs between 10 attempts; schedule is 20, 40, 80, 160,
      -- then capped at 300; total = 20+40+80+160+5*300 = 1800s.
      totalSecs `shouldBe` 1800

  describe "fallback chain" $ do

    it "uses the cached interpreter when it answers the slot" $ do
      (params, os0) <- loadMainnetFixture
      sqv <- newStateQueryVarFromMainnet
      atomically $
        writeTVar (sqvInterpreterVar sqv) (Just (wideInterpreter os0))
      withMockLsq sqv neverCalled $ \callRef -> do
        sd <- getSlotDetailsIOWith fastRetry mkNullTracer sqv
                testSystemStart slotInBabbage
        sdSlotNo sd `shouldBe` slotInBabbage
        -- The cache was hit; no LSQ round-trip should have happened.
        readIORef callRef `shouldReturnP` 0
      -- Force-use of 'params' so the binding isn't unused-warning'd.
      cepByron params `shouldSatisfy` const True

    it "falls back to the observed summary when the cache cannot answer" $ do
      (params, _os0) <- loadMainnetFixture
      sqv <- newStateQueryVarFromMainnet
      -- Cache: stale (Byron bounded at epoch 1) — cannot answer slotInBabbage.
      atomically $
        writeTVar (sqvInterpreterVar sqv) (Just (staleInterpreter params))
      -- Observed: walk through every transition; current era unbounded.
      atomically $ do
        os <- readTVar (sqvObservedVar sqv)
        writeTVar (sqvObservedVar sqv) (walkToConway os)
      withMockLsq sqv neverCalled $ \callRef -> do
        sd <- getSlotDetailsIOWith fastRetry mkNullTracer sqv
                testSystemStart slotInBabbage
        sdSlotNo sd `shouldBe` slotInBabbage
        -- Cache untouched (we used the observed summary, didn't refresh).
        cached <- atomically $ readTVar (sqvInterpreterVar sqv)
        cached `shouldSatisfy` isJust
        readIORef callRef `shouldReturnP` 0

  describe "node-fetch retry" $ do

    it "retries a too-narrow node response and succeeds when a wider one arrives" $ do
      (params, os0) <- loadMainnetFixture
      sqv <- newStateQueryVarFromMainnet
      -- Break the observed summary so the slot can only be answered by
      -- the node fallback. Mirrors the preview\/Mithril setup where the
      -- first observed block jumps from Byron straight to Babbage.
      atomically $ writeTVar (sqvInterpreterVar sqv) Nothing
      atomically $ do
        os <- readTVar (sqvObservedVar sqv)
        writeTVar (sqvObservedVar sqv) (breakObserved os)
      -- Stub: first three calls return a stale interpreter; fourth onward
      -- returns wide.
      let responder n
            | n < 3     = Right (staleInterpreter params)
            | otherwise = Right (wideInterpreter os0)
      withMockLsq sqv responder $ \callRef -> do
        sd <- getSlotDetailsIOWith fastRetry mkNullTracer sqv
                testSystemStart slotInBabbage
        sdSlotNo sd `shouldBe` slotInBabbage
        -- Cache now holds the (wide) interpreter that answered.
        cached <- atomically $ readTVar (sqvInterpreterVar sqv)
        cached `shouldSatisfy` isJust
        -- Exactly four node round-trips: three stale + one wide.
        readIORef callRef `shouldReturnP` 4

    it "does not cache a too-narrow node interpreter mid-retry" $ do
      (params, _os0) <- loadMainnetFixture
      sqv <- newStateQueryVarFromMainnet
      atomically $ writeTVar (sqvInterpreterVar sqv) Nothing
      atomically $ do
        os <- readTVar (sqvObservedVar sqv)
        writeTVar (sqvObservedVar sqv) (breakObserved os)
      -- Always return stale — forces a throw. We assert the cache is
      -- never populated with the bad value.
      let responder _ = Right (staleInterpreter params)
      withMockLsq sqv responder $ \_callRef -> do
        result <- try @SomeException $
          getSlotDetailsIOWith threeAttemptRetry mkNullTracer sqv
            testSystemStart slotInBabbage
        case result of
          Right _ -> expectationFailure "expected throw"
          Left _  -> pure ()
        cached <- atomically $ readTVar (sqvInterpreterVar sqv)
        cached `shouldBe` Nothing

    it "throws after exhausting attempts when no source can answer" $ do
      (params, _os0) <- loadMainnetFixture
      sqv <- newStateQueryVarFromMainnet
      atomically $ writeTVar (sqvInterpreterVar sqv) Nothing
      atomically $ do
        os <- readTVar (sqvObservedVar sqv)
        writeTVar (sqvObservedVar sqv) (breakObserved os)
      let responder _ = Right (staleInterpreter params)
      withMockLsq sqv responder $ \callRef -> do
        result <- try @SomeException $
          getSlotDetailsIOWith threeAttemptRetry mkNullTracer sqv
            testSystemStart slotInBabbage
        result `shouldSatisfy` isLeft
        -- Exactly 'rcMaxAttempts' node round-trips — observed is
        -- broken, cache empty, so no local re-check ever short-circuits.
        readIORef callRef `shouldReturnP` rcMaxAttempts threeAttemptRetry

    it "stops retrying when a concurrent writer caches a wider interpreter" $ do
      (params, os0) <- loadMainnetFixture
      sqv <- newStateQueryVarFromMainnet
      atomically $ writeTVar (sqvInterpreterVar sqv) Nothing
      atomically $ do
        os <- readTVar (sqvObservedVar sqv)
        writeTVar (sqvObservedVar sqv) (breakObserved os)
      -- Node always responds stale; concurrent thread seeds wide into
      -- the cache during the first backoff. The retry's local re-check
      -- picks it up before going back to the node.
      let responder _ = Right (staleInterpreter params)
      withMockLsq sqv responder $ \callRef -> do
        seeder <- async $ do
          threadDelay 500  -- 0.5 ms; less than the 1 ms backoff
          atomically $
            writeTVar (sqvInterpreterVar sqv) (Just (wideInterpreter os0))
        sd <- getSlotDetailsIOWith fastRetry mkNullTracer sqv
                testSystemStart slotInBabbage
        cancel seeder
        sdSlotNo sd `shouldBe` slotInBabbage
        -- We may or may not have hit the node once before the writer
        -- landed; either way, we shouldn't have used all 5 attempts.
        n <- readIORef callRef
        n `shouldSatisfy` (< rcMaxAttempts fastRetry)

  describe "AcquireFailure handling" $ do

    it "retries on AcquireFailurePointTooOld" $ do
      (_params, os0) <- loadMainnetFixture
      sqv <- newStateQueryVarFromMainnet
      atomically $ writeTVar (sqvInterpreterVar sqv) Nothing
      atomically $ do
        os <- readTVar (sqvObservedVar sqv)
        writeTVar (sqvObservedVar sqv) (breakObserved os)
      let responder n
            | n < 2     = Left LSQ.AcquireFailurePointTooOld
            | otherwise = Right (wideInterpreter os0)
      withMockLsq sqv responder $ \callRef -> do
        sd <- getSlotDetailsIOWith fastRetry mkNullTracer sqv
                testSystemStart slotInBabbage
        sdSlotNo sd `shouldBe` slotInBabbage
        -- Exactly three calls: two AcquireFailures + one success.
        readIORef callRef `shouldReturnP` 3

-- ---------------------------------------------------------------------------
-- * Test helpers
-- ---------------------------------------------------------------------------

-- | Build a 'StateQueryVar' the same way production does, loading mainnet
-- config from fixtures so the initial observed summary is well-formed.
newStateQueryVarFromMainnet :: IO StateQueryVar
newStateQueryVarFromMainnet = do
  Right nc <- parseNodeConfig (mainnetDir <> "/config.json")
  Right gc <- readCardanoGenesisConfig nc mainnetDir
  newStateQueryVar (mkTopLevelConfig nc gc)

-- | A responder that fails the test if called. Use as the LSQ stub for
-- tests asserting "no node round-trip".
neverCalled :: Int -> Either LSQ.AcquireFailure CardanoInterpreter
neverCalled _ = panic "neverCalled: LSQ stub was invoked"

-- | 'shouldReturn' specialised to a return-then-equality assertion;
-- matches the local pattern in 'ObservedSummarySpec'.
shouldReturnP :: (Eq a, Show a) => IO a -> a -> IO ()
shouldReturnP action expected = do
  got <- action
  got `shouldBe` expected
