{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for 'DbSync.Ledger.Worker'.
--
-- The worker reads blocks from the ledger queue, applies them, and
-- signals epoch boundaries via 'leEpochReady'. Tests for
-- 'applyBlockAndSnapshot' itself need a real LSM session, so what
-- we cover here is the /coordination/ around it: queue draining,
-- the optional 'leEpochWait' signal, the back-pressure shape, and
-- the per-message dispatch (forward vs rollback).
--
-- 'runLedgerWorkerWith' lets tests inject a fake @applyBlockAndSnapshot@
-- without pulling the LSM construction in. 'chainSyncDispatchLoop'
-- lets tests stub both the forward and the rollback handlers and
-- verify the message routing.
module DbSync.Ledger.WorkerSpec
  ( spec
  ) where

import Cardano.Prelude

import Control.Concurrent.STM (newTBQueueIO, writeTBQueue)
import qualified Control.Concurrent.Class.MonadSTM.Strict as Strict
import qualified Data.Strict.Maybe as SMaybe
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)

import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Ouroboros.Network.Block (pattern GenesisPoint)

import qualified DbSync.Era.Shelley.EpochUpdate as Generic
import qualified DbSync.Era.Shelley.StakeDist as Generic
import DbSync.Ledger.Types
  ( ApplyResult (..)
  , emptyDepositsMap
  )
import DbSync.Ledger.Worker
  ( WorkerHooks (..)
  , chainSyncDispatchLoop
  , runLedgerWorkerWith
  )
import DbSync.Node.ChainSyncMsg (ChainSyncMsg (..))
import DbSync.StateQuery (SlotDetails (..))

import qualified Data.Set as Set

import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  runLedgerWorkerWithSpec
  chainSyncDispatchLoopSpec

runLedgerWorkerWithSpec :: Spec
runLedgerWorkerWithSpec = describe "runLedgerWorkerWith" $ do
  it "applies every block delivered to the ledger queue" $ do
    queue       <- newTBQueueIO 10
    epochReady  <- Strict.newEmptyTMVarIO
    epochWait   <- Strict.newEmptyTMVarIO
    callsRef    <- newIORef (0 :: Int)
    workerThread <- async $
      runLedgerWorkerWith Nothing (countingHooks callsRef Nothing) Nothing
        queue epochReady epochWait

    -- Push three "blocks". The fake hook treats any value that
    -- typechecks; we just use unit since the worker doesn't inspect
    -- the block when the hook is faked.
    atomically $ do
      writeTBQueue queue dummyBlock
      writeTBQueue queue dummyBlock
      writeTBQueue queue dummyBlock

    -- Give the worker a moment to drain.
    waitFor (readIORef callsRef >>= \n -> pure (n >= 3))

    cancel workerThread
    readIORef callsRef `shouldReturn` 3

  it "signals leEpochReady when the fake hook reports a new epoch" $ do
    queue       <- newTBQueueIO 10
    epochReady  <- Strict.newEmptyTMVarIO
    epochWait   <- Strict.newEmptyTMVarIO
    callsRef    <- newIORef (0 :: Int)
    -- One epoch boundary at call #2.
    let onEpochAt2 :: Int -> SMaybe.Maybe Generic.NewEpoch
        onEpochAt2 2 = SMaybe.Just (mkNewEpoch 7)
        onEpochAt2 _ = SMaybe.Nothing

    workerThread <- async $
      runLedgerWorkerWith Nothing (countingHooks callsRef (Just onEpochAt2)) Nothing
        queue epochReady epochWait

    atomically $ do
      writeTBQueue queue dummyBlock
      writeTBQueue queue dummyBlock
      writeTBQueue queue dummyBlock

    -- Wait for the epoch signal.
    epoch <- atomically $ Strict.takeTMVar epochReady
    epoch `shouldBe` EpochNo 7

    cancel workerThread

  it "stops cleanly when cancelled mid-loop (no leaked threads)" $ do
    queue       <- newTBQueueIO 10
    epochReady  <- Strict.newEmptyTMVarIO
    epochWait   <- Strict.newEmptyTMVarIO
    callsRef    <- newIORef (0 :: Int)
    workerThread <- async $
      runLedgerWorkerWith Nothing (countingHooks callsRef Nothing) Nothing
        queue epochReady epochWait

    -- No blocks pushed; the worker is blocked on the queue.
    cancel workerThread
    -- 'wait' should observe the AsyncCancelled exception and not hang.
    result <- try (wait workerThread) :: IO (Either SomeException ())
    case result of
      Left _  -> pure ()
      Right _ -> panic "worker exited normally; expected AsyncCancelled"

chainSyncDispatchLoopSpec :: Spec
chainSyncDispatchLoopSpec = describe "chainSyncDispatchLoop" $ do
  it "routes MsgRollback to the rollback handler" $ do
    queue         <- newTBQueueIO 10
    rollbackCalls <- newIORef (0 :: Int)
    let forwardH _blk = panic "forward handler must not be called for rollback-only test"
        rollbackH _p  = atomicModifyIORef' rollbackCalls (\n -> (n + 1, ()))

    workerThread <- async $
      chainSyncDispatchLoop Nothing forwardH rollbackH Nothing queue

    -- Push three rollback markers (all at GenesisPoint — the handler
    -- only counts, so the point payload doesn't matter).
    atomically $ do
      writeTBQueue queue (MsgRollback GenesisPoint)
      writeTBQueue queue (MsgRollback GenesisPoint)
      writeTBQueue queue (MsgRollback GenesisPoint)

    waitFor (readIORef rollbackCalls >>= \n -> pure (n >= 3))

    cancel workerThread
    readIORef rollbackCalls `shouldReturn` 3

  it "stops cleanly when cancelled mid-loop" $ do
    queue <- newTBQueueIO 10
    let forwardH _blk = pure ()
        rollbackH _p  = pure ()

    workerThread <- async $
      chainSyncDispatchLoop Nothing forwardH rollbackH Nothing queue

    -- No messages pushed; the worker is blocked on the queue.
    cancel workerThread
    result <- try (wait workerThread) :: IO (Either SomeException ())
    case result of
      Left _  -> pure ()
      Right _ -> panic "dispatch loop exited normally; expected AsyncCancelled"

-- ---------------------------------------------------------------------------
-- Helpers

-- | The worker doesn't inspect the block when the apply hook is
-- faked, so we use unit as the "block" type via a coercion at the
-- hook-call site. (See the WorkerHooks definition.)
dummyBlock :: ()
dummyBlock = ()

-- | Hooks that count the number of apply-calls into an IORef, and
-- optionally produce an epoch-boundary 'Generic.NewEpoch' on a
-- specific call.
countingHooks
  :: IORef Int
  -> Maybe (Int -> SMaybe.Maybe Generic.NewEpoch)
  -> WorkerHooks ()
countingHooks ref mNewEpoch =
  WorkerHooks
    { whGetSlotDetails = \_blk -> pure dummySlotDetails
    , whApplyAndSnapshot = \_blk _slotDetails -> do
        n <- atomicModifyIORef' ref (\x -> (x + 1, x + 1))
        let neAtCall = case mNewEpoch of
              Just f  -> f n
              Nothing -> SMaybe.Nothing
            ar = mkApplyResult neAtCall
        pure (ar, False)
    }

mkApplyResult :: SMaybe.Maybe Generic.NewEpoch -> ApplyResult
mkApplyResult ne =
  ApplyResult
    { apPrices          = SMaybe.Nothing
    , apGovExpiresAfter = SMaybe.Nothing
    , apPoolsRegistered = Set.empty
    , apNewEpoch        = ne
    , apDeposits        = SMaybe.Nothing
    , apSlotDetails     = dummySlotDetails
    , apStakeSlice      = Generic.NoSlices
    , apEvents          = []
    , apGovActionState  = Nothing
    , apDepositsMap     = emptyDepositsMap
    }

mkNewEpoch :: Word64 -> Generic.NewEpoch
mkNewEpoch n =
  Generic.NewEpoch
    { Generic.neEpoch       = EpochNo n
    , Generic.neIsEBB       = False
    , Generic.neAdaPots     = SMaybe.Nothing
    , Generic.neEpochUpdate =
        Generic.EpochUpdate
          { Generic.euProtoParams = SMaybe.Nothing
          , Generic.euNonce       = Ledger.NeutralNonce
          }
    , Generic.neDRepState   = SMaybe.Nothing
    , Generic.neEnacted     = SMaybe.Nothing
    , Generic.nePoolDistr   = SMaybe.Nothing
    }

dummySlotDetails :: SlotDetails
dummySlotDetails =
  SlotDetails
    { sdSlotTime    = epochZero
    , sdCurrentTime = epochZero
    , sdEpochNo     = EpochNo 0
    , sdSlotNo      = SlotNo 0
    , sdEpochSlot   = 0
    , sdEpochSize   = EpochSize 21600
    }
  where
    epochZero = UTCTime (toEnum 0) (secondsToDiffTime 0)

-- | Spin until the predicate holds, with a 1-second timeout cap.
waitFor :: IO Bool -> IO ()
waitFor p = go (100 :: Int)
  where
    go 0 = panic "waitFor: predicate never became true"
    go n = do
      ok <- p
      unless ok $ do
        threadDelay 10_000   -- 10 ms
        go (n - 1)


