-- | Per-epoch deposit-parameter accumulator owned by the
-- 'LedgerWorker' thread and drained by the consumer at each epoch
-- boundary.
--
-- Only 'IngestChainHistory' uses this buffer. 'FollowingChainTip'
-- reads protocol params inline from the worker's
-- 'leLatestApplyResult'.
module DbSync.Worker.Ledger.DepositAccumulator
  ( -- * Types
    EpochParams (..)
  , EpochParamsRef

    -- * Construction
  , newEpochParamsRef

    -- * Mutation
  , recordEpochParams
  , drainCompletedEpochs
  , takeAllEpochs

    -- * Persistence
  , flushEpochParams

    -- * Pure helpers (exported for tests)
  , insertParams
  , partitionCompleted
  , depositColumnVectors
  ) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo (..))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as Map
import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import DbSync.SyncState.Row (ControlConnection (..), HasControlConnection (..))
import DbSync.Db.Statement.Worker.EpochParamPending (insertEpochParamPendingStmt)
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Error (throwDb)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | The deposit values that @pool_update@ and @stake_registration@
-- need when the ledger feature is enabled.
data EpochParams = EpochParams
  { epStakeKeyDeposit :: !DbLovelace
  , epPoolDeposit     :: !DbLovelace
  }
  deriving stock (Eq, Show)

-- | Per-epoch buffer indexed by 'EpochNo'. Mutable handle held on
-- 'DbSync.Worker.Ledger.Types.LedgerEnv'.
type EpochParamsRef = IORef (Map EpochNo EpochParams)

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

newEpochParamsRef :: IO EpochParamsRef
newEpochParamsRef = newIORef Map.empty

-- ---------------------------------------------------------------------------
-- * Mutation
-- ---------------------------------------------------------------------------

-- | Record this block's epoch params. Repeat calls for the same
-- 'EpochNo' overwrite; the values are constant within an epoch.
--
-- The worker calls this only past the resume replay boundary. The
-- previous run flushed every committed epoch before it advanced
-- @sync_state@, so accumulating during replay would re-insert rows.
recordEpochParams :: EpochParamsRef -> EpochNo -> EpochParams -> IO ()
recordEpochParams ref e ps =
  atomicModifyIORef' ref $ \m -> (insertParams e ps m, ())

-- | Atomically take every entry whose epoch is at or before the
-- watermark, leaving in-progress epochs in the buffer. The consumer
-- calls this at each epoch boundary with the just-completed epoch
-- as the watermark.
drainCompletedEpochs
  :: EpochParamsRef
  -> EpochNo
  -> IO (Map EpochNo EpochParams)
drainCompletedEpochs ref completedThrough =
  atomicModifyIORef' ref $ partitionCompleted completedThrough

-- | Atomically take every entry, regardless of epoch. Used at the
-- 'IngestChainHistory' → 'PreparingForVolatileTail' handoff to flush
-- the final in-progress epoch.
takeAllEpochs :: EpochParamsRef -> IO (Map EpochNo EpochParams)
takeAllEpochs ref = atomicModifyIORef' ref $ \m -> (Map.empty, m)

-- ---------------------------------------------------------------------------
-- * Persistence
-- ---------------------------------------------------------------------------

-- | INSERT the drained per-epoch params into @epoch_param_pending@.
-- Empty input is a no-op. Idempotent via @ON CONFLICT (epoch_no)
-- DO NOTHING@ on the underlying statement.
flushEpochParams
  :: (HasControlConnection env, MonadReader env m, MonadIO m)
  => Map EpochNo EpochParams
  -> m ()
flushEpochParams m
  | Map.null m = pure ()
  | otherwise = do
      ControlConnection conn <- asks getControlConnection
      let cols = depositColumnVectors m
      result <- liftIO $ Conn.use conn (Sess.statement cols insertEpochParamPendingStmt)
      case result of
        Right () -> pure ()
        Left  e  -> throwDb $ "flushEpochParams: " <> show e

-- ---------------------------------------------------------------------------
-- * Pure helpers
-- ---------------------------------------------------------------------------

insertParams
  :: EpochNo
  -> EpochParams
  -> Map EpochNo EpochParams
  -> Map EpochNo EpochParams
insertParams = Map.insert

-- | Split the map at the watermark. The result is the
-- @(remaining, toFlush)@ pair that 'atomicModifyIORef'' expects, so
-- the entries at or before the watermark come second.
partitionCompleted
  :: EpochNo
  -> Map EpochNo EpochParams
  -> (Map EpochNo EpochParams, Map EpochNo EpochParams)
partitionCompleted completedThrough m =
  let (toFlush, remaining) = Map.partitionWithKey (\k _ -> k <= completedThrough) m
   in (remaining, toFlush)

-- | Reshape the drained map into the three parallel column vectors
-- that 'insertEpochParamPendingStmt' takes: epoch_no, stake_key,
-- pool.
depositColumnVectors
  :: Map EpochNo EpochParams
  -> ([Word64], [DbLovelace], [DbLovelace])
depositColumnVectors m =
  foldr step ([], [], []) (Map.toAscList m)
  where
    step (e, p) (es, ss, ps) =
      (unEpochNo e : es, epStakeKeyDeposit p : ss, epPoolDeposit p : ps)
