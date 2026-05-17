-- | Per-epoch deposit-parameter accumulator owned by the
-- 'LedgerWorker' thread and drained by the consumer at each epoch
-- boundary.
--
-- The worker writes the current epoch's @(stake_key_deposit,
-- pool_deposit)@ pair on every applied block via
-- 'recordEpochParams'; subsequent writes for the same epoch are
-- idempotent (protocol params are constant within an epoch). At
-- each epoch boundary the consumer 'drainCompletedEpochs' to take
-- everything for epochs at or before a watermark, leaving any
-- in-progress entries behind.
--
-- The same mutable handle is used during 'IngestChainHistory' only;
-- 'FollowingChainTip' reads protocol params inline from the
-- worker's 'leLatestApplyResult' and does not exercise this buffer.
--
-- Replay handling: the worker calls 'recordEpochParams' only when
-- the block being applied is past the resume replay boundary. The
-- previous run will already have flushed every committed-epoch
-- entry to PG before advancing @sync_state@, so re-accumulating
-- during replay would either duplicate-INSERT (caught by the
-- @ON CONFLICT@ clause on @epoch_param_pending@) or leak stale
-- data if the clause were ever removed. The gate keeps the
-- invariant explicit.
module DbSync.Ledger.DepositAccumulator
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

import DbSync.Checkpoint.SyncState (ControlConnection (..), HasControlConnection (..))
import DbSync.Db.Statement.EpochParamPending (insertEpochParamPendingStmt)
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Error (throwDb)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | The protocol-param deposit values that 'pool_update' and
-- 'stake_registration' need when the ledger feature is enabled.
data EpochParams = EpochParams
  { epStakeKeyDeposit :: !DbLovelace
  , epPoolDeposit     :: !DbLovelace
  }
  deriving stock (Eq, Show)

-- | Per-epoch buffer indexed by 'EpochNo'. Mutable handle held on
-- 'DbSync.Ledger.Types.LedgerEnv'.
type EpochParamsRef = IORef (Map EpochNo EpochParams)

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

-- | Allocate an empty 'EpochParamsRef'.
newEpochParamsRef :: IO EpochParamsRef
newEpochParamsRef = newIORef Map.empty

-- ---------------------------------------------------------------------------
-- * Mutation
-- ---------------------------------------------------------------------------

-- | Record this block's epoch params. Subsequent calls for the same
-- 'EpochNo' overwrite (the values are constant within an epoch so
-- this is a no-op in practice). Cheap; safe to call on every
-- applied non-replay block.
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

-- | Insert (or overwrite) the params for one epoch.
insertParams
  :: EpochNo
  -> EpochParams
  -> Map EpochNo EpochParams
  -> Map EpochNo EpochParams
insertParams = Map.insert

-- | Split the map at the supplied watermark: everything at or before
-- the watermark is returned as the @(toFlush, remaining)@ pair the
-- caller can plug into 'atomicModifyIORef''.
partitionCompleted
  :: EpochNo
  -> Map EpochNo EpochParams
  -> (Map EpochNo EpochParams, Map EpochNo EpochParams)
partitionCompleted completedThrough m =
  let (toFlush, remaining) = Map.partitionWithKey (\k _ -> k <= completedThrough) m
   in (remaining, toFlush)

-- | Reshape the drained map into three parallel column vectors
-- matching 'insertEpochParamPendingStmt' (epoch_no, stake_key,
-- pool). Pure so callers can unit-test the projection without a
-- live connection.
depositColumnVectors
  :: Map EpochNo EpochParams
  -> ([Word64], [DbLovelace], [DbLovelace])
depositColumnVectors m =
  foldr step ([], [], []) (Map.toAscList m)
  where
    step (e, p) (es, ss, ps) =
      (unEpochNo e : es, epStakeKeyDeposit p : ss, epPoolDeposit p : ps)
