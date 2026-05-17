{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : DbSync.Extractor.EpochBoundary
Description : Epoch-boundary projection — writes ada_pots (and, in
              future, reward / drep_distr / pool_stat / epoch_param
              / epoch_state) at each epoch transition.

Owns the @ada_pots@ table (and the other epoch-derived tables as
they land — see LEDGER-PLAN.md §15). Driven not by the per-block
'pdProcess' callback but by the consumer's epoch-boundary handler
in 'DbSync.Phase.Ingest.Consumer', which:

  1. Detects a boundary by comparing the current block's epoch to
     the previously-observed one.
  2. Waits on 'leLatestApplyResult' until the LedgerWorker has
     produced an 'ApplyResult' carrying @apNewEpoch = Just …@ for
     this transition.
  3. Calls 'runEpochBoundary' (this module) to write the boundary
     rows to the per-table loader-stream queues.
  4. Calls 'lsCommit' which drains every queue (including the
     boundary-table queues) /in parallel/ across the per-table
     worker connections — that parallelism is option γ from
     LEDGER-PLAN.md §15.

When the ledger feature is disabled, 'runEpochBoundary' is never
called (the consumer skips it on the ledger-disabled arm) and the
@ada_pots@ table is never written to. The schema is still created
because the extractor's 'pdTables' is unconditional — operators who
flip the ledger flag mid-deployment hit a clean
@dbsync_sync_state.ledger_enabled@ mismatch error rather than an
ambiguous schema state.
-}
module DbSync.Extractor.EpochBoundary
  ( -- * Extractor registration
    epochBoundaryExtractor

    -- * Boundary handler (called by the consumer)
  , runEpochBoundary
  ) where

import Cardano.Prelude

import qualified Cardano.Ledger.Shelley.AdaPots as Shelley
import qualified Cardano.Ledger.State as Ledger
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import qualified Data.Strict.Maybe as Strict

import qualified DbSync.Era.Shelley.EpochUpdate as Generic
import DbSync.Db.Schema.AdaPots (AdaPots (..), adaPotsTableDef)
import DbSync.Db.Schema.Ids (BlockId)
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Ledger.Types (ApplyResult (..))
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.StateQuery (SlotDetails (..))
import DbSync.Util (coinToDbLovelace)
import DbSync.Writer (HasWriter (..), Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor registration
-- ---------------------------------------------------------------------------

-- | The EpochBoundary extractor.
--
-- Only registers tables — the per-block 'pdProcess' is a no-op
-- because epoch-boundary work is event-driven, not block-driven.
-- 'runEpochBoundary' is what actually does the work, called from
-- 'DbSync.Phase.Ingest.Consumer' when a boundary is detected.
--
-- Currently registers @ada_pots@. Other boundary tables
-- (@reward@, @reward_rest@, @drep_distr@, @pool_stat@,
-- @epoch_param@, @epoch_state@) will be added here as they land —
-- see LEDGER-PLAN.md §15.5.
epochBoundaryExtractor :: ExtractorDef
epochBoundaryExtractor = ExtractorDef
  { pdName         = "epoch_boundary"
  , pdVersion      = 1
  , pdDependencies = [("core", 1)]
  , pdTables       = [adaPotsTableDef]
  , pdProcess      = \_ -> pure ()  -- No-op; consumer drives runEpochBoundary
  }

-- ---------------------------------------------------------------------------
-- * Boundary handler
-- ---------------------------------------------------------------------------

-- | Run the epoch-boundary writes for a single transition.
--
-- Reads the @apNewEpoch.neAdaPots@ payload from the 'ApplyResult'
-- and (if the ledger emitted ada-pots data for this boundary —
-- always 'Just' from Shelley onward) builds an 'AdaPots' row and
-- dispatches it to the @ada_pots@ COPY queue via 'writeAdaPots'.
--
-- Pre-Shelley boundaries don't emit ada-pots (the @AdaPots@ event
-- doesn't exist in Byron) and this function is a no-op for them.
--
-- Idempotent in the sense that the consumer is responsible for
-- calling it exactly once per boundary. Calling it twice would
-- write two rows for the same epoch.
runEpochBoundary
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => ApplyResult
  -> BlockId
  -> m ()
runEpochBoundary applyResult blockId =
  case apNewEpoch applyResult of
    Strict.Nothing -> pure ()  -- Not a boundary, or worker hasn't caught up
    Strict.Just newEpoch -> writeBoundaryAdaPots applyResult newEpoch blockId

-- ---------------------------------------------------------------------------
-- * AdaPots
-- ---------------------------------------------------------------------------

-- | Build and dispatch the 'AdaPots' row for the boundary, if the
-- ledger reported any pots data.
writeBoundaryAdaPots
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => ApplyResult
  -> Generic.NewEpoch
  -> BlockId
  -> m ()
writeBoundaryAdaPots applyResult newEpoch blockId =
  case Generic.neAdaPots newEpoch of
    Strict.Nothing -> pure ()  -- Pre-Shelley; nothing to write
    Strict.Just pots -> do
      resolver <- asks getResolver
      writer   <- asks getWriter
      apId <- liftIO $ assignAdaPotsId resolver
      let row = mkAdaPotsRow applyResult newEpoch blockId pots
      liftIO $ writeAdaPots writer apId row

-- | Build an 'AdaPots' record from the boundary's
-- 'Shelley.AdaPots' value.
--
-- The deposit pots (stake, drep, proposal) come from
-- 'Shelley.obligationsPot' — they are not direct fields on
-- 'Shelley.AdaPots' itself.
--
-- @utxo@ is taken /verbatim/ from the supplied pots — the caller
-- (the LedgerWorker via 'DbSync.Ledger.State.applyBlock') has
-- already applied the @fixUTxOPots@ correction so that the sum of
-- pots equals @maxLovelaceSupply@.
mkAdaPotsRow
  :: ApplyResult
  -> Generic.NewEpoch
  -> BlockId
  -> Shelley.AdaPots
  -> AdaPots
mkAdaPotsRow applyResult newEpoch blockId pots =
  AdaPots
    { adaPotsSlotNo            = unSlotNo (sdSlotNo (apSlotDetails applyResult))
    , adaPotsEpochNo           = unEpochNo (Generic.neEpoch newEpoch)
    , adaPotsTreasury          = coinToDbLovelace (Shelley.treasuryAdaPot pots)
    , adaPotsReserves          = coinToDbLovelace (Shelley.reservesAdaPot pots)
    , adaPotsRewards           = coinToDbLovelace (Shelley.rewardsAdaPot pots)
    , adaPotsUtxo              = coinToDbLovelace (Shelley.utxoAdaPot pots)
    , adaPotsDepositsStake     =
        coinToDbLovelace (Ledger.oblStake oblgs <> Ledger.oblPool oblgs)
    , adaPotsFees              = coinToDbLovelace (Shelley.feesAdaPot pots)
    , adaPotsBlockId           = blockId
    , adaPotsDepositsDrep      = coinToDbLovelace (Ledger.oblDRep oblgs)
    , adaPotsDepositsProposal  = coinToDbLovelace (Ledger.oblProposal oblgs)
    }
  where
    oblgs :: Ledger.Obligations
    oblgs = Shelley.obligationsPot pots


