{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

-- | Locally-observed Cardano hard-fork summary.
--
-- Builds a 'History.Summary' (and hence an 'Interpreter') by /observing/
-- the era of each block delivered by ChainSync, instead of asking the
-- cardano-node for an authoritative summary via @GetInterpreter@.
--
-- This serves dbsync during the brief window where the node's LedgerDB
-- is still replaying, when a 'GetInterpreter' query would fail with
-- @AcquireFailurePointTooOld@. As soon as the node finishes replaying,
-- 'StateQuery' switches to the node's authoritative interpreter; this
-- module's output is discarded.
--
-- == Why this is the same point of truth as the node
--
-- Every 'CardanoBlock' carries its era in its constructor
-- ('BlockByron', 'BlockShelley', …). When the era of an incoming block
-- differs from the era of the previous block, an era transition just
-- occurred. The transition's epoch boundary is computed using the
-- previous era's 'EraParams' (epoch size in slots), which we sourced
-- from the consensus library's 'History.Shape' (built from the genesis
-- configs at startup). No hardcoded per-network table; the chain is the
-- point of truth.
--
-- == Limitation
--
-- If dbsync resumes from a non-Byron postgres tip without observing the
-- preceding transitions, the resulting summary will be incomplete. The
-- 'isObservationBroken' flag detects this case (the era of an incoming
-- block jumps more than one era past the currently-known era), and
-- callers fall back to the existing retry-with-backoff path. In
-- practice this only matters for the rare scenario where both dbsync
-- and the node are restarted simultaneously and the node hasn't
-- finished replaying yet.
module DbSync.StateQuery.ObservedSummary
  ( -- * Types
    ObservedSummary
  , EraIdx (..)
  , ObservationResult (..)
  , ObservedTransition (..)

    -- * Construction
  , initObservedSummary

    -- * Observing
  , observeBlock
  , observeAt

    -- * Snapshotting
  , currentInterpreter
  , currentSummary
  , isObservationBroken
  , currentEra

    -- * Internals exposed for testing
  , eraOf
  , extractCardanoEraParams
  , CardanoEraParams (..)
  ) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import Data.SOP.Counting (Exactly (ExactlyCons, ExactlyNil))
import Data.SOP.NonEmpty (nonEmptyFromList)

import Ouroboros.Consensus.Block (blockSlot)
import Ouroboros.Consensus.Cardano.Block
  ( CardanoBlock
  , CardanoEras
  , HardForkBlock
      ( BlockAllegra
      , BlockAlonzo
      , BlockBabbage
      , BlockByron
      , BlockConway
      , BlockDijkstra
      , BlockMary
      , BlockShelley
      )
  , StandardCrypto
  )
import Ouroboros.Consensus.Cardano.Node ()                    -- 'CanHardFork' instance for CardanoEras
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
import Ouroboros.Consensus.HardFork.Combinator.Basics (hardForkLedgerConfigShape)
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Shelley.HFEras ()                  -- per-era HFC instances
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol () -- 'LedgerSupportsProtocol' orphans

-- ---------------------------------------------------------------------------
-- * Era index
-- ---------------------------------------------------------------------------

-- | Index identifying which Cardano era a block belongs to. Order matches
-- 'CardanoEras' StandardCrypto'.
data EraIdx
  = ByronIdx
  | ShelleyIdx
  | AllegraIdx
  | MaryIdx
  | AlonzoIdx
  | BabbageIdx
  | ConwayIdx
  | DijkstraIdx
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Pattern-match a 'CardanoBlock' to its era index.
eraOf :: CardanoBlock StandardCrypto -> EraIdx
eraOf = \case
  BlockByron _    -> ByronIdx
  BlockShelley _  -> ShelleyIdx
  BlockAllegra _  -> AllegraIdx
  BlockMary _     -> MaryIdx
  BlockAlonzo _   -> AlonzoIdx
  BlockBabbage _  -> BabbageIdx
  BlockConway _   -> ConwayIdx
  BlockDijkstra _ -> DijkstraIdx

-- ---------------------------------------------------------------------------
-- * Cardano era params (record extracted from a Shape)
-- ---------------------------------------------------------------------------

-- | The eight per-era 'EraParams' for a Cardano chain, extracted once at
-- startup from the consensus-derived 'History.Shape'.
data CardanoEraParams = CardanoEraParams
  { cepByron    :: !History.EraParams
  , cepShelley  :: !History.EraParams
  , cepAllegra  :: !History.EraParams
  , cepMary     :: !History.EraParams
  , cepAlonzo   :: !History.EraParams
  , cepBabbage  :: !History.EraParams
  , cepConway   :: !History.EraParams
  , cepDijkstra :: !History.EraParams
  } deriving stock (Show)

-- | Extract the eight per-era 'EraParams' from the consensus 'Shape'.
--
-- The 'CardanoEras StandardCrypto' shape has exactly eight entries
-- (Byron, Shelley, Allegra, Mary, Alonzo, Babbage, Conway, Dijkstra).
-- Pattern-matching all the way through is verbose but proves the
-- extraction is total.
extractCardanoEraParams
  :: History.Shape (CardanoEras StandardCrypto)
  -> CardanoEraParams
extractCardanoEraParams shape =
  case History.getShape shape of
    ExactlyCons byron
      (ExactlyCons shelley
        (ExactlyCons allegra
          (ExactlyCons mary
            (ExactlyCons alonzo
              (ExactlyCons babbage
                (ExactlyCons conway
                  (ExactlyCons dijkstra ExactlyNil))))))) ->
      CardanoEraParams
        { cepByron    = byron
        , cepShelley  = shelley
        , cepAllegra  = allegra
        , cepMary     = mary
        , cepAlonzo   = alonzo
        , cepBabbage  = babbage
        , cepConway   = conway
        , cepDijkstra = dijkstra
        }

-- | Look up the 'EraParams' for a given era index.
paramsAt :: EraIdx -> CardanoEraParams -> History.EraParams
paramsAt = \case
  ByronIdx    -> cepByron
  ShelleyIdx  -> cepShelley
  AllegraIdx  -> cepAllegra
  MaryIdx     -> cepMary
  AlonzoIdx   -> cepAlonzo
  BabbageIdx  -> cepBabbage
  ConwayIdx   -> cepConway
  DijkstraIdx -> cepDijkstra

-- ---------------------------------------------------------------------------
-- * Observed summary
-- ---------------------------------------------------------------------------

-- | Locally-observed hard-fork summary state.
--
-- Maintained as a list of /closed/ past eras plus the /current/ era's
-- start bound. As ChainSync delivers blocks we pattern-match on the era
-- constructor and, on transitions, close the previous era and open the
-- next one.
data ObservedSummary = ObservedSummary
  { osCurrentEra      :: !EraIdx
    -- ^ Era of the most-recently-observed block
  , osCurrentEraStart :: !History.Bound
    -- ^ Start bound of the current era
  , osPastEras        :: ![History.EraSummary]
    -- ^ Closed past eras, in order (oldest first)
  , osParams          :: !CardanoEraParams
    -- ^ Per-era params, extracted from the consensus 'Shape' at startup
  , osBroken          :: !Bool
    -- ^ True if we observed an era jump greater than one era. The
    --   summary is then incomplete and callers should fall back to the
    --   node's interpreter.
  } deriving stock (Show)

-- | Initial state: only Byron is known, with no past eras.
--
-- Takes the entire 'TopLevelConfig' so that the per-era 'EraParams' can
-- be sourced directly from consensus rather than redefined locally.
initObservedSummary
  :: TopLevelConfig (CardanoBlock StandardCrypto)
  -> ObservedSummary
initObservedSummary topLevelCfg =
  ObservedSummary
    { osCurrentEra      = ByronIdx
    , osCurrentEraStart = History.initBound
    , osPastEras        = []
    , osParams          = extractCardanoEraParams shape
    , osBroken          = False
    }
  where
    shape :: History.Shape (CardanoEras StandardCrypto)
    shape = hardForkLedgerConfigShape (configLedger topLevelCfg)

-- ---------------------------------------------------------------------------
-- * Observing
-- ---------------------------------------------------------------------------

-- | Outcome of feeding one block to the observed summary.
data ObservationResult
  = Unchanged
  | NewTransition !ObservedTransition
  | ObservationBroken !EraIdx !EraIdx
    -- ^ The block's era is more than one ahead of the known current era.
    --   The summary is now incomplete; 'osBroken' is set.
  deriving stock (Eq, Show)

-- | A newly-observed era boundary.
data ObservedTransition = ObservedTransition
  { otFromEra :: !EraIdx
  , otToEra   :: !EraIdx
  , otAtSlot  :: !SlotNo
  , otAtEpoch :: !EpochNo
  } deriving stock (Eq, Show)

-- | Observe one block. Returns the outcome and the updated state.
--
-- Convenience wrapper around 'observeAt' that extracts the era and
-- slot from the block.
observeBlock
  :: CardanoBlock StandardCrypto
  -> ObservedSummary
  -> (ObservationResult, ObservedSummary)
observeBlock blk = observeAt (eraOf blk) (blockSlot blk)

-- | Observe an era + slot directly. Useful for testing without having
-- to construct a real 'CardanoBlock'.
--
-- Cases:
--
-- * @era == current era@ → no change.
-- * @era == current era + 1@ → era transition observed; close the
--   previous era at the epoch boundary containing the slot and open
--   the new era there.
-- * @era > current era + 1@ → set the 'osBroken' flag (we cannot
--   construct correct intermediate boundaries from a single
--   observation). The state is preserved otherwise so that the
--   partial summary is at least usable for blocks before the gap.
-- * @era < current era@ → defensive no-op (the chain doesn't move
--   backwards in eras within a single sync).
observeAt
  :: EraIdx
  -> SlotNo
  -> ObservedSummary
  -> (ObservationResult, ObservedSummary)
observeAt newEra slot os
  | osBroken os                         = (Unchanged, os)
  | newEra == osCurrentEra os           = (Unchanged, os)
  | newEra < osCurrentEra os            = (Unchanged, os)
  | fromEnum newEra > fromEnum (osCurrentEra os) + 1 =
      ( ObservationBroken (osCurrentEra os) newEra
      , os { osBroken = True }
      )
  | otherwise =
      let (transition, os') = closeCurrentEra slot os newEra
       in (NewTransition transition, os')

-- | Close the current era at the epoch boundary containing 'slot'
-- and open the new era there.
closeCurrentEra
  :: SlotNo
  -> ObservedSummary
  -> EraIdx
  -> (ObservedTransition, ObservedSummary)
closeCurrentEra slot os newEra =
  ( ObservedTransition
      { otFromEra = osCurrentEra os
      , otToEra   = newEra
      , otAtSlot  = History.boundSlot newBound
      , otAtEpoch = History.boundEpoch newBound
      }
  , os
      { osCurrentEra      = newEra
      , osCurrentEraStart = newBound
      , osPastEras        = osPastEras os ++ [closedEra]
      }
  )
  where
    prevParams :: History.EraParams
    prevParams = paramsAt (osCurrentEra os) (osParams os)

    -- Compute the epoch of the era boundary. The transition fires at
    -- the start of the epoch /containing/ the first block of the new
    -- era (with respect to the previous era's epoch alignment). We
    -- divide rather than 'slotToEpochBound' because the latter rounds
    -- /up/.
    newEpoch :: EpochNo
    newEpoch =
      let SlotNo blkSlot       = slot
          SlotNo prevStartSlot = History.boundSlot (osCurrentEraStart os)
          EpochNo prevEpoch    = History.boundEpoch (osCurrentEraStart os)
          History.EraParams{ History.eraEpochSize = EpochSize epochSizeW } = prevParams
          slotsFromPrevStart =
            if blkSlot >= prevStartSlot
              then blkSlot - prevStartSlot
              else 0
          epochsAdded =
            if epochSizeW == 0
              then 0
              else slotsFromPrevStart `div` epochSizeW
       in EpochNo (prevEpoch + epochsAdded)

    newBound :: History.Bound
    newBound = History.mkUpperBound prevParams (osCurrentEraStart os) newEpoch

    closedEra :: History.EraSummary
    closedEra = History.EraSummary
      { History.eraStart  = osCurrentEraStart os
      , History.eraEnd    = History.EraEnd newBound
      , History.eraParams = prevParams
      }

-- ---------------------------------------------------------------------------
-- * Snapshotting
-- ---------------------------------------------------------------------------

-- | The era of the most-recently-observed block.
currentEra :: ObservedSummary -> EraIdx
currentEra = osCurrentEra

-- | Whether the observation has broken (a too-large era gap was seen).
isObservationBroken :: ObservedSummary -> Bool
isObservationBroken = osBroken

-- | Snapshot the current observed summary into a 'History.Summary'.
--
-- The current era is given 'EraUnbounded' as its end (we don't predict
-- future transitions). dbsync only uses the result for slots ≤ the
-- last-observed block, so the unbounded end is harmless.
--
-- The 'Summary' type's type-list parameter is the /maximum/ number of
-- eras the chain may have; the actual list of 'EraSummary' values may
-- be shorter. We use the SOP-provided 'nonEmptyFromList' to convert a
-- plain list into the length-indexed 'Data.SOP.NonEmpty.NonEmpty'.
-- 'nonEmptyFromList' returns 'Nothing' only on the empty list, which
-- cannot happen here because the current era is always present.
currentSummary :: ObservedSummary -> History.Summary (CardanoEras StandardCrypto)
currentSummary os =
  case nonEmptyFromList (osPastEras os ++ [currentEraSum]) of
    Just ne -> History.Summary ne
    Nothing -> panic "DbSync.StateQuery.ObservedSummary.currentSummary: \
                     \unreachable empty list (current era is always present)"
  where
    currentEraSum :: History.EraSummary
    currentEraSum = History.EraSummary
      { History.eraStart  = osCurrentEraStart os
      , History.eraEnd    = History.EraUnbounded
      , History.eraParams = paramsAt (osCurrentEra os) (osParams os)
      }

-- | Snapshot the current observed summary into an 'Interpreter'.
currentInterpreter
  :: ObservedSummary
  -> History.Interpreter (CardanoEras StandardCrypto)
currentInterpreter = History.mkInterpreter . currentSummary
