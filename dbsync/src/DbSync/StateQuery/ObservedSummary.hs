{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

-- | Locally-observed Cardano hard-fork summary.
--
-- Builds a 'History.Summary' from the era constructor of each block
-- ChainSync delivers. This answers slot queries while the node's
-- LedgerDB still replays and @GetInterpreter@ would fail with
-- @AcquireFailurePointTooOld@.
module DbSync.StateQuery.ObservedSummary
  ( -- * Types
    ObservedSummary
  , EraIdx (..)
  , renderEraIdx
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

-- | Which Cardano era a block belongs to. The order matches
-- 'CardanoEras' StandardCrypto', and 'observeAt' does arithmetic on
-- 'fromEnum', so entries must stay in chain order.
data EraIdx
  = ByronIdx
  | ShelleyIdx
  | AllegraIdx
  | MaryIdx
  | AlonzoIdx
  | BabbageIdx
  | ConwayIdx
  | DijkstraIdx
  deriving stock (Eq, Ord, Enum, Bounded, Show)

-- | Human era name, without the @Idx@ suffix that derived 'Show' adds.
renderEraIdx :: EraIdx -> Text
renderEraIdx = \case
  ByronIdx    -> "Byron"
  ShelleyIdx  -> "Shelley"
  AllegraIdx  -> "Allegra"
  MaryIdx     -> "Mary"
  AlonzoIdx   -> "Alonzo"
  BabbageIdx  -> "Babbage"
  ConwayIdx   -> "Conway"
  DijkstraIdx -> "Dijkstra"

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

-- | The eight per-era 'EraParams', extracted once at startup from the
-- consensus-derived 'History.Shape'. There is no per-network table
-- here; the genesis configs are the point of truth.
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

-- | The 'CardanoEras StandardCrypto' shape has exactly eight entries,
-- so spelling out the whole 'ExactlyCons' chain proves this total.
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

-- | A list of /closed/ past eras plus the /current/ era's start
-- bound. Each observed transition closes the previous era and opens
-- the next one.
data ObservedSummary = ObservedSummary
  { osCurrentEra      :: !EraIdx
    -- ^ Era of the most-recently-observed block
  , osCurrentEraStart :: !History.Bound
    -- ^ Start bound of the current era
  , osPastEras        :: ![History.EraSummary]
    -- ^ Closed past eras, oldest first
  , osParams          :: !CardanoEraParams
    -- ^ Per-era params, extracted from the consensus 'Shape' at startup
  , osBroken          :: !Bool
    -- ^ True after an era jump greater than one era. The summary is
    --   then incomplete and callers must use the node's interpreter.
  } deriving stock (Show)

-- | Initial state: only Byron is known, with no past eras. Takes the
-- whole 'TopLevelConfig' so 'EraParams' come from consensus rather
-- than a local redefinition.
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

observeBlock
  :: CardanoBlock StandardCrypto
  -> ObservedSummary
  -> (ObservationResult, ObservedSummary)
observeBlock blk = observeAt (eraOf blk) (blockSlot blk)

-- | Observe an era + slot directly, so tests need not build a real
-- 'CardanoBlock'.
--
-- * @era == current@ → no change.
-- * @era == current + 1@ → close the previous era at the epoch
--   boundary containing the slot and open the new era there.
-- * @era > current + 1@ → set 'osBroken'. A single observation
--   cannot place the skipped boundaries. The rest of the state
--   stays usable for blocks before the gap.
-- * @era < current@ → defensive no-op; eras never move backwards
--   within one sync.
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

    -- The transition fires at the start of the epoch /containing/ the
    -- first block of the new era, in the previous era's epoch
    -- alignment. Divide rather than call 'slotToEpochBound', which
    -- rounds /up/.
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

currentEra :: ObservedSummary -> EraIdx
currentEra = osCurrentEra

-- | True once an era gap larger than one era was seen, which happens
-- when dbsync resumes from a non-Byron tip without observing the
-- preceding transitions. Callers must then use the node's
-- interpreter.
isObservationBroken :: ObservedSummary -> Bool
isObservationBroken = osBroken

-- | Snapshot the observed state into a 'History.Summary'.
--
-- The current era ends 'EraUnbounded' because future transitions are
-- unknown. Callers only ask about slots at or below the last observed
-- block, so the unbounded end is harmless.
--
-- The 'Summary' type parameter is the /maximum/ era count, so the
-- actual 'EraSummary' list may be shorter. 'nonEmptyFromList' returns
-- 'Nothing' only for an empty list, which cannot happen: the current
-- era is always present.
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

currentInterpreter
  :: ObservedSummary
  -> History.Interpreter (CardanoEras StandardCrypto)
currentInterpreter = History.mkInterpreter . currentSummary
