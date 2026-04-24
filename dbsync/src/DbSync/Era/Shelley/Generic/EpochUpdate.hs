{- |
Module      : DbSync.Era.Shelley.Generic.EpochUpdate
Description : Era-agnostic per-epoch update and new-epoch summary.

'NewEpoch' is the summary we emit once per epoch boundary during
'IngestChainHistory' — it carries the ada-pots snapshot, the
protocol-parameter update, and (from Conway on) the DRep / gov-state
snapshot. 'EpochUpdate' is the parameters-only subset that every era
produces.

The two main entry points are 'epochUpdate', which reads from the
current 'ExtLedgerState', and 'extractEpochNonce', which digs into
the header state for the per-epoch VRF nonce.
-}
module DbSync.Era.Shelley.Generic.EpochUpdate
  ( NewEpoch (..)
  , EpochUpdate (..)
  , epochUpdate
  ) where

import Cardano.Prelude

import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Shelley.API.Wallet as Shelley
import qualified Cardano.Protocol.TPraos.API as Shelley
import qualified Cardano.Protocol.TPraos.Rules.Tickn as Shelley
import Cardano.Slotting.Slot (EpochNo (..))
import qualified Data.Strict.Maybe as Strict
import Ouroboros.Consensus.Cardano.Block
  ( ConwayEra
  , HardForkState (..)
  , StandardCrypto
  )
import Ouroboros.Consensus.Cardano.CanHardFork ()
import qualified Ouroboros.Consensus.HeaderValidation as Consensus
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Protocol.Praos as Consensus
import qualified Ouroboros.Consensus.Protocol.TPraos as Consensus

import DbSync.Era.Shelley.Generic.ProtoParams (ProtoParams, epochProtoParams)
import DbSync.Ledger.Keys (PoolKeyHash)
import DbSync.Node.Connection (CardanoBlock)
import DbSync.Util (maybeToStrictMaybe)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Summary of an epoch boundary crossing.
--
-- Only Conway-and-later eras populate 'neDRepState' and 'neEnacted';
-- earlier eras set them to 'Strict.Nothing'.
data NewEpoch = NewEpoch
  { neEpoch       :: !EpochNo
  , neIsEBB       :: !Bool
  , neAdaPots     :: !(Strict.Maybe Shelley.AdaPots)
  , neEpochUpdate :: !EpochUpdate
  , neDRepState   :: !(Strict.Maybe (DRepPulsingState ConwayEra))
  , neEnacted     :: !(Strict.Maybe (ConwayGovState ConwayEra))
  , nePoolDistr   :: !(Strict.Maybe (Map PoolKeyHash (Coin, Word64), Map PoolKeyHash Natural))
  }

-- | Protocol-params-and-nonce slice of an epoch crossing.
data EpochUpdate = EpochUpdate
  { euProtoParams :: !(Strict.Maybe ProtoParams)
  , euNonce       :: !Ledger.Nonce
  }

-- ---------------------------------------------------------------------------
-- * Projections
-- ---------------------------------------------------------------------------

-- | Pull the current 'EpochUpdate' out of the ledger state.
epochUpdate :: ExtLedgerState (CardanoBlock StandardCrypto) mk -> EpochUpdate
epochUpdate lstate =
  EpochUpdate
    { euProtoParams = maybeToStrictMaybe $ epochProtoParams lstate
    , euNonce       = extractEpochNonce lstate
    }

-- | Extract the per-epoch VRF nonce from the header state, routing
-- through the right protocol (TPraos vs Praos) for each era.
extractEpochNonce :: ExtLedgerState (CardanoBlock StandardCrypto) mk -> Ledger.Nonce
extractEpochNonce extLedgerState =
  case Consensus.headerStateChainDep (headerState extLedgerState) of
    ChainDepStateByron _     -> Ledger.NeutralNonce
    ChainDepStateShelley st  -> extractNonce st
    ChainDepStateAllegra st  -> extractNonce st
    ChainDepStateMary st     -> extractNonce st
    ChainDepStateAlonzo st   -> extractNonce st
    ChainDepStateBabbage st  -> extractNoncePraos st
    ChainDepStateConway st   -> extractNoncePraos st
    ChainDepStateDijkstra st -> extractNoncePraos st
  where
    extractNonce :: Consensus.TPraosState -> Ledger.Nonce
    extractNonce =
      Shelley.ticknStateEpochNonce . Shelley.csTickn . Consensus.tpraosStateChainDepState

    extractNoncePraos :: Consensus.PraosState -> Ledger.Nonce
    extractNoncePraos = praosStateEpochNonce
