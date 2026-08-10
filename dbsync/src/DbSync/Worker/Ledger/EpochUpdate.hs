-- | Era-agnostic per-epoch update and new-epoch summary.
--
-- 'NewEpoch' arrives once per epoch boundary. 'EpochUpdate' is the
-- parameters-only subset that every era produces.
module DbSync.Worker.Ledger.EpochUpdate
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

import DbSync.Worker.Ledger.ProtoParams (ProtoParams, epochProtoParams)
import DbSync.Worker.Ledger.Keys (PoolKeyHash)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock)
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
-- * NFData instances
-- ---------------------------------------------------------------------------

instance NFData EpochUpdate where
  rnf (EpochUpdate a b) = rnf (a, b)

instance NFData NewEpoch where
  rnf (NewEpoch a b c d e f g) = rnf ((a, b, c, d), (e, f, g))

-- ---------------------------------------------------------------------------
-- * Projections
-- ---------------------------------------------------------------------------

epochUpdate :: ExtLedgerState (CardanoBlock StandardCrypto) mk -> EpochUpdate
epochUpdate lstate =
  EpochUpdate
    { euProtoParams = maybeToStrictMaybe $ epochProtoParams lstate
    , euNonce       = extractEpochNonce lstate
    }

-- | Per-epoch VRF nonce from the header state. Each era routes through
-- either TPraos or Praos.
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
