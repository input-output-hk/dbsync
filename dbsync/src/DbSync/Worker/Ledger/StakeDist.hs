{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Era-agnostic stake-distribution slice types and helpers.
--
--   * 'StakeSlice' / 'StakeSliceRes' — the era-collapsed shape for
--     incrementally inserting stake-distribution rows across the blocks
--     of an epoch.
--   * 'getStakeSlice', 'catchupStakeSlice', 'getPoolDistr' —
--     projections that slice the @ssStakeMark@ snapshot out of a
--     Shelley-family 'ExtLedgerState'.
--
-- Slices are anchored on the /next/ epoch: the \"mark\" snapshot's
-- values activate on @current epoch + 1@, so 'sliceEpochNo' is
-- @nesEL + 1@.
module DbSync.Worker.Ledger.StakeDist
  ( -- * Types
    StakeSliceRes (..)
  , StakeSlice (..)
  , StakeSliceMode (..)

    -- * Projections
  , getSecurityParameter
  , getStakeSlice
  , catchupStakeSlice
  , getPoolDistr
  ) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes.NonZero (NonZero, unNonZero)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Compactible as Ledger
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (..))
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.State (StakeWithDelegation (..))
import qualified Cardano.Ledger.State as Ledger
import Cardano.Ledger.Val ((<+>))
import Cardano.Slotting.Slot (EpochNo (..))
import qualified Data.Map.Strict as Map
import Data.VMap (VB, VMap (..), VS)
import qualified Data.VMap as VMap
import qualified Data.Vector.Generic as VG
import Lens.Micro ((^.))
import Ouroboros.Consensus.Block (BlockProtocol)
import Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardCrypto)
import Ouroboros.Consensus.Config (configSecurityParam)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo, pInfoConfig)
import Ouroboros.Consensus.Protocol.Abstract (ConsensusProtocol, maxRollbacks)
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

import DbSync.Worker.Ledger.Keys (PoolKeyHash, StakeCred)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Per-block result of the stake-distribution extraction.
--
--   * 'Slice' — an actual slice of entries. The 'Bool' is 'True' for
--     the last slice of this epoch and is used only for logging.
--   * 'NoSlices' — Byron blocks, or blocks where the index is past the
--     end of the delegation vector.
data StakeSliceRes
  = Slice !StakeSlice !Bool
  | NoSlices

-- | One slice of the stake distribution — a list of
-- @(credential, (amount, pool))@ triples tagged with the epoch whose
-- stake it describes.
data StakeSlice = StakeSlice
  { sliceEpochNo :: !EpochNo
  , sliceDistr   :: ![(StakeCred, (Coin, PoolKeyHash))]
  }
  deriving stock (Eq)

emptySlice :: EpochNo -> StakeSlice
emptySlice epoch = StakeSlice epoch []

instance NFData StakeSliceRes where
  rnf (Slice slice lastFlag) = rnf (slice, lastFlag)
  rnf NoSlices               = ()

instance NFData StakeSlice where
  rnf (StakeSlice epoch distr) = rnf (epoch, distr)

-- | Whether the slice computation is running for steady-state
-- ingestion or for an era-migration backfill (the first Shelley
-- block following the Byron tail).
data StakeSliceMode
  = SteadyStateSlice
  | EraMigrationSlice
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Security parameter
-- ---------------------------------------------------------------------------

-- | Extract the chain's security parameter @k@ from a 'ProtocolInfo'.
-- Used to decide the starting index into the \"mark\" snapshot when
-- slicing — we only start emitting stake slices once we're past block
-- @k@ of the epoch.
getSecurityParameter
  :: ConsensusProtocol (BlockProtocol blk)
  => ProtocolInfo blk
  -> NonZero Word64
getSecurityParameter = maxRollbacks . configSecurityParam . pInfoConfig

-- ---------------------------------------------------------------------------
-- * Slicing across an epoch
-- ---------------------------------------------------------------------------

-- | Compute the stake slice for a single block of an epoch.
--
-- 'sliceIndex' can match the @epochBlockNo@ for every block.
--
-- 'minSliceSize' has to be constant or it could cause missing data; if
-- too small it is bumped to a @defaultEpochSliceSize@ big enough to
-- cover all delegations.
getStakeSlice
  :: ConsensusProtocol (BlockProtocol blk)
  => ProtocolInfo blk
  -> Word64
  -> ExtLedgerState (CardanoBlock StandardCrypto) mk
  -> StakeSliceMode
  -> StakeSliceRes
getStakeSlice pInfo !epochBlockNo els mode =
  case ledgerState els of
    LedgerStateByron _      -> NoSlices
    LedgerStateShelley sls  -> genericStakeSlice pInfo epochBlockNo sls mode
    LedgerStateAllegra als  -> genericStakeSlice pInfo epochBlockNo als mode
    LedgerStateMary mls     -> genericStakeSlice pInfo epochBlockNo mls mode
    LedgerStateAlonzo als   -> genericStakeSlice pInfo epochBlockNo als mode
    LedgerStateBabbage bls  -> genericStakeSlice pInfo epochBlockNo bls mode
    LedgerStateConway cls   -> genericStakeSlice pInfo epochBlockNo cls mode
    LedgerStateDijkstra dls -> genericStakeSlice pInfo epochBlockNo dls mode

genericStakeSlice
  :: forall era blk p mk
   . ConsensusProtocol (BlockProtocol blk)
  => ProtocolInfo blk
  -> Word64
  -> LedgerState (ShelleyBlock p era) mk
  -> StakeSliceMode
  -> StakeSliceRes
genericStakeSlice pInfo epochBlockNo lstate mode
  | index > delegationsLen                    = NoSlices
  | index == delegationsLen                   = Slice (emptySlice (scEpoch ctx)) True
  | index + size > delegationsLen             = Slice (ctxSlice ctx index (delegationsLen - index)) True
  | otherwise                                 = Slice (ctxSlice ctx index size) False
  where
    ctx :: SliceCtx
    ctx = sliceCtx pInfo lstate

    isMigration :: Bool
    isMigration = mode == EraMigrationSlice

    k :: Word64
    k = scK ctx

    delegationsLen :: Word64
    delegationsLen = scDelegationsLen ctx

    epochSliceSize :: Word64
    epochSliceSize = scSliceSize ctx

    -- Starting index into the delegation vector.
    index :: Word64
    index
      | isMigration            = 0
      | epochBlockNo < k       = delegationsLen + 1  -- forces the empty slice
      | otherwise              = (epochBlockNo - k) * epochSliceSize

    size :: Word64
    size
      | isMigration, epochBlockNo + 1 < k = 0
      | isMigration                       = (epochBlockNo + 1 - k) * epochSliceSize
      | otherwise                         = epochSliceSize

-- ---------------------------------------------------------------------------
-- * Shared slice context
-- ---------------------------------------------------------------------------

-- | Quantities every slice computation over a \"mark\" snapshot needs.
-- Both the per-block slicer and the boundary catch-up derive their
-- indices from the same context, which is what guarantees the two
-- never overlap and never leave a gap between them.
data SliceCtx = SliceCtx
  { scEpoch          :: !EpochNo
  , scK              :: !Word64
  , scEntries        :: !(VMap.KVVector VB VS (Credential Staking, StakeWithDelegation))
  , scDelegationsLen :: !Word64
  , scSliceSize      :: !Word64
  }

sliceCtx
  :: ConsensusProtocol (BlockProtocol blk)
  => ProtocolInfo blk
  -> LedgerState (ShelleyBlock p era) mk
  -> SliceCtx
sliceCtx pInfo lstate =
  SliceCtx
    { scEpoch          = epoch
    , scK              = k
    , scEntries        = activeStakeEntries
    , scDelegationsLen = delegationsLen
    , scSliceSize      = max minSliceSize defaultEpochSliceSize
    }
  where
    -- The "mark" snapshot activates at current-epoch + 1. Picking it
    -- means rows land in the DB tagged for the next epoch.
    epoch :: EpochNo
    epoch = EpochNo $ 1 + unEpochNo (Shelley.nesEL (Consensus.shelleyLedgerState lstate))

    -- On mainnet this is 2160.
    k :: Word64
    k = unNonZero $ getSecurityParameter pInfo

    stakeSnapshot :: Ledger.SnapShot
    stakeSnapshot =
      Ledger.ssStakeMark . Shelley.esSnapshots . Shelley.nesEs $
        Consensus.shelleyLedgerState lstate

    activeStakeEntries :: VMap.KVVector VB VS (Credential Staking, StakeWithDelegation)
    activeStakeEntries = VMap.unVMap $ Ledger.unActiveStake $ Ledger.ssActiveStake stakeSnapshot

    delegationsLen :: Word64
    delegationsLen = fromIntegral $ VG.length activeStakeEntries

    minSliceSize :: Word64
    minSliceSize = 2000

    -- Deterministic across the whole epoch. The last slice can be
    -- smaller; any slice after that is empty. Sized so even at 20%
    -- block-production rate we cover everything.
    defaultEpochSliceSize :: Word64
    defaultEpochSliceSize = 1 + div (delegationsLen * 5) (10 * k)

-- | Slice @[start, start + len)@ of the context's delegation vector.
ctxSlice :: SliceCtx -> Word64 -> Word64 -> StakeSlice
ctxSlice ctx start len =
  StakeSlice
    { sliceEpochNo = scEpoch ctx
    , sliceDistr   = distribution
    }
  where
    activeStakeSliced :: VMap VB VS (Credential Staking) StakeWithDelegation
    activeStakeSliced =
      VMap $ VG.slice (fromIntegral start) (fromIntegral len) (scEntries ctx)

    distribution :: [(StakeCred, (Coin, PoolKeyHash))]
    distribution =
      VMap.foldlWithKey
        (\acc cred swd ->
          (cred, (Ledger.fromCompact (unNonZero (swdStake swd)), swdDelegation swd)) : acc
        )
        []
        activeStakeSliced

-- ---------------------------------------------------------------------------
-- * Boundary catch-up
-- ---------------------------------------------------------------------------

-- | Suffix of the just-ended epoch's stake distribution that per-block
-- slicing never emitted.
--
-- Per-block slices only start at epoch-block @k@, so an epoch with
-- fewer than @k + 1@ blocks emits nothing at all, and a short epoch
-- can end before its slices reach the end of the delegation vector.
-- Called at the epoch boundary with the /pre-boundary/ ledger state
-- (whose \"mark\" snapshot fed the ended epoch's slices) and that
-- epoch's final epoch-block counter; returns the un-emitted tail as a
-- final slice, or 'NoSlices' when the per-block path already covered
-- the whole vector.
catchupStakeSlice
  :: ConsensusProtocol (BlockProtocol blk)
  => ProtocolInfo blk
  -> Word64
  -- ^ Epoch-block counter of the ended epoch's last block.
  -> ExtLedgerState (CardanoBlock StandardCrypto) mk
  -> StakeSliceRes
catchupStakeSlice pInfo !finalEpochBlockNo els =
  case ledgerState els of
    LedgerStateByron _      -> NoSlices
    LedgerStateShelley sls  -> genericCatchupSlice pInfo finalEpochBlockNo sls
    LedgerStateAllegra als  -> genericCatchupSlice pInfo finalEpochBlockNo als
    LedgerStateMary mls     -> genericCatchupSlice pInfo finalEpochBlockNo mls
    LedgerStateAlonzo als   -> genericCatchupSlice pInfo finalEpochBlockNo als
    LedgerStateBabbage bls  -> genericCatchupSlice pInfo finalEpochBlockNo bls
    LedgerStateConway cls   -> genericCatchupSlice pInfo finalEpochBlockNo cls
    LedgerStateDijkstra dls -> genericCatchupSlice pInfo finalEpochBlockNo dls

genericCatchupSlice
  :: ConsensusProtocol (BlockProtocol blk)
  => ProtocolInfo blk
  -> Word64
  -> LedgerState (ShelleyBlock p era) mk
  -> StakeSliceRes
genericCatchupSlice pInfo finalEpochBlockNo lstate
  | emittedEnd >= delegationsLen = NoSlices
  | otherwise =
      Slice (ctxSlice ctx emittedEnd (delegationsLen - emittedEnd)) True
  where
    ctx :: SliceCtx
    ctx = sliceCtx pInfo lstate

    delegationsLen :: Word64
    delegationsLen = scDelegationsLen ctx

    -- End (exclusive) of the prefix the per-block slices covered: the
    -- block at epoch-block @b >= k@ emits @[(b - k) * size, (b - k)
    -- * size + size)@, so after the last block the prefix reaches
    -- @(final - k + 1) * size@. Nothing was emitted if the epoch
    -- never reached block @k@.
    emittedEnd :: Word64
    emittedEnd
      | finalEpochBlockNo < scK ctx = 0
      | otherwise = (finalEpochBlockNo - scK ctx + 1) * scSliceSize ctx

-- ---------------------------------------------------------------------------
-- * Pool distribution
-- ---------------------------------------------------------------------------

-- | Pool-side aggregate of the \"mark\" snapshot: stake totals per
-- pool plus the blocks-made counter from the /previous/ epoch (used
-- for block-production metrics).
getPoolDistr
  :: ExtLedgerState (CardanoBlock StandardCrypto) mk
  -> Maybe (Map PoolKeyHash (Coin, Word64), Map PoolKeyHash Natural)
getPoolDistr els =
  case ledgerState els of
    LedgerStateByron _      -> Nothing
    LedgerStateShelley sls  -> Just $ genericPoolDistr sls
    LedgerStateAllegra als  -> Just $ genericPoolDistr als
    LedgerStateMary mls     -> Just $ genericPoolDistr mls
    LedgerStateAlonzo als   -> Just $ genericPoolDistr als
    LedgerStateBabbage bls  -> Just $ genericPoolDistr bls
    LedgerStateConway cls   -> Just $ genericPoolDistr cls
    LedgerStateDijkstra dls -> Just $ genericPoolDistr dls

genericPoolDistr
  :: forall era p mk
   . LedgerState (ShelleyBlock p era) mk
  -> (Map PoolKeyHash (Coin, Word64), Map PoolKeyHash Natural)
genericPoolDistr lstate = (stakePerPool, blocksPerPool)
  where
    nes :: Shelley.NewEpochState era
    nes = Consensus.shelleyLedgerState lstate

    stakeMark :: Ledger.SnapShot
    stakeMark = Ledger.ssStakeMark $ Shelley.esSnapshots $ Shelley.nesEs nes

    stakePerPool :: Map PoolKeyHash (Coin, Word64)
    stakePerPool = countStakePerPool (Ledger.ssActiveStake stakeMark)

    blocksPerPool :: Map PoolKeyHash Natural
    blocksPerPool = nes ^. Shelley.nesBprevL

countStakePerPool
  :: Ledger.ActiveStake
  -> Map PoolKeyHash (Coin, Word64)
countStakePerPool (Ledger.ActiveStake activeStake) =
  VMap.foldlWithKey accum Map.empty activeStake
  where
    accum !acc _cred swd =
      Map.insertWith
        addDel
        (swdDelegation swd)
        (Ledger.fromCompact (unNonZero (swdStake swd)), 1)
        acc

    addDel (c, n) (c', n') = (c <+> c', n + n')
