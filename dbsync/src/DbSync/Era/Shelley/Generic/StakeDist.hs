{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : DbSync.Era.Shelley.Generic.StakeDist
Description : Era-agnostic stake-distribution slice types and helpers.

Two halves to this module:

  * 'StakeSlice' / 'StakeSliceRes' — the era-collapsed shape in which
    we incrementally insert stake-distribution rows across the blocks
    of an epoch.
  * 'getStakeSlice', 'countEpochStake', 'fullEpochStake',
    'getPoolDistr' — projections that slice the @ssStakeMark@ snapshot
    out of a Shelley-family 'ExtLedgerState'.

Slices are anchored on the /next/ epoch: we read the \"mark\" snapshot
whose values activate on @current epoch + 1@, so the 'sliceEpochNo'
returned by every helper is @nesEL + 1@.
-}
module DbSync.Era.Shelley.Generic.StakeDist
  ( -- * Types
    StakeSliceRes (..)
  , StakeSlice (..)

    -- * Projections
  , getSecurityParameter
  , getStakeSlice
  , countEpochStake
  , fullEpochStake
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

import DbSync.Ledger.Keys (PoolKeyHash, StakeCred)
import DbSync.Node.Connection (CardanoBlock)

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

{- |
Compute the stake slice for a single block of an epoch.

'sliceIndex' can match the @epochBlockNo@ for every block.

'minSliceSize' has to be constant or it could cause missing data. If
the value is too small it is bumped to a @defaultEpochSliceSize@ big
enough to cover all delegations. On mainnet, @minSliceSize = 2000@
holds until delegations grow past ~8.6M, at which point the size is
adjusted.
-}
getStakeSlice
  :: ConsensusProtocol (BlockProtocol blk)
  => ProtocolInfo blk
  -> Word64
  -> ExtLedgerState (CardanoBlock StandardCrypto) mk
  -> Bool
  -> StakeSliceRes
getStakeSlice pInfo !epochBlockNo els isMigration =
  case ledgerState els of
    LedgerStateByron _      -> NoSlices
    LedgerStateShelley sls  -> genericStakeSlice pInfo epochBlockNo sls isMigration
    LedgerStateAllegra als  -> genericStakeSlice pInfo epochBlockNo als isMigration
    LedgerStateMary mls     -> genericStakeSlice pInfo epochBlockNo mls isMigration
    LedgerStateAlonzo als   -> genericStakeSlice pInfo epochBlockNo als isMigration
    LedgerStateBabbage bls  -> genericStakeSlice pInfo epochBlockNo bls isMigration
    LedgerStateConway cls   -> genericStakeSlice pInfo epochBlockNo cls isMigration
    LedgerStateDijkstra dls -> genericStakeSlice pInfo epochBlockNo dls isMigration

genericStakeSlice
  :: forall era blk p mk
   . ConsensusProtocol (BlockProtocol blk)
  => ProtocolInfo blk
  -> Word64
  -> LedgerState (ShelleyBlock p era) mk
  -> Bool
  -> StakeSliceRes
genericStakeSlice pInfo epochBlockNo lstate isMigration
  | index > delegationsLen                    = NoSlices
  | index == delegationsLen                   = Slice (emptySlice epoch) True
  | index + size > delegationsLen             = Slice (mkSlice (delegationsLen - index)) True
  | otherwise                                 = Slice (mkSlice size) False
  where
    epoch :: EpochNo
    epoch = EpochNo $ 1 + unEpochNo (Shelley.nesEL (Consensus.shelleyLedgerState lstate))

    minSliceSize :: Word64
    minSliceSize = 2000

    -- On mainnet this is 2160.
    k :: Word64
    k = unNonZero $ getSecurityParameter pInfo

    -- The \"mark\" snapshot activates at current-epoch + 1. Picking it
    -- means rows land in the DB tagged for the next epoch.
    stakeSnapshot :: Ledger.SnapShot
    stakeSnapshot =
      Ledger.ssStakeMark . Shelley.esSnapshots . Shelley.nesEs $
        Consensus.shelleyLedgerState lstate

    activeStakeEntries :: VMap.KVVector VB VS (Credential Staking, StakeWithDelegation)
    activeStakeEntries = VMap.unVMap $ Ledger.unActiveStake $ Ledger.ssActiveStake stakeSnapshot

    delegationsLen :: Word64
    delegationsLen = fromIntegral $ VG.length activeStakeEntries

    -- Deterministic across the whole epoch. The last slice can be
    -- smaller; any slice after that is empty.
    epochSliceSize :: Word64
    epochSliceSize = max minSliceSize defaultEpochSliceSize
      where
        -- On mainnet this is 2160.
        expectedBlocks :: Word64
        expectedBlocks = 10 * k

        -- Sized so even at 20% block-production rate we cover everything.
        defaultEpochSliceSize :: Word64
        defaultEpochSliceSize = 1 + div (delegationsLen * 5) expectedBlocks

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

    mkSlice :: Word64 -> StakeSlice
    mkSlice actualSize =
      StakeSlice
        { sliceEpochNo = epoch
        , sliceDistr   = distribution
        }
      where
        activeStakeSliced :: VMap VB VS (Credential Staking) StakeWithDelegation
        activeStakeSliced =
          VMap $ VG.slice (fromIntegral index) (fromIntegral actualSize) activeStakeEntries

        distribution :: [(StakeCred, (Coin, PoolKeyHash))]
        distribution =
          VMap.foldlWithKey
            (\acc cred swd ->
              (cred, (Ledger.fromCompact (unNonZero (swdStake swd)), swdDelegation swd)) : acc
            )
            []
            activeStakeSliced

-- ---------------------------------------------------------------------------
-- * Counting & full-epoch projections
-- ---------------------------------------------------------------------------

-- | Total number of (non-zero) stake entries in the current \"mark\"
-- snapshot, tagged with the epoch they belong to. 'Nothing' for
-- Byron.
countEpochStake
  :: ExtLedgerState (CardanoBlock StandardCrypto) mk
  -> Maybe (Word64, EpochNo)
countEpochStake els =
  case ledgerState els of
    LedgerStateByron _      -> Nothing
    LedgerStateShelley sls  -> genericCountEpochStake sls
    LedgerStateAllegra als  -> genericCountEpochStake als
    LedgerStateMary mls     -> genericCountEpochStake mls
    LedgerStateAlonzo als   -> genericCountEpochStake als
    LedgerStateBabbage bls  -> genericCountEpochStake bls
    LedgerStateConway cls   -> genericCountEpochStake cls
    LedgerStateDijkstra dls -> genericCountEpochStake dls

genericCountEpochStake
  :: LedgerState (ShelleyBlock p era) mk
  -> Maybe (Word64, EpochNo)
genericCountEpochStake lstate = Just (delegationsLen, epoch)
  where
    epoch :: EpochNo
    epoch = EpochNo $ 1 + unEpochNo (Shelley.nesEL (Consensus.shelleyLedgerState lstate))

    stakeSnapshot :: Ledger.SnapShot
    stakeSnapshot =
      Ledger.ssStakeMark . Shelley.esSnapshots . Shelley.nesEs $
        Consensus.shelleyLedgerState lstate

    activeStake :: VMap VB VS (Credential Staking) StakeWithDelegation
    activeStake = Ledger.unActiveStake $ Ledger.ssActiveStake stakeSnapshot

    -- @ActiveStake@ only stores non-zero entries, no filtering needed.
    delegationsLen :: Word64
    delegationsLen = fromIntegral $ VMap.size activeStake

-- | Whole-epoch stake distribution as a single 'StakeSliceRes'. Used
-- by migration paths that want every row in one go rather than sliced
-- across the blocks of the epoch.
fullEpochStake
  :: ExtLedgerState (CardanoBlock StandardCrypto) mk
  -> StakeSliceRes
fullEpochStake els =
  case ledgerState els of
    LedgerStateByron _      -> NoSlices
    LedgerStateShelley sls  -> genericFullStakeSlice sls
    LedgerStateAllegra als  -> genericFullStakeSlice als
    LedgerStateMary mls     -> genericFullStakeSlice mls
    LedgerStateAlonzo als   -> genericFullStakeSlice als
    LedgerStateBabbage bls  -> genericFullStakeSlice bls
    LedgerStateConway cls   -> genericFullStakeSlice cls
    LedgerStateDijkstra dls -> genericFullStakeSlice dls

genericFullStakeSlice
  :: forall era p mk
   . LedgerState (ShelleyBlock p era) mk
  -> StakeSliceRes
genericFullStakeSlice lstate = Slice stakeSlice True
  where
    epoch :: EpochNo
    epoch = EpochNo $ 1 + unEpochNo (Shelley.nesEL (Consensus.shelleyLedgerState lstate))

    stakeSnapshot :: Ledger.SnapShot
    stakeSnapshot =
      Ledger.ssStakeMark . Shelley.esSnapshots . Shelley.nesEs $
        Consensus.shelleyLedgerState lstate

    activeStakeEntries :: VMap.KVVector VB VS (Credential Staking, StakeWithDelegation)
    activeStakeEntries = VMap.unVMap $ Ledger.unActiveStake $ Ledger.ssActiveStake stakeSnapshot

    delegationsLen :: Word64
    delegationsLen = fromIntegral $ VG.length activeStakeEntries

    stakeSlice :: StakeSlice
    stakeSlice =
      StakeSlice
        { sliceEpochNo = epoch
        , sliceDistr   = distribution
        }
      where
        activeStakeSliced :: VMap VB VS (Credential Staking) StakeWithDelegation
        activeStakeSliced =
          VMap $ VG.slice 0 (fromIntegral delegationsLen) activeStakeEntries

        distribution :: [(StakeCred, (Coin, PoolKeyHash))]
        distribution =
          VMap.foldlWithKey
            (\acc cred swd ->
              (cred, (Ledger.fromCompact (unNonZero (swdStake swd)), swdDelegation swd)) : acc
            )
            []
            activeStakeSliced

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
