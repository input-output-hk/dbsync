{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema types for the StakeDelegation extractor tables.
--
-- Two extractors share this module:
--
--   * @stake_delegation@ (block-extracted): @stake_registration@,
--     @stake_deregistration@, @delegation@, @withdrawal@.
--   * @stake_delegation_ledger@ (ledger-derived): @reward@,
--     @pot_reward@, @epoch_stake@, @epoch_stake_progress@.
--
-- Schema modules group by domain; extractor ownership picks tables
-- via 'DbSync.Extractor.pdTables'.
module DbSync.Db.Schema.StakeDelegation
  ( -- * Schema types
    StakeRegistration (..)
  , StakeDeregistration (..)
  , Delegation (..)
  , Withdrawal (..)
  , Reward (..)
  , PotReward (..)
  , EpochStake (..)
  , EpochStakeProgress (..)

    -- * Table definitions
  , stakeRegistrationTableDef
  , stakeDeregistrationTableDef
  , delegationTableDef
  , withdrawalTableDef
  , rewardTableDef
  , potRewardTableDef
  , epochStakeTableDef
  , epochStakeProgressTableDef

    -- * Column records (compile-time-safe column references)
  , StakeRegistrationCols (..), stakeRegistrationCols, stakeRegistrationColsList
  , StakeDeregistrationCols (..), stakeDeregistrationCols, stakeDeregistrationColsList
  , DelegationCols (..), delegationCols, delegationColsList
  , WithdrawalCols (..), withdrawalCols, withdrawalColsList
  , RewardCols (..), rewardCols, rewardColsList
  , PotRewardCols (..), potRewardCols, potRewardColsList
  , EpochStakeCols (..), epochStakeCols, epochStakeColsList
  , EpochStakeProgressCols (..), epochStakeProgressCols, epochStakeProgressColsList

    -- * Per-module column-record registry
  , stakeDelegationColumnRecords

    -- * COPY encoding
  , encodeStakeRegistrationCopy
  , encodeStakeDeregistrationCopy
  , encodeDelegationCopy
  , encodeWithdrawalCopy
  , encodeRewardCopy
  , encodePotRewardCopy
  , encodeEpochStakeCopy
  , encodeEpochStakeProgressCopy

    -- * Hasql encoders \/ decoders
  , stakeRegistrationEncoder
  , stakeRegistrationDecoder
  , entityStakeRegistrationDecoder
  , stakeDeregistrationEncoder
  , stakeDeregistrationDecoder
  , entityStakeDeregistrationDecoder
  , delegationEncoder
  , delegationDecoder
  , entityDelegationDecoder
  , withdrawalEncoder
  , withdrawalDecoder
  , entityWithdrawalDecoder
  , rewardEncoder
  , rewardDecoder
  , entityRewardDecoder
  , potRewardEncoder
  , potRewardDecoder
  , entityPotRewardDecoder
  , epochStakeEncoder
  , epochStakeDecoder
  , entityEpochStakeDecoder
  , epochStakeProgressEncoder
  , epochStakeProgressDecoder
  , entityEpochStakeProgressDecoder
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
import DbSync.Db.Types
  ( DbLovelace (..)
  , RewardSource
  , dbLovelaceValueDecoder
  , dbLovelaceValueEncoder
  , maybeDbLovelaceDecoder
  , maybeDbLovelaceEncoder
  , rewardSourceDecoder
  , rewardSourceEncoder
  , bRewardSource
  )

import DbSync.Db.Loader.Encoder
  ( buildCopyRow
  , bBool
  , bInt64
  , bWord64
  )

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key StakeRegistration = StakeRegistrationId
type instance Key StakeDeregistration = StakeDeregistrationId
type instance Key Delegation = DelegationId
type instance Key Withdrawal = WithdrawalId
type instance Key Reward = RewardId
type instance Key PotReward = PotRewardId
type instance Key EpochStake = EpochStakeId
type instance Key EpochStakeProgress = EpochStakeProgressId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | The @stake_registration@ table.
data StakeRegistration = StakeRegistration
  { stakeRegistrationAddrId    :: !StakeAddressId
  , stakeRegistrationCertIndex :: !Word16
  , stakeRegistrationEpochNo   :: !Word64
  , stakeRegistrationTxId      :: !TxId
  , stakeRegistrationDeposit   :: !(Maybe DbLovelace)
  }
  deriving stock (Eq, Show)

-- | The @stake_deregistration@ table.
data StakeDeregistration = StakeDeregistration
  { stakeDeregistrationAddrId     :: !StakeAddressId
  , stakeDeregistrationCertIndex  :: !Word16
  , stakeDeregistrationEpochNo    :: !Word64
  , stakeDeregistrationTxId       :: !TxId
  , stakeDeregistrationRedeemerId :: !(Maybe RedeemerId)
  }
  deriving stock (Eq, Show)

-- | The @delegation@ table.
data Delegation = Delegation
  { delegationAddrId        :: !StakeAddressId
  , delegationCertIndex     :: !Word16
  , delegationPoolHashId    :: !PoolHashId
  , delegationActiveEpochNo :: !Word64
  , delegationTxId          :: !TxId
  , delegationSlotNo        :: !Word64
  , delegationRedeemerId    :: !(Maybe RedeemerId)
  }
  deriving stock (Eq, Show)

-- | The @withdrawal@ table.
data Withdrawal = Withdrawal
  { withdrawalAddrId     :: !StakeAddressId
  , withdrawalTxId       :: !TxId
  , withdrawalAmount     :: !DbLovelace
  , withdrawalRedeemerId :: !(Maybe RedeemerId)
  }
  deriving stock (Eq, Show)

-- | The @reward@ table. Sourced from the ledger state, never from
-- block extraction. Each row is one reward earned by a stake address
-- in a specific epoch from a specific source (leader, member,
-- reserves, treasury, refund). @earned_epoch@ is computed by
-- PostgreSQL via @GENERATED ALWAYS AS (...) STORED@; the field is
-- present here for SELECT decoding but is omitted from COPY rows.
data Reward = Reward
  { rewardAddrId         :: !StakeAddressId
  , rewardType           :: !RewardSource
  , rewardAmount         :: !DbLovelace
  , rewardSpendableEpoch :: !Word64
  , rewardPoolId         :: !PoolHashId
  , rewardEarnedEpoch    :: !Word64
  }
  deriving stock (Eq, Show)

-- | The @pot_reward@ table. Holds rewards paid out from a protocol
-- pot (reserves \/ treasury): MIR distributions (Shelley→Babbage),
-- gov-action deposit refunds, and treasury withdrawal payouts
-- (Conway+). Distinct from 'Reward' which is for pool-block-production
-- rewards. Same generated @earned_epoch@ pattern as 'Reward'.
data PotReward = PotReward
  { potRewardAddrId         :: !StakeAddressId
  , potRewardType           :: !RewardSource
  , potRewardAmount         :: !DbLovelace
  , potRewardSpendableEpoch :: !Word64
  , potRewardEarnedEpoch    :: !Word64
  }
  deriving stock (Eq, Show)

-- | The @epoch_stake@ table. Active stake distribution per
-- (stake address, pool, epoch). Unique on @(addr_id, pool_id,
-- epoch_no)@ — the constraint is added during
-- @PreparingForVolatileTail@, not at @CREATE TABLE@ time.
data EpochStake = EpochStake
  { epochStakeAddrId  :: !StakeAddressId
  , epochStakePoolId  :: !PoolHashId
  , epochStakeAmount  :: !DbLovelace
  , epochStakeEpochNo :: !Word64
  }
  deriving stock (Eq, Show)

-- | The @epoch_stake_progress@ table. One row per epoch tracking
-- whether the ledger worker has finished writing the matching
-- @epoch_stake@ rows. Unique on @epoch_no@.
data EpochStakeProgress = EpochStakeProgress
  { epochStakeProgressEpochNo   :: !Word64
  , epochStakeProgressCompleted :: !Bool
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definitions
-- ---------------------------------------------------------------------------

stakeRegistrationTableDef :: TableDef
stakeRegistrationTableDef = TableDef
  { tdName    = "stake_registration"
  , tdColumns =
      [ ColumnDef "id"         PgBigInt   False
      , ColumnDef "addr_id"    PgBigInt   False
      , ColumnDef "cert_index" PgBigInt   False
      , ColumnDef "epoch_no"   PgBigInt   False
      , ColumnDef "tx_id"      PgBigInt   False
      , ColumnDef "deposit"    PgNumeric  True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys =
      [ ForeignKey "tx_id" "tx" "id"
      ]
  }

stakeDeregistrationTableDef :: TableDef
stakeDeregistrationTableDef = TableDef
  { tdName    = "stake_deregistration"
  , tdColumns =
      [ ColumnDef "id"          PgBigInt  False
      , ColumnDef "addr_id"     PgBigInt  False
      , ColumnDef "cert_index"  PgBigInt  False
      , ColumnDef "epoch_no"    PgBigInt  False
      , ColumnDef "tx_id"       PgBigInt  False
      , ColumnDef "redeemer_id" PgBigInt  True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys =
      [ ForeignKey "tx_id" "tx" "id"
      ]
  }

delegationTableDef :: TableDef
delegationTableDef = TableDef
  { tdName    = "delegation"
  , tdColumns =
      [ ColumnDef "id"              PgBigInt  False
      , ColumnDef "addr_id"         PgBigInt  False
      , ColumnDef "cert_index"      PgBigInt  False
      , ColumnDef "pool_hash_id"    PgBigInt  False
      , ColumnDef "active_epoch_no" PgBigInt  False
      , ColumnDef "tx_id"           PgBigInt  False
      , ColumnDef "slot_no"         PgBigInt  False
      , ColumnDef "redeemer_id"     PgBigInt  True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys =
      [ ForeignKey "tx_id" "tx" "id"
      ]
  }

withdrawalTableDef :: TableDef
withdrawalTableDef = TableDef
  { tdName    = "withdrawal"
  , tdColumns =
      [ ColumnDef "id"          PgBigInt   False
      , ColumnDef "addr_id"     PgBigInt   False
      , ColumnDef "tx_id"       PgBigInt   False
      , ColumnDef "amount"      PgNumeric  False
      , ColumnDef "redeemer_id" PgBigInt   True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys =
      [ ForeignKey "tx_id" "tx" "id"
      ]
  }

-- | @reward.earned_epoch@ derives from @spendable_epoch@ and the
-- reward @type@: refund rewards are earned in the same epoch they
-- become spendable; everything else is earned two epochs earlier
-- (with a @0@ floor for the genesis-era window).
--
-- IDENTITY leaf: nothing FKs into @reward@, so PostgreSQL allocates
-- the id from the backing sequence at INSERT time.
rewardTableDef :: TableDef
rewardTableDef = TableDef
  { tdName    = "reward"
  , tdColumns =
      [ ColumnDef "id"              PgBigInt  False
      , ColumnDef "addr_id"         PgBigInt  False
      , ColumnDef "type"            PgText    False
      , ColumnDef "amount"          PgNumeric False
      , ColumnDef "spendable_epoch" PgBigInt  False
      , ColumnDef "pool_id"         PgBigInt  False
      , ColumnDef "earned_epoch"    PgBigInt  False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
    -- Natural key: one reward per (addr, type, earned epoch, pool).
  , tdUniqueConstraints = ["addr_id" :| ["type", "earned_epoch", "pool_id"]]
  , tdGeneratedColumns =
      [ ( "earned_epoch"
        , "(CASE WHEN (type='refund') then spendable_epoch \
          \else (CASE WHEN spendable_epoch >= 2 \
          \then spendable_epoch-2 else 0 end) end)"
        )
      ]
  , tdIdentityColumns = ["id"]
  , tdForeignKeys = []
  }

-- | @pot_reward.earned_epoch@ is one epoch behind
-- @spendable_epoch@ (with a @0@ floor for epoch 0).
--
-- IDENTITY leaf: same reasoning as 'rewardTableDef'.
potRewardTableDef :: TableDef
potRewardTableDef = TableDef
  { tdName    = "pot_reward"
  , tdColumns =
      [ ColumnDef "id"              PgBigInt  False
      , ColumnDef "addr_id"         PgBigInt  False
      , ColumnDef "type"            PgText    False
      , ColumnDef "amount"          PgNumeric False
      , ColumnDef "spendable_epoch" PgBigInt  False
      , ColumnDef "earned_epoch"    PgBigInt  False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns =
      [ ( "earned_epoch"
        , "(CASE WHEN spendable_epoch >= 1 \
          \then spendable_epoch-1 else 0 end)"
        )
      ]
  , tdIdentityColumns = ["id"]
  , tdForeignKeys = []
  }

-- | The @epoch_stake@ table. The triple (addr_id, pool_id,
-- epoch_no) is unique; the constraint is added during
-- @PreparingForVolatileTail@, not at @CREATE TABLE@ time.
--
-- IDENTITY leaf: nothing FKs into @epoch_stake@.
epochStakeTableDef :: TableDef
epochStakeTableDef = TableDef
  { tdName    = "epoch_stake"
  , tdColumns =
      [ ColumnDef "id"       PgBigInt  False
      , ColumnDef "addr_id"  PgBigInt  False
      , ColumnDef "pool_id"  PgBigInt  False
      , ColumnDef "amount"   PgNumeric False
      , ColumnDef "epoch_no" PgBigInt  False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = ["addr_id" :| ["pool_id", "epoch_no"]]
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys = []
  }

-- | The @epoch_stake_progress@ table. Unique on @epoch_no@.
--
-- IDENTITY leaf: nothing FKs into @epoch_stake_progress@.
epochStakeProgressTableDef :: TableDef
epochStakeProgressTableDef = TableDef
  { tdName    = "epoch_stake_progress"
  , tdColumns =
      [ ColumnDef "id"        PgBigInt  False
      , ColumnDef "epoch_no"  PgBigInt  False
      , ColumnDef "completed" PgBoolean False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = [pure "epoch_no"]
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys = []
  }

-- ---------------------------------------------------------------------------
-- * Column records
-- ---------------------------------------------------------------------------

data StakeRegistrationCols = StakeRegistrationCols
  { srcId        :: !TableColumn
  , srcAddrId    :: !TableColumn
  , srcCertIndex :: !TableColumn
  , srcEpochNo   :: !TableColumn
  , srcTxId      :: !TableColumn
  , srcDeposit   :: !TableColumn
  }

stakeRegistrationCols :: StakeRegistrationCols
stakeRegistrationCols =
  let c = TableColumn stakeRegistrationTableDef
  in StakeRegistrationCols
       { srcId        = c "id"
       , srcAddrId    = c "addr_id"
       , srcCertIndex = c "cert_index"
       , srcEpochNo   = c "epoch_no"
       , srcTxId      = c "tx_id"
       , srcDeposit   = c "deposit"
       }

stakeRegistrationColsList :: [TableColumn]
stakeRegistrationColsList =
  [ stakeRegistrationCols.srcId
  , stakeRegistrationCols.srcAddrId
  , stakeRegistrationCols.srcCertIndex
  , stakeRegistrationCols.srcEpochNo
  , stakeRegistrationCols.srcTxId
  , stakeRegistrationCols.srcDeposit
  ]

data StakeDeregistrationCols = StakeDeregistrationCols
  { sdcId         :: !TableColumn
  , sdcAddrId     :: !TableColumn
  , sdcCertIndex  :: !TableColumn
  , sdcEpochNo    :: !TableColumn
  , sdcTxId       :: !TableColumn
  , sdcRedeemerId :: !TableColumn
  }

stakeDeregistrationCols :: StakeDeregistrationCols
stakeDeregistrationCols =
  let c = TableColumn stakeDeregistrationTableDef
  in StakeDeregistrationCols
       { sdcId         = c "id"
       , sdcAddrId     = c "addr_id"
       , sdcCertIndex  = c "cert_index"
       , sdcEpochNo    = c "epoch_no"
       , sdcTxId       = c "tx_id"
       , sdcRedeemerId = c "redeemer_id"
       }

stakeDeregistrationColsList :: [TableColumn]
stakeDeregistrationColsList =
  [ stakeDeregistrationCols.sdcId
  , stakeDeregistrationCols.sdcAddrId
  , stakeDeregistrationCols.sdcCertIndex
  , stakeDeregistrationCols.sdcEpochNo
  , stakeDeregistrationCols.sdcTxId
  , stakeDeregistrationCols.sdcRedeemerId
  ]

data DelegationCols = DelegationCols
  { dcId             :: !TableColumn
  , dcAddrId         :: !TableColumn
  , dcCertIndex      :: !TableColumn
  , dcPoolHashId     :: !TableColumn
  , dcActiveEpochNo  :: !TableColumn
  , dcTxId           :: !TableColumn
  , dcSlotNo         :: !TableColumn
  , dcRedeemerId     :: !TableColumn
  }

delegationCols :: DelegationCols
delegationCols =
  let c = TableColumn delegationTableDef
  in DelegationCols
       { dcId             = c "id"
       , dcAddrId         = c "addr_id"
       , dcCertIndex      = c "cert_index"
       , dcPoolHashId     = c "pool_hash_id"
       , dcActiveEpochNo  = c "active_epoch_no"
       , dcTxId           = c "tx_id"
       , dcSlotNo         = c "slot_no"
       , dcRedeemerId     = c "redeemer_id"
       }

delegationColsList :: [TableColumn]
delegationColsList =
  [ delegationCols.dcId
  , delegationCols.dcAddrId
  , delegationCols.dcCertIndex
  , delegationCols.dcPoolHashId
  , delegationCols.dcActiveEpochNo
  , delegationCols.dcTxId
  , delegationCols.dcSlotNo
  , delegationCols.dcRedeemerId
  ]

data WithdrawalCols = WithdrawalCols
  { wcId         :: !TableColumn
  , wcAddrId     :: !TableColumn
  , wcTxId       :: !TableColumn
  , wcAmount     :: !TableColumn
  , wcRedeemerId :: !TableColumn
  }

withdrawalCols :: WithdrawalCols
withdrawalCols =
  let c = TableColumn withdrawalTableDef
  in WithdrawalCols
       { wcId         = c "id"
       , wcAddrId     = c "addr_id"
       , wcTxId       = c "tx_id"
       , wcAmount     = c "amount"
       , wcRedeemerId = c "redeemer_id"
       }

withdrawalColsList :: [TableColumn]
withdrawalColsList =
  [ withdrawalCols.wcId
  , withdrawalCols.wcAddrId
  , withdrawalCols.wcTxId
  , withdrawalCols.wcAmount
  , withdrawalCols.wcRedeemerId
  ]

data RewardCols = RewardCols
  { rcId              :: !TableColumn
  , rcAddrId          :: !TableColumn
  , rcType            :: !TableColumn
  , rcAmount          :: !TableColumn
  , rcSpendableEpoch  :: !TableColumn
  , rcPoolId          :: !TableColumn
  , rcEarnedEpoch     :: !TableColumn
  }

rewardCols :: RewardCols
rewardCols =
  let c = TableColumn rewardTableDef
  in RewardCols
       { rcId              = c "id"
       , rcAddrId          = c "addr_id"
       , rcType            = c "type"
       , rcAmount          = c "amount"
       , rcSpendableEpoch  = c "spendable_epoch"
       , rcPoolId          = c "pool_id"
       , rcEarnedEpoch     = c "earned_epoch"
       }

rewardColsList :: [TableColumn]
rewardColsList =
  [ rewardCols.rcId
  , rewardCols.rcAddrId
  , rewardCols.rcType
  , rewardCols.rcAmount
  , rewardCols.rcSpendableEpoch
  , rewardCols.rcPoolId
  , rewardCols.rcEarnedEpoch
  ]

data PotRewardCols = PotRewardCols
  { prcId              :: !TableColumn
  , prcAddrId          :: !TableColumn
  , prcType            :: !TableColumn
  , prcAmount          :: !TableColumn
  , prcSpendableEpoch  :: !TableColumn
  , prcEarnedEpoch     :: !TableColumn
  }

potRewardCols :: PotRewardCols
potRewardCols =
  let c = TableColumn potRewardTableDef
  in PotRewardCols
       { prcId              = c "id"
       , prcAddrId          = c "addr_id"
       , prcType            = c "type"
       , prcAmount          = c "amount"
       , prcSpendableEpoch  = c "spendable_epoch"
       , prcEarnedEpoch     = c "earned_epoch"
       }

potRewardColsList :: [TableColumn]
potRewardColsList =
  [ potRewardCols.prcId
  , potRewardCols.prcAddrId
  , potRewardCols.prcType
  , potRewardCols.prcAmount
  , potRewardCols.prcSpendableEpoch
  , potRewardCols.prcEarnedEpoch
  ]

data EpochStakeCols = EpochStakeCols
  { escId      :: !TableColumn
  , escAddrId  :: !TableColumn
  , escPoolId  :: !TableColumn
  , escAmount  :: !TableColumn
  , escEpochNo :: !TableColumn
  }

epochStakeCols :: EpochStakeCols
epochStakeCols =
  let c = TableColumn epochStakeTableDef
  in EpochStakeCols
       { escId      = c "id"
       , escAddrId  = c "addr_id"
       , escPoolId  = c "pool_id"
       , escAmount  = c "amount"
       , escEpochNo = c "epoch_no"
       }

epochStakeColsList :: [TableColumn]
epochStakeColsList =
  [ epochStakeCols.escId
  , epochStakeCols.escAddrId
  , epochStakeCols.escPoolId
  , epochStakeCols.escAmount
  , epochStakeCols.escEpochNo
  ]

data EpochStakeProgressCols = EpochStakeProgressCols
  { espcId        :: !TableColumn
  , espcEpochNo   :: !TableColumn
  , espcCompleted :: !TableColumn
  }

epochStakeProgressCols :: EpochStakeProgressCols
epochStakeProgressCols =
  let c = TableColumn epochStakeProgressTableDef
  in EpochStakeProgressCols
       { espcId        = c "id"
       , espcEpochNo   = c "epoch_no"
       , espcCompleted = c "completed"
       }

epochStakeProgressColsList :: [TableColumn]
epochStakeProgressColsList =
  [ epochStakeProgressCols.espcId
  , epochStakeProgressCols.espcEpochNo
  , epochStakeProgressCols.espcCompleted
  ]

-- ---------------------------------------------------------------------------
-- * Per-module column-record registry
-- ---------------------------------------------------------------------------

stakeDelegationColumnRecords :: [(TableDef, [TableColumn])]
stakeDelegationColumnRecords =
  [ (stakeRegistrationTableDef,   stakeRegistrationColsList)
  , (stakeDeregistrationTableDef, stakeDeregistrationColsList)
  , (delegationTableDef,          delegationColsList)
  , (withdrawalTableDef,          withdrawalColsList)
  , (rewardTableDef,              rewardColsList)
  , (potRewardTableDef,           potRewardColsList)
  , (epochStakeTableDef,          epochStakeColsList)
  , (epochStakeProgressTableDef,  epochStakeProgressColsList)
  ]

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodeStakeRegistrationCopy :: StakeRegistration -> ByteString
encodeStakeRegistrationCopy sr =
  buildCopyRow
    [ Just $ bInt64 (getStakeAddressId $ stakeRegistrationAddrId sr)
    , Just $ bInt64 (fromIntegral $ stakeRegistrationCertIndex sr)
    , Just $ bWord64 (stakeRegistrationEpochNo sr)
    , Just $ bInt64 (getTxId $ stakeRegistrationTxId sr)
    , bWord64 . unDbLovelace <$> stakeRegistrationDeposit sr
    ]

encodeStakeDeregistrationCopy :: StakeDeregistration -> ByteString
encodeStakeDeregistrationCopy sd =
  buildCopyRow
    [ Just $ bInt64 (getStakeAddressId $ stakeDeregistrationAddrId sd)
    , Just $ bInt64 (fromIntegral $ stakeDeregistrationCertIndex sd)
    , Just $ bWord64 (stakeDeregistrationEpochNo sd)
    , Just $ bInt64 (getTxId $ stakeDeregistrationTxId sd)
    , bInt64 . getRedeemerId <$> stakeDeregistrationRedeemerId sd
    ]

encodeDelegationCopy :: Delegation -> ByteString
encodeDelegationCopy d =
  buildCopyRow
    [ Just $ bInt64 (getStakeAddressId $ delegationAddrId d)
    , Just $ bInt64 (fromIntegral $ delegationCertIndex d)
    , Just $ bInt64 (getPoolHashId $ delegationPoolHashId d)
    , Just $ bWord64 (delegationActiveEpochNo d)
    , Just $ bInt64 (getTxId $ delegationTxId d)
    , Just $ bWord64 (delegationSlotNo d)
    , bInt64 . getRedeemerId <$> delegationRedeemerId d
    ]

encodeWithdrawalCopy :: Withdrawal -> ByteString
encodeWithdrawalCopy w =
  buildCopyRow
    [ Just $ bInt64 (getStakeAddressId $ withdrawalAddrId w)
    , Just $ bInt64 (getTxId $ withdrawalTxId w)
    , Just $ bWord64 (unDbLovelace $ withdrawalAmount w)
    , bInt64 . getRedeemerId <$> withdrawalRedeemerId w
    ]

-- | COPY row for @reward@. The id is allocated by PostgreSQL from
-- the IDENTITY sequence, and @earned_epoch@ is computed via the
-- @GENERATED ALWAYS AS (...) STORED@ expression; the COPY column
-- list (built by 'DbSync.Db.Loader.Connection.buildColumnList')
-- excludes both.
encodeRewardCopy :: Reward -> ByteString
encodeRewardCopy r =
  buildCopyRow
    [ Just $ bInt64 (getStakeAddressId $ rewardAddrId r)
    , Just $ bRewardSource (rewardType r)
    , Just $ bWord64 (unDbLovelace $ rewardAmount r)
    , Just $ bWord64 (rewardSpendableEpoch r)
    , Just $ bInt64 (getPoolHashId $ rewardPoolId r)
    ]

-- | COPY row for @pot_reward@. Same generated-column + identity
-- treatment as 'encodeRewardCopy'.
encodePotRewardCopy :: PotReward -> ByteString
encodePotRewardCopy pr =
  buildCopyRow
    [ Just $ bInt64 (getStakeAddressId $ potRewardAddrId pr)
    , Just $ bRewardSource (potRewardType pr)
    , Just $ bWord64 (unDbLovelace $ potRewardAmount pr)
    , Just $ bWord64 (potRewardSpendableEpoch pr)
    ]

encodeEpochStakeCopy :: EpochStake -> ByteString
encodeEpochStakeCopy es =
  buildCopyRow
    [ Just $ bInt64 (getStakeAddressId $ epochStakeAddrId es)
    , Just $ bInt64 (getPoolHashId $ epochStakePoolId es)
    , Just $ bWord64 (unDbLovelace $ epochStakeAmount es)
    , Just $ bWord64 (epochStakeEpochNo es)
    ]

encodeEpochStakeProgressCopy :: EpochStakeProgress -> ByteString
encodeEpochStakeProgressCopy esp =
  buildCopyRow
    [ Just $ bWord64 (epochStakeProgressEpochNo esp)
    , Just $ bBool (epochStakeProgressCompleted esp)
    ]

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------

-- StakeRegistration --------------------------------------------------------

stakeRegistrationEncoder :: E.Params StakeRegistration
stakeRegistrationEncoder = mconcat
  [ stakeRegistrationAddrId    >$< idEncoder getStakeAddressId
  , (fromIntegral :: Word16 -> Int64) . stakeRegistrationCertIndex
                               >$< E.param (E.nonNullable E.int8)
  , stakeRegistrationEpochNo   >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , stakeRegistrationTxId      >$< idEncoder getTxId
  , stakeRegistrationDeposit   >$< maybeDbLovelaceEncoder
  ]

stakeRegistrationDecoder :: D.Row StakeRegistration
stakeRegistrationDecoder = StakeRegistration
  <$> idDecoder StakeAddressId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> idDecoder TxId
  <*> maybeDbLovelaceDecoder

entityStakeRegistrationDecoder :: D.Row (StakeRegistrationId, StakeRegistration)
entityStakeRegistrationDecoder = (,)
  <$> idDecoder StakeRegistrationId
  <*> stakeRegistrationDecoder

-- StakeDeregistration ------------------------------------------------------

stakeDeregistrationEncoder :: E.Params StakeDeregistration
stakeDeregistrationEncoder = mconcat
  [ stakeDeregistrationAddrId     >$< idEncoder getStakeAddressId
  , (fromIntegral :: Word16 -> Int64) . stakeDeregistrationCertIndex
                                  >$< E.param (E.nonNullable E.int8)
  , stakeDeregistrationEpochNo    >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , stakeDeregistrationTxId       >$< idEncoder getTxId
  , stakeDeregistrationRedeemerId >$< maybeIdEncoder getRedeemerId
  ]

stakeDeregistrationDecoder :: D.Row StakeDeregistration
stakeDeregistrationDecoder = StakeDeregistration
  <$> idDecoder StakeAddressId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> idDecoder TxId
  <*> maybeIdDecoder RedeemerId

entityStakeDeregistrationDecoder
  :: D.Row (StakeDeregistrationId, StakeDeregistration)
entityStakeDeregistrationDecoder = (,)
  <$> idDecoder StakeDeregistrationId
  <*> stakeDeregistrationDecoder

-- Delegation ---------------------------------------------------------------

delegationEncoder :: E.Params Delegation
delegationEncoder = mconcat
  [ delegationAddrId        >$< idEncoder getStakeAddressId
  , (fromIntegral :: Word16 -> Int64) . delegationCertIndex
                            >$< E.param (E.nonNullable E.int8)
  , delegationPoolHashId    >$< idEncoder getPoolHashId
  , delegationActiveEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , delegationTxId          >$< idEncoder getTxId
  , delegationSlotNo        >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , delegationRedeemerId    >$< maybeIdEncoder getRedeemerId
  ]

delegationDecoder :: D.Row Delegation
delegationDecoder = Delegation
  <$> idDecoder StakeAddressId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> idDecoder PoolHashId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> idDecoder TxId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> maybeIdDecoder RedeemerId

entityDelegationDecoder :: D.Row (DelegationId, Delegation)
entityDelegationDecoder = (,)
  <$> idDecoder DelegationId
  <*> delegationDecoder

-- Withdrawal ---------------------------------------------------------------

withdrawalEncoder :: E.Params Withdrawal
withdrawalEncoder = mconcat
  [ withdrawalAddrId     >$< idEncoder getStakeAddressId
  , withdrawalTxId       >$< idEncoder getTxId
  , withdrawalAmount     >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , withdrawalRedeemerId >$< maybeIdEncoder getRedeemerId
  ]

withdrawalDecoder :: D.Row Withdrawal
withdrawalDecoder = Withdrawal
  <$> idDecoder StakeAddressId
  <*> idDecoder TxId
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> maybeIdDecoder RedeemerId

entityWithdrawalDecoder :: D.Row (WithdrawalId, Withdrawal)
entityWithdrawalDecoder = (,)
  <$> idDecoder WithdrawalId
  <*> withdrawalDecoder

-- Reward -------------------------------------------------------------------
--
-- @earned_epoch@ is GENERATED, so it appears in the decoder (we read
-- it back when SELECTing) but not in the encoder (PostgreSQL computes
-- it on insert). The INSERT statement that uses 'rewardEncoder' must
-- omit @earned_epoch@ from its column list, just as the COPY path
-- does via 'tdGeneratedColumns'.

rewardEncoder :: E.Params Reward
rewardEncoder = mconcat
  [ rewardAddrId         >$< idEncoder getStakeAddressId
  , rewardType           >$< E.param (E.nonNullable rewardSourceEncoder)
  , rewardAmount         >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , rewardSpendableEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , rewardPoolId         >$< idEncoder getPoolHashId
  ]

rewardDecoder :: D.Row Reward
rewardDecoder = Reward
  <$> idDecoder StakeAddressId
  <*> D.column (D.nonNullable rewardSourceDecoder)
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> idDecoder PoolHashId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))

entityRewardDecoder :: D.Row (RewardId, Reward)
entityRewardDecoder = (,)
  <$> idDecoder RewardId
  <*> rewardDecoder

-- PotReward ----------------------------------------------------------------

potRewardEncoder :: E.Params PotReward
potRewardEncoder = mconcat
  [ potRewardAddrId         >$< idEncoder getStakeAddressId
  , potRewardType           >$< E.param (E.nonNullable rewardSourceEncoder)
  , potRewardAmount         >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , potRewardSpendableEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  ]

potRewardDecoder :: D.Row PotReward
potRewardDecoder = PotReward
  <$> idDecoder StakeAddressId
  <*> D.column (D.nonNullable rewardSourceDecoder)
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))

entityPotRewardDecoder :: D.Row (PotRewardId, PotReward)
entityPotRewardDecoder = (,)
  <$> idDecoder PotRewardId
  <*> potRewardDecoder

-- EpochStake ---------------------------------------------------------------

epochStakeEncoder :: E.Params EpochStake
epochStakeEncoder = mconcat
  [ epochStakeAddrId  >$< idEncoder getStakeAddressId
  , epochStakePoolId  >$< idEncoder getPoolHashId
  , epochStakeAmount  >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , epochStakeEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  ]

epochStakeDecoder :: D.Row EpochStake
epochStakeDecoder = EpochStake
  <$> idDecoder StakeAddressId
  <*> idDecoder PoolHashId
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))

entityEpochStakeDecoder :: D.Row (EpochStakeId, EpochStake)
entityEpochStakeDecoder = (,)
  <$> idDecoder EpochStakeId
  <*> epochStakeDecoder

-- EpochStakeProgress -------------------------------------------------------

epochStakeProgressEncoder :: E.Params EpochStakeProgress
epochStakeProgressEncoder = mconcat
  [ epochStakeProgressEpochNo   >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , epochStakeProgressCompleted >$< E.param (E.nonNullable E.bool)
  ]

epochStakeProgressDecoder :: D.Row EpochStakeProgress
epochStakeProgressDecoder = EpochStakeProgress
  <$> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> D.column (D.nonNullable D.bool)

entityEpochStakeProgressDecoder :: D.Row (EpochStakeProgressId, EpochStakeProgress)
entityEpochStakeProgressDecoder = (,)
  <$> idDecoder EpochStakeProgressId
  <*> epochStakeProgressDecoder
