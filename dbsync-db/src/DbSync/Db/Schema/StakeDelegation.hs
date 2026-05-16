{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema types for the StakeDelegation extractor tables.
--
-- Two extractors share this module:
--
--   * @stake_delegation@ (block-extracted): @stake_address@,
--     @stake_registration@, @stake_deregistration@, @delegation@,
--     @withdrawal@.
--   * @stake_delegation_ledger@ (ledger-derived): @reward@,
--     @reward_rest@, @epoch_stake@, @epoch_stake_progress@.
--
-- Schema modules group by domain; extractor ownership picks tables
-- via 'DbSync.Extractor.pdTables'.
module DbSync.Db.Schema.StakeDelegation
  ( -- * Schema types
    StakeAddress (..)
  , StakeRegistration (..)
  , StakeDeregistration (..)
  , Delegation (..)
  , Withdrawal (..)
  , Reward (..)
  , RewardRest (..)
  , EpochStake (..)
  , EpochStakeProgress (..)

    -- * Table definitions
  , stakeAddressTableDef
  , stakeRegistrationTableDef
  , stakeDeregistrationTableDef
  , delegationTableDef
  , withdrawalTableDef
  , rewardTableDef
  , rewardRestTableDef
  , epochStakeTableDef
  , epochStakeProgressTableDef

    -- * COPY encoding
  , encodeStakeAddressCopy
  , encodeStakeRegistrationCopy
  , encodeStakeDeregistrationCopy
  , encodeDelegationCopy
  , encodeWithdrawalCopy
  , encodeRewardCopy
  , encodeRewardRestCopy
  , encodeEpochStakeCopy
  , encodeEpochStakeProgressCopy

    -- * Hasql encoders \/ decoders
  , stakeAddressEncoder
  , stakeAddressDecoder
  , entityStakeAddressDecoder
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
  , rewardRestEncoder
  , rewardRestDecoder
  , entityRewardRestDecoder
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

import DbSync.Db.Writer.Copy.Encoder
  ( buildCopyRow
  , bBool
  , bHex
  , bInt64
  , bText
  , bWord64
  )

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key StakeAddress = StakeAddressId
type instance Key StakeRegistration = StakeRegistrationId
type instance Key StakeDeregistration = StakeDeregistrationId
type instance Key Delegation = DelegationId
type instance Key Withdrawal = WithdrawalId
type instance Key Reward = RewardId
type instance Key RewardRest = RewardRestId
type instance Key EpochStake = EpochStakeId
type instance Key EpochStakeProgress = EpochStakeProgressId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | The @stake_address@ table (dedup table).
-- One row per unique stake credential hash.
data StakeAddress = StakeAddress
  { stakeAddressHashRaw    :: !ByteString        -- ^ 28-byte stake credential hash
  , stakeAddressView       :: !Text              -- ^ Bech32 representation
  , stakeAddressScriptHash :: !(Maybe ByteString) -- ^ Script hash if script-based
  }
  deriving stock (Eq, Show)

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

-- | The @reward_rest@ table. Holds reserves\/treasury\/refund
-- rewards that aren't tied to a delegation pool. Same generated
-- @earned_epoch@ pattern as 'Reward'.
data RewardRest = RewardRest
  { rewardRestAddrId         :: !StakeAddressId
  , rewardRestType           :: !RewardSource
  , rewardRestAmount         :: !DbLovelace
  , rewardRestSpendableEpoch :: !Word64
  , rewardRestEarnedEpoch    :: !Word64
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

stakeAddressTableDef :: TableDef
stakeAddressTableDef = TableDef
  { tdName    = "stake_address"
  , tdColumns =
      [ ColumnDef "id"          PgBigInt  False
      , ColumnDef "hash_raw"    PgBytea   False
      , ColumnDef "view"        PgText    False
      , ColumnDef "script_hash" PgBytea   True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

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
  , tdForeignKeys =
      [ ForeignKey "tx_id" "tx" "id"
      ]
  }

-- | @reward.earned_epoch@ derives from @spendable_epoch@ and the
-- reward @type@: refund rewards are earned in the same epoch they
-- become spendable; everything else is earned two epochs earlier
-- (with a @0@ floor for the genesis-era window).
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
  , tdUniqueConstraints = []
  , tdGeneratedColumns =
      [ ( "earned_epoch"
        , "(CASE WHEN (type='refund') then spendable_epoch \
          \else (CASE WHEN spendable_epoch >= 2 \
          \then spendable_epoch-2 else 0 end) end)"
        )
      ]
  , tdForeignKeys = []
  }

-- | @reward_rest.earned_epoch@ is one epoch behind
-- @spendable_epoch@ (with a @0@ floor for epoch 0).
rewardRestTableDef :: TableDef
rewardRestTableDef = TableDef
  { tdName    = "reward_rest"
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
  , tdForeignKeys = []
  }

-- | The @epoch_stake@ table. The triple (addr_id, pool_id,
-- epoch_no) is unique; the constraint is added during
-- @PreparingForVolatileTail@, not at @CREATE TABLE@ time.
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
  , tdForeignKeys = []
  }

-- | The @epoch_stake_progress@ table. Unique on @epoch_no@.
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
  , tdForeignKeys = []
  }

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodeStakeAddressCopy :: StakeAddressId -> StakeAddress -> ByteString
encodeStakeAddressCopy (StakeAddressId sid) sa =
  buildCopyRow
    [ Just $ bInt64 sid
    , Just $ bHex (stakeAddressHashRaw sa)
    , Just $ bText (stakeAddressView sa)
    , bHex <$> stakeAddressScriptHash sa
    ]

encodeStakeRegistrationCopy :: StakeRegistrationId -> StakeRegistration -> ByteString
encodeStakeRegistrationCopy (StakeRegistrationId sid) sr =
  buildCopyRow
    [ Just $ bInt64 sid
    , Just $ bInt64 (getStakeAddressId $ stakeRegistrationAddrId sr)
    , Just $ bInt64 (fromIntegral $ stakeRegistrationCertIndex sr)
    , Just $ bWord64 (stakeRegistrationEpochNo sr)
    , Just $ bInt64 (getTxId $ stakeRegistrationTxId sr)
    , bWord64 . unDbLovelace <$> stakeRegistrationDeposit sr
    ]

encodeStakeDeregistrationCopy :: StakeDeregistrationId -> StakeDeregistration -> ByteString
encodeStakeDeregistrationCopy (StakeDeregistrationId sid) sd =
  buildCopyRow
    [ Just $ bInt64 sid
    , Just $ bInt64 (getStakeAddressId $ stakeDeregistrationAddrId sd)
    , Just $ bInt64 (fromIntegral $ stakeDeregistrationCertIndex sd)
    , Just $ bWord64 (stakeDeregistrationEpochNo sd)
    , Just $ bInt64 (getTxId $ stakeDeregistrationTxId sd)
    , bInt64 . getRedeemerId <$> stakeDeregistrationRedeemerId sd
    ]

encodeDelegationCopy :: DelegationId -> Delegation -> ByteString
encodeDelegationCopy (DelegationId did) d =
  buildCopyRow
    [ Just $ bInt64 did
    , Just $ bInt64 (getStakeAddressId $ delegationAddrId d)
    , Just $ bInt64 (fromIntegral $ delegationCertIndex d)
    , Just $ bInt64 (getPoolHashId $ delegationPoolHashId d)
    , Just $ bWord64 (delegationActiveEpochNo d)
    , Just $ bInt64 (getTxId $ delegationTxId d)
    , Just $ bWord64 (delegationSlotNo d)
    , bInt64 . getRedeemerId <$> delegationRedeemerId d
    ]

encodeWithdrawalCopy :: WithdrawalId -> Withdrawal -> ByteString
encodeWithdrawalCopy (WithdrawalId wid) w =
  buildCopyRow
    [ Just $ bInt64 wid
    , Just $ bInt64 (getStakeAddressId $ withdrawalAddrId w)
    , Just $ bInt64 (getTxId $ withdrawalTxId w)
    , Just $ bWord64 (unDbLovelace $ withdrawalAmount w)
    , bInt64 . getRedeemerId <$> withdrawalRedeemerId w
    ]

-- | COPY row for @reward@. @earned_epoch@ is omitted because
-- PostgreSQL computes it via the @GENERATED ALWAYS AS (...) STORED@
-- expression on the column; the COPY column list (built by
-- 'DbSync.Copy.Connection.buildColumnList') excludes it.
encodeRewardCopy :: RewardId -> Reward -> ByteString
encodeRewardCopy (RewardId rid) r =
  buildCopyRow
    [ Just $ bInt64 rid
    , Just $ bInt64 (getStakeAddressId $ rewardAddrId r)
    , Just $ bRewardSource (rewardType r)
    , Just $ bWord64 (unDbLovelace $ rewardAmount r)
    , Just $ bWord64 (rewardSpendableEpoch r)
    , Just $ bInt64 (getPoolHashId $ rewardPoolId r)
    ]

-- | COPY row for @reward_rest@. Same generated-column treatment as
-- 'encodeRewardCopy'.
encodeRewardRestCopy :: RewardRestId -> RewardRest -> ByteString
encodeRewardRestCopy (RewardRestId rid) rr =
  buildCopyRow
    [ Just $ bInt64 rid
    , Just $ bInt64 (getStakeAddressId $ rewardRestAddrId rr)
    , Just $ bRewardSource (rewardRestType rr)
    , Just $ bWord64 (unDbLovelace $ rewardRestAmount rr)
    , Just $ bWord64 (rewardRestSpendableEpoch rr)
    ]

encodeEpochStakeCopy :: EpochStakeId -> EpochStake -> ByteString
encodeEpochStakeCopy (EpochStakeId eid) es =
  buildCopyRow
    [ Just $ bInt64 eid
    , Just $ bInt64 (getStakeAddressId $ epochStakeAddrId es)
    , Just $ bInt64 (getPoolHashId $ epochStakePoolId es)
    , Just $ bWord64 (unDbLovelace $ epochStakeAmount es)
    , Just $ bWord64 (epochStakeEpochNo es)
    ]

encodeEpochStakeProgressCopy
  :: EpochStakeProgressId -> EpochStakeProgress -> ByteString
encodeEpochStakeProgressCopy (EpochStakeProgressId eid) esp =
  buildCopyRow
    [ Just $ bInt64 eid
    , Just $ bWord64 (epochStakeProgressEpochNo esp)
    , Just $ bBool (epochStakeProgressCompleted esp)
    ]

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------

-- StakeAddress -------------------------------------------------------------

stakeAddressEncoder :: E.Params StakeAddress
stakeAddressEncoder = mconcat
  [ stakeAddressHashRaw    >$< E.param (E.nonNullable E.bytea)
  , stakeAddressView       >$< E.param (E.nonNullable E.text)
  , stakeAddressScriptHash >$< E.param (E.nullable E.bytea)
  ]

stakeAddressDecoder :: D.Row StakeAddress
stakeAddressDecoder = StakeAddress
  <$> D.column (D.nonNullable D.bytea)
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nullable D.bytea)

entityStakeAddressDecoder :: D.Row (StakeAddressId, StakeAddress)
entityStakeAddressDecoder = (,)
  <$> idDecoder StakeAddressId
  <*> stakeAddressDecoder

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

-- RewardRest ---------------------------------------------------------------

rewardRestEncoder :: E.Params RewardRest
rewardRestEncoder = mconcat
  [ rewardRestAddrId         >$< idEncoder getStakeAddressId
  , rewardRestType           >$< E.param (E.nonNullable rewardSourceEncoder)
  , rewardRestAmount         >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , rewardRestSpendableEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  ]

rewardRestDecoder :: D.Row RewardRest
rewardRestDecoder = RewardRest
  <$> idDecoder StakeAddressId
  <*> D.column (D.nonNullable rewardSourceDecoder)
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))

entityRewardRestDecoder :: D.Row (RewardRestId, RewardRest)
entityRewardRestDecoder = (,)
  <$> idDecoder RewardRestId
  <*> rewardRestDecoder

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
