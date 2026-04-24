{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema types for the StakeDelegation extractor tables: stake_address,
-- stake_registration, stake_deregistration, delegation, withdrawal.
module DbSync.Db.Schema.StakeDelegation
  ( -- * Schema types
    StakeAddress (..)
  , StakeRegistration (..)
  , StakeDeregistration (..)
  , Delegation (..)
  , Withdrawal (..)

    -- * Table definitions
  , stakeAddressTableDef
  , stakeRegistrationTableDef
  , stakeDeregistrationTableDef
  , delegationTableDef
  , withdrawalTableDef

    -- * COPY encoding
  , encodeStakeAddressCopy
  , encodeStakeRegistrationCopy
  , encodeStakeDeregistrationCopy
  , encodeDelegationCopy
  , encodeWithdrawalCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
import DbSync.Db.Types (DbLovelace (..))

import DbSync.Db.Writer.Copy.Encoder (buildCopyRow, bHex, bInt64, bText, bWord64)

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key StakeAddress = StakeAddressId
type instance Key StakeRegistration = StakeRegistrationId
type instance Key StakeDeregistration = StakeDeregistrationId
type instance Key Delegation = DelegationId
type instance Key Withdrawal = WithdrawalId

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
