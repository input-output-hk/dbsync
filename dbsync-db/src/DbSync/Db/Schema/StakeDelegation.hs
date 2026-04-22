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

import qualified Data.Text.Encoding as TE

import DbSync.Db.Schema.Core (encodeHex, encodeInt64, encodeWord64)
import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Db.Writer.Copy.Encoder (encodeToCopyRow)

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
  encodeToCopyRow
    [ Just $ encodeInt64 sid
    , Just $ encodeHex (stakeAddressHashRaw sa)
    , Just $ TE.encodeUtf8 (stakeAddressView sa)
    , encodeHex <$> stakeAddressScriptHash sa
    ]

encodeStakeRegistrationCopy :: StakeRegistrationId -> StakeRegistration -> ByteString
encodeStakeRegistrationCopy (StakeRegistrationId sid) sr =
  encodeToCopyRow
    [ Just $ encodeInt64 sid
    , Just $ encodeInt64 (getStakeAddressId $ stakeRegistrationAddrId sr)
    , Just $ encodeInt64 (fromIntegral $ stakeRegistrationCertIndex sr)
    , Just $ encodeWord64 (stakeRegistrationEpochNo sr)
    , Just $ encodeInt64 (getTxId $ stakeRegistrationTxId sr)
    , encodeWord64 . unDbLovelace <$> stakeRegistrationDeposit sr
    ]

encodeStakeDeregistrationCopy :: StakeDeregistrationId -> StakeDeregistration -> ByteString
encodeStakeDeregistrationCopy (StakeDeregistrationId sid) sd =
  encodeToCopyRow
    [ Just $ encodeInt64 sid
    , Just $ encodeInt64 (getStakeAddressId $ stakeDeregistrationAddrId sd)
    , Just $ encodeInt64 (fromIntegral $ stakeDeregistrationCertIndex sd)
    , Just $ encodeWord64 (stakeDeregistrationEpochNo sd)
    , Just $ encodeInt64 (getTxId $ stakeDeregistrationTxId sd)
    , encodeInt64 . getRedeemerId <$> stakeDeregistrationRedeemerId sd
    ]

encodeDelegationCopy :: DelegationId -> Delegation -> ByteString
encodeDelegationCopy (DelegationId did) d =
  encodeToCopyRow
    [ Just $ encodeInt64 did
    , Just $ encodeInt64 (getStakeAddressId $ delegationAddrId d)
    , Just $ encodeInt64 (fromIntegral $ delegationCertIndex d)
    , Just $ encodeInt64 (getPoolHashId $ delegationPoolHashId d)
    , Just $ encodeWord64 (delegationActiveEpochNo d)
    , Just $ encodeInt64 (getTxId $ delegationTxId d)
    , Just $ encodeWord64 (delegationSlotNo d)
    , encodeInt64 . getRedeemerId <$> delegationRedeemerId d
    ]

encodeWithdrawalCopy :: WithdrawalId -> Withdrawal -> ByteString
encodeWithdrawalCopy (WithdrawalId wid) w =
  encodeToCopyRow
    [ Just $ encodeInt64 wid
    , Just $ encodeInt64 (getStakeAddressId $ withdrawalAddrId w)
    , Just $ encodeInt64 (getTxId $ withdrawalTxId w)
    , Just $ encodeWord64 (unDbLovelace $ withdrawalAmount w)
    , encodeInt64 . getRedeemerId <$> withdrawalRedeemerId w
    ]
