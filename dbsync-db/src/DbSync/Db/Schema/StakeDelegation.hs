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
  , dbLovelaceValueDecoder
  , dbLovelaceValueEncoder
  , maybeDbLovelaceDecoder
  , maybeDbLovelaceEncoder
  )

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
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
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
