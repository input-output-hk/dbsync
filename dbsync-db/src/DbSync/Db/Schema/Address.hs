{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema for the @address@ dedup table.
--
-- Addresses are normalised: each unique raw payment-address byte-string
-- gets a single row, and @tx_out.address_id@ references it. This
-- replaces the original inline columns @tx_out.address@,
-- @tx_out.address_has_script@, @tx_out.payment_cred@, eliminating the
-- duplication that those columns produced across millions of outputs.
--
-- Owned by the @utxo@ extractor — every @tx_out@ depends on an
-- @address@ row, so the two tables must be populated by the same
-- extractor.
module DbSync.Db.Schema.Address
  ( -- * Schema types
    Address (..)

    -- * Table definitions
  , addressTableDef

    -- * COPY encoding
  , encodeAddressCopy

    -- * Hasql encoders \/ decoders
  , addressEncoder
  , addressDecoder
  , entityAddressDecoder
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
import DbSync.Db.Loader.Encoder
  ( buildCopyRow
  , bBool
  , bHex
  , bInt64
  , bText
  )

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key Address = AddressId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | The @address@ table. Unique on @raw@.
data Address = Address
  { addressAddress        :: !Text                 -- ^ Bech32 / Byron base58 form
  , addressRaw            :: !ByteString           -- ^ Raw address bytes (the dedup key)
  , addressHasScript      :: !Bool                 -- ^ Bit 4 of the header byte
  , addressPaymentCred    :: !(Maybe ByteString)   -- ^ First 28 bytes after the header
  , addressStakeAddressId :: !(Maybe StakeAddressId) -- ^ FK to stake_address (NULL during ingest)
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definitions
-- ---------------------------------------------------------------------------

addressTableDef :: TableDef
addressTableDef = TableDef
  { tdName    = "address"
  , tdColumns =
      [ ColumnDef "id"               PgBigInt False
      , ColumnDef "address"          PgText   False
      , ColumnDef "raw"              PgBytea  False
      , ColumnDef "has_script"       PgBoolean False
      , ColumnDef "payment_cred"     PgBytea  True
      , ColumnDef "stake_address_id" PgBigInt True
      ]
  , tdMode    = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = [pure "raw"]
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodeAddressCopy :: AddressId -> Address -> ByteString
encodeAddressCopy (AddressId aid) a =
  buildCopyRow
    [ Just $ bInt64 aid
    , Just $ bText (addressAddress a)
    , Just $ bHex (addressRaw a)
    , Just $ bBool (addressHasScript a)
    , bHex <$> addressPaymentCred a
    , bInt64 . getStakeAddressId <$> addressStakeAddressId a
    ]

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------

addressEncoder :: E.Params Address
addressEncoder = mconcat
  [ addressAddress        >$< E.param (E.nonNullable E.text)
  , addressRaw            >$< E.param (E.nonNullable E.bytea)
  , addressHasScript      >$< E.param (E.nonNullable E.bool)
  , addressPaymentCred    >$< E.param (E.nullable E.bytea)
  , addressStakeAddressId >$< maybeIdEncoder getStakeAddressId
  ]

addressDecoder :: D.Row Address
addressDecoder = Address
  <$> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable D.bytea)
  <*> D.column (D.nonNullable D.bool)
  <*> D.column (D.nullable D.bytea)
  <*> maybeIdDecoder StakeAddressId

entityAddressDecoder :: D.Row (AddressId, Address)
entityAddressDecoder = (,)
  <$> idDecoder AddressId
  <*> addressDecoder
