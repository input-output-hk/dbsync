{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema for the @address@ dedup table.
--
-- Addresses are normalised: each unique raw payment-address byte-string
-- gets a single row that @tx_out.address_id@ references, avoiding
-- duplication across outputs.
--
-- Owned by the @utxo@ extractor — every @tx_out@ depends on an
-- @address@ row, so the two tables must be populated by the same
-- extractor.
module DbSync.Db.Schema.Address
  ( -- * Schema types
    Address (..)

    -- * Building an address from raw bytes
  , addressFromRaw
  , rawToDisplayText
  , rawHasScript
  , extractPaymentCred

    -- * Table definitions
  , addressTableDef

    -- * Column records (compile-time-safe column references)
  , AddressCols (..)
  , addressCols
  , addressColsList

    -- * Per-module column-record registry
  , addressColumnRecords

    -- * COPY encoding
  , encodeAddressCopy

    -- * Hasql encoders \/ decoders
  , addressEncoder
  , addressDecoder
  , entityAddressDecoder
  ) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import Data.ByteString.Base58 (bitcoinAlphabet, encodeBase58)
import Data.Functor.Contravariant ((>$<))
import qualified Data.Text.Encoding as TextEnc
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
import DbSync.Util.Bech32 (serialiseShelleyAddrToBech32)

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
-- * Building an address from raw bytes
-- ---------------------------------------------------------------------------

-- | Reconstruct an 'Address' row from its raw bytes and resolved stake
-- id. The display text, has-script flag, and payment credential are all
-- pure functions of the raw bytes, so the ingest path buffers only the
-- @stake_address_id@ and rebuilds the row when it is written.
addressFromRaw :: ByteString -> Maybe StakeAddressId -> Address
addressFromRaw raw mStakeId = Address
  { addressAddress        = rawToDisplayText raw
  , addressRaw            = raw
  , addressHasScript      = rawHasScript raw
  , addressPaymentCred    = extractPaymentCred raw
  , addressStakeAddressId = mStakeId
  }

-- | Render an address from its raw bytes. Shelley+ payment addresses
-- (header high bit clear) Bech32-encode with the HRP taken from the
-- header; Byron bootstrap addresses (CBOR, high bit set) Base58-encode.
-- For Byron, @encodeBase58 bitcoinAlphabet raw@ equals
-- @Cardano.Chain.Common.addrToBase58@ on the decoded address, since the
-- stored raw is exactly that address's byron-CBOR serialisation.
rawToDisplayText :: ByteString -> Text
rawToDisplayText raw = case BS.uncons raw of
  Just (header, _)
    | header .&. 0x80 == 0 -> serialiseShelleyAddrToBech32 raw
  _ -> TextEnc.decodeUtf8 (encodeBase58 bitcoinAlphabet raw)

-- | Whether the address header marks a script payment credential (bit
-- 4). Byron addresses never carry scripts.
rawHasScript :: ByteString -> Bool
rawHasScript bs
  | BS.null bs = False
  | otherwise  = (BS.head bs .&. 0x10) /= 0

-- | The 28-byte payment credential (bytes 1..28) of a Shelley payment
-- address (header high nibble @0x0@-@0x7@). Byron raws, reward
-- addresses, and anything shorter than 29 bytes yield 'Nothing'.
extractPaymentCred :: ByteString -> Maybe ByteString
extractPaymentCred bs
  | BS.length bs < 29 = Nothing
  | otherwise =
      let typeBits = BS.head bs .&. 0xF0
      in if typeBits <= 0x70
           then Just (BS.take 28 (BS.drop 1 bs))
           else Nothing

-- ---------------------------------------------------------------------------
-- * Table definitions
-- ---------------------------------------------------------------------------

addressTableDef :: TableDef
addressTableDef = TableDef
  { tdName    = "address"
  , tdColumns =
      [ ColumnDef "id"               PgBigInt  False
      , ColumnDef "address"          PgText    False
      , ColumnDef "raw"              PgBytea   False
      , ColumnDef "has_script"       PgBoolean False
      , ColumnDef "payment_cred"     PgBytea   True
      , ColumnDef "stake_address_id" PgBigInt  True
      , ColumnDef "raw_hash"         PgBytea   False
      ]
  , tdMode    = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
    -- Plutus script addresses can exceed PG's btree row-size limit
    -- (~8191 B), so the dedup index is on a fixed-size md5 of @raw@
    -- rather than on @raw@ itself. Application-side dedup still keys
    -- on @raw@; this constraint is the DB-level safety net.
  , tdUniqueConstraints = [pure "raw_hash"]
  , tdGeneratedColumns  = [("raw_hash", "decode(md5(raw), 'hex')")]
  , tdIdentityColumns = []
  , tdForeignKeys = []
  }

-- ---------------------------------------------------------------------------
-- * Column records
-- ---------------------------------------------------------------------------

data AddressCols = AddressCols
  { acId             :: !TableColumn
  , acAddress        :: !TableColumn
  , acRaw            :: !TableColumn
  , acHasScript      :: !TableColumn
  , acPaymentCred    :: !TableColumn
  , acStakeAddressId :: !TableColumn
  , acRawHash        :: !TableColumn
  }

addressCols :: AddressCols
addressCols =
  let c = TableColumn addressTableDef
  in AddressCols
       { acId             = c "id"
       , acAddress        = c "address"
       , acRaw            = c "raw"
       , acHasScript      = c "has_script"
       , acPaymentCred    = c "payment_cred"
       , acStakeAddressId = c "stake_address_id"
       , acRawHash        = c "raw_hash"
       }

addressColsList :: [TableColumn]
addressColsList =
  [ addressCols.acId
  , addressCols.acAddress
  , addressCols.acRaw
  , addressCols.acHasScript
  , addressCols.acPaymentCred
  , addressCols.acStakeAddressId
  , addressCols.acRawHash
  ]

-- ---------------------------------------------------------------------------
-- * Per-module column-record registry
-- ---------------------------------------------------------------------------

addressColumnRecords :: [(TableDef, [TableColumn])]
addressColumnRecords =
  [ (addressTableDef, addressColsList)
  ]

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
