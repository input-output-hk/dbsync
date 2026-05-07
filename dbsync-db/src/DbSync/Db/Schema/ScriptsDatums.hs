{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : DbSync.Db.Schema.ScriptsDatums
Description : Schema types for the @scripts_datums@ extractor.

The @scripts_datums@ extractor owns five tables, all populated from
the witness set of a transaction:

  * @datum@ — Plutus datums (deduped on @hash@).
  * @script@ — script payloads (Plutus / native; deduped on @hash@).
  * @redeemer@ — script invocations.
  * @redeemer_data@ — Plutus redeemer payloads (deduped on @hash@).
  * @extra_key_witness@ — required-signer hashes.

The FK from @redeemer.redeemer_data_id@ to @redeemer_data.id@ forces
all five tables into the same extractor.
-}
module DbSync.Db.Schema.ScriptsDatums
  ( -- * Schema types
    Datum (..)
  , Script (..)
  , Redeemer (..)
  , RedeemerData (..)
  , ExtraKeyWitness (..)

    -- * Table definitions
  , datumTableDef
  , scriptTableDef
  , redeemerTableDef
  , redeemerDataTableDef
  , extraKeyWitnessTableDef

    -- * COPY encoding
  , encodeDatumCopy
  , encodeScriptCopy
  , encodeRedeemerCopy
  , encodeRedeemerDataCopy
  , encodeExtraKeyWitnessCopy

    -- * Hasql encoders \/ decoders
  , datumEncoder
  , datumDecoder
  , entityDatumDecoder
  , scriptEncoder
  , scriptDecoder
  , entityScriptDecoder
  , redeemerEncoder
  , redeemerDecoder
  , entityRedeemerDecoder
  , redeemerDataEncoder
  , redeemerDataDecoder
  , entityRedeemerDataDecoder
  , extraKeyWitnessEncoder
  , extraKeyWitnessDecoder
  , entityExtraKeyWitnessDecoder
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
  , ScriptPurpose
  , ScriptType
  , dbLovelaceValueDecoder
  , dbLovelaceValueEncoder
  , bScriptPurpose
  , bScriptType
  , scriptPurposeDecoder
  , scriptPurposeEncoder
  , scriptTypeDecoder
  , scriptTypeEncoder
  )
import DbSync.Db.Writer.Copy.Encoder
  ( buildCopyRow
  , bHex
  , bInt64
  , bText
  , bWord64
  )

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key Datum = DatumId
type instance Key Script = ScriptId
type instance Key Redeemer = RedeemerId
type instance Key RedeemerData = RedeemerDataId
type instance Key ExtraKeyWitness = ExtraKeyWitnessId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | The @datum@ table. Unique on @hash@. JSONB @value@.
data Datum = Datum
  { datumHash  :: !ByteString
  , datumTxId  :: !TxId
  , datumValue :: !(Maybe Text)  -- ^ JSONB-as-text (hasql streams JSONB through 'E.text')
  , datumBytes :: !ByteString
  }
  deriving stock (Eq, Show)

-- | The @script@ table. Unique on @hash@.
data Script = Script
  { scriptTxId            :: !TxId
  , scriptHash            :: !ByteString
  , scriptType            :: !ScriptType
  , scriptJson            :: !(Maybe Text)        -- ^ JSONB-as-text
  , scriptBytes           :: !(Maybe ByteString)
  , scriptSerialisedSize  :: !(Maybe Word64)
  }
  deriving stock (Eq, Show)

-- | The @redeemer@ table. References @redeemer_data@ via 'redeemerRedeemerDataId'.
data Redeemer = Redeemer
  { redeemerTxId            :: !TxId
  , redeemerUnitMem         :: !Word64
  , redeemerUnitSteps       :: !Word64
  , redeemerFee             :: !(Maybe DbLovelace)
  , redeemerPurpose         :: !ScriptPurpose
  , redeemerIndex           :: !Word64
  , redeemerScriptHash      :: !(Maybe ByteString)
  , redeemerRedeemerDataId  :: !RedeemerDataId
  }
  deriving stock (Eq, Show)

-- | The @redeemer_data@ table. Unique on @hash@. JSONB @value@.
data RedeemerData = RedeemerData
  { redeemerDataHash  :: !ByteString
  , redeemerDataTxId  :: !TxId
  , redeemerDataValue :: !(Maybe Text)  -- ^ JSONB-as-text
  , redeemerDataBytes :: !ByteString
  }
  deriving stock (Eq, Show)

-- | The @extra_key_witness@ table.
data ExtraKeyWitness = ExtraKeyWitness
  { extraKeyWitnessHash :: !ByteString
  , extraKeyWitnessTxId :: !TxId
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definitions
-- ---------------------------------------------------------------------------

datumTableDef :: TableDef
datumTableDef = TableDef
  { tdName    = "datum"
  , tdColumns =
      [ ColumnDef "id"    PgBigInt False
      , ColumnDef "hash"  PgBytea  False
      , ColumnDef "tx_id" PgBigInt False
      , ColumnDef "value" PgJsonb  True
      , ColumnDef "bytes" PgBytea  False
      ]
  , tdMode    = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = [pure "hash"]
  , tdGeneratedColumns = []
  }

scriptTableDef :: TableDef
scriptTableDef = TableDef
  { tdName    = "script"
  , tdColumns =
      [ ColumnDef "id"              PgBigInt              False
      , ColumnDef "tx_id"           PgBigInt              False
      , ColumnDef "hash"            PgBytea               False
      , ColumnDef "type"            (PgEnum "scripttype") False
      , ColumnDef "json"            PgJsonb               True
      , ColumnDef "bytes"           PgBytea               True
      , ColumnDef "serialised_size" PgBigInt              True
      ]
  , tdMode    = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = [pure "hash"]
  , tdGeneratedColumns = []
  }

redeemerTableDef :: TableDef
redeemerTableDef = TableDef
  { tdName    = "redeemer"
  , tdColumns =
      [ ColumnDef "id"               PgBigInt                     False
      , ColumnDef "tx_id"            PgBigInt                     False
      , ColumnDef "unit_mem"         PgBigInt                     False
      , ColumnDef "unit_steps"       PgBigInt                     False
      , ColumnDef "fee"              PgNumeric                    True
      , ColumnDef "purpose"          (PgEnum "scriptpurposetype") False
      , ColumnDef "index"            PgBigInt                     False
      , ColumnDef "script_hash"      PgBytea                      True
      , ColumnDef "redeemer_data_id" PgBigInt                     False
      ]
  , tdMode    = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  }

redeemerDataTableDef :: TableDef
redeemerDataTableDef = TableDef
  { tdName    = "redeemer_data"
  , tdColumns =
      [ ColumnDef "id"    PgBigInt False
      , ColumnDef "hash"  PgBytea  False
      , ColumnDef "tx_id" PgBigInt False
      , ColumnDef "value" PgJsonb  True
      , ColumnDef "bytes" PgBytea  False
      ]
  , tdMode    = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = [pure "hash"]
  , tdGeneratedColumns = []
  }

extraKeyWitnessTableDef :: TableDef
extraKeyWitnessTableDef = TableDef
  { tdName    = "extra_key_witness"
  , tdColumns =
      [ ColumnDef "id"    PgBigInt False
      , ColumnDef "hash"  PgBytea  False
      , ColumnDef "tx_id" PgBigInt False
      ]
  , tdMode    = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  }

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodeDatumCopy :: DatumId -> Datum -> ByteString
encodeDatumCopy (DatumId did) d =
  buildCopyRow
    [ Just $ bInt64 did
    , Just $ bHex (datumHash d)
    , Just $ bInt64 (getTxId $ datumTxId d)
    , bText <$> datumValue d
    , Just $ bHex (datumBytes d)
    ]

encodeScriptCopy :: ScriptId -> Script -> ByteString
encodeScriptCopy (ScriptId sid) s =
  buildCopyRow
    [ Just $ bInt64 sid
    , Just $ bInt64 (getTxId $ scriptTxId s)
    , Just $ bHex (scriptHash s)
    , Just $ bScriptType (scriptType s)
    , bText <$> scriptJson s
    , bHex <$> scriptBytes s
    , bWord64 <$> scriptSerialisedSize s
    ]

encodeRedeemerCopy :: RedeemerId -> Redeemer -> ByteString
encodeRedeemerCopy (RedeemerId rid) r =
  buildCopyRow
    [ Just $ bInt64 rid
    , Just $ bInt64 (getTxId $ redeemerTxId r)
    , Just $ bWord64 (redeemerUnitMem r)
    , Just $ bWord64 (redeemerUnitSteps r)
    , bWord64 . unDbLovelace <$> redeemerFee r
    , Just $ bScriptPurpose (redeemerPurpose r)
    , Just $ bWord64 (redeemerIndex r)
    , bHex <$> redeemerScriptHash r
    , Just $ bInt64 (getRedeemerDataId $ redeemerRedeemerDataId r)
    ]

encodeRedeemerDataCopy :: RedeemerDataId -> RedeemerData -> ByteString
encodeRedeemerDataCopy (RedeemerDataId rdid) rd =
  buildCopyRow
    [ Just $ bInt64 rdid
    , Just $ bHex (redeemerDataHash rd)
    , Just $ bInt64 (getTxId $ redeemerDataTxId rd)
    , bText <$> redeemerDataValue rd
    , Just $ bHex (redeemerDataBytes rd)
    ]

encodeExtraKeyWitnessCopy :: ExtraKeyWitnessId -> ExtraKeyWitness -> ByteString
encodeExtraKeyWitnessCopy (ExtraKeyWitnessId ekid) ekw =
  buildCopyRow
    [ Just $ bInt64 ekid
    , Just $ bHex (extraKeyWitnessHash ekw)
    , Just $ bInt64 (getTxId $ extraKeyWitnessTxId ekw)
    ]

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------

datumEncoder :: E.Params Datum
datumEncoder = mconcat
  [ datumHash  >$< E.param (E.nonNullable E.bytea)
  , datumTxId  >$< idEncoder getTxId
  , datumValue >$< E.param (E.nullable E.text)
  , datumBytes >$< E.param (E.nonNullable E.bytea)
  ]

datumDecoder :: D.Row Datum
datumDecoder = Datum
  <$> D.column (D.nonNullable D.bytea)
  <*> idDecoder TxId
  <*> D.column (D.nullable D.text)
  <*> D.column (D.nonNullable D.bytea)

entityDatumDecoder :: D.Row (DatumId, Datum)
entityDatumDecoder = (,)
  <$> idDecoder DatumId
  <*> datumDecoder

scriptEncoder :: E.Params Script
scriptEncoder = mconcat
  [ scriptTxId           >$< idEncoder getTxId
  , scriptHash           >$< E.param (E.nonNullable E.bytea)
  , scriptType           >$< E.param (E.nonNullable scriptTypeEncoder)
  , scriptJson           >$< E.param (E.nullable E.text)
  , scriptBytes          >$< E.param (E.nullable E.bytea)
  , scriptSerialisedSize >$< E.param (E.nullable $ fromIntegral >$< E.int8)
  ]

scriptDecoder :: D.Row Script
scriptDecoder = Script
  <$> idDecoder TxId
  <*> D.column (D.nonNullable D.bytea)
  <*> D.column (D.nonNullable scriptTypeDecoder)
  <*> D.column (D.nullable D.text)
  <*> D.column (D.nullable D.bytea)
  <*> D.column (D.nullable $ fromIntegral <$> D.int8)

entityScriptDecoder :: D.Row (ScriptId, Script)
entityScriptDecoder = (,)
  <$> idDecoder ScriptId
  <*> scriptDecoder

redeemerEncoder :: E.Params Redeemer
redeemerEncoder = mconcat
  [ redeemerTxId           >$< idEncoder getTxId
  , redeemerUnitMem        >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , redeemerUnitSteps      >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , redeemerFee            >$< E.param (E.nullable dbLovelaceValueEncoder)
  , redeemerPurpose        >$< E.param (E.nonNullable scriptPurposeEncoder)
  , redeemerIndex          >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , redeemerScriptHash     >$< E.param (E.nullable E.bytea)
  , redeemerRedeemerDataId >$< idEncoder getRedeemerDataId
  ]

redeemerDecoder :: D.Row Redeemer
redeemerDecoder = Redeemer
  <$> idDecoder TxId
  <*> D.column (D.nonNullable $ fromIntegral <$> D.int8)
  <*> D.column (D.nonNullable $ fromIntegral <$> D.int8)
  <*> D.column (D.nullable dbLovelaceValueDecoder)
  <*> D.column (D.nonNullable scriptPurposeDecoder)
  <*> D.column (D.nonNullable $ fromIntegral <$> D.int8)
  <*> D.column (D.nullable D.bytea)
  <*> idDecoder RedeemerDataId

entityRedeemerDecoder :: D.Row (RedeemerId, Redeemer)
entityRedeemerDecoder = (,)
  <$> idDecoder RedeemerId
  <*> redeemerDecoder

redeemerDataEncoder :: E.Params RedeemerData
redeemerDataEncoder = mconcat
  [ redeemerDataHash  >$< E.param (E.nonNullable E.bytea)
  , redeemerDataTxId  >$< idEncoder getTxId
  , redeemerDataValue >$< E.param (E.nullable E.text)
  , redeemerDataBytes >$< E.param (E.nonNullable E.bytea)
  ]

redeemerDataDecoder :: D.Row RedeemerData
redeemerDataDecoder = RedeemerData
  <$> D.column (D.nonNullable D.bytea)
  <*> idDecoder TxId
  <*> D.column (D.nullable D.text)
  <*> D.column (D.nonNullable D.bytea)

entityRedeemerDataDecoder :: D.Row (RedeemerDataId, RedeemerData)
entityRedeemerDataDecoder = (,)
  <$> idDecoder RedeemerDataId
  <*> redeemerDataDecoder

extraKeyWitnessEncoder :: E.Params ExtraKeyWitness
extraKeyWitnessEncoder = mconcat
  [ extraKeyWitnessHash >$< E.param (E.nonNullable E.bytea)
  , extraKeyWitnessTxId >$< idEncoder getTxId
  ]

extraKeyWitnessDecoder :: D.Row ExtraKeyWitness
extraKeyWitnessDecoder = ExtraKeyWitness
  <$> D.column (D.nonNullable D.bytea)
  <*> idDecoder TxId

entityExtraKeyWitnessDecoder :: D.Row (ExtraKeyWitnessId, ExtraKeyWitness)
entityExtraKeyWitnessDecoder = (,)
  <$> idDecoder ExtraKeyWitnessId
  <*> extraKeyWitnessDecoder
