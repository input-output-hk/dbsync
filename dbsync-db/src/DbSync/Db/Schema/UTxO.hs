{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema types for the UTxO extractor tables: tx_out, tx_in,
-- collateral_tx_in, reference_tx_in.
--
-- During 'IngestChainHistory', @tx_in.tx_out_id@ is NULL (deferred
-- resolution). The @tx_out_hash@ and @tx_out_index@ columns are
-- populated instead, and resolved via a post-load SQL join during
-- 'PreparingForVolatileTail'.
module DbSync.Db.Schema.UTxO
  ( -- * Schema types
    TxOut (..)
  , TxIn (..)
  , CollateralTxIn (..)
  , CollateralTxOut (..)
  , ReferenceTxIn (..)

    -- * Table definitions
  , txOutTableDef
  , txInTableDef
  , collateralTxInTableDef
  , collateralTxOutTableDef
  , referenceTxInTableDef

    -- * Column records (compile-time-safe column references)
  , TxOutCols (..)
  , txOutCols
  , txOutColsList
  , TxInCols (..)
  , txInCols
  , txInColsList
  , CollateralTxInCols (..)
  , collateralTxInCols
  , collateralTxInColsList
  , CollateralTxOutCols (..)
  , collateralTxOutCols
  , collateralTxOutColsList
  , ReferenceTxInCols (..)
  , referenceTxInCols
  , referenceTxInColsList

    -- * Per-module column-record registry
  , utxoColumnRecords

    -- * COPY encoding
  , encodeTxOutCopy
  , encodeTxInCopy
  , encodeCollateralTxInCopy
  , encodeCollateralTxOutCopy
  , encodeReferenceTxInCopy

    -- * Hasql encoders \/ decoders
  , txOutEncoder
  , txOutDecoder
  , entityTxOutDecoder
  , txInEncoder
  , txInDecoder
  , entityTxInDecoder
  , collateralTxInEncoder
  , collateralTxInDecoder
  , entityCollateralTxInDecoder
  , collateralTxOutEncoder
  , collateralTxOutDecoder
  , entityCollateralTxOutDecoder
  , referenceTxInEncoder
  , referenceTxInDecoder
  , entityReferenceTxInDecoder
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
import DbSync.Db.Types (DbLovelace (..), dbLovelaceValueDecoder, dbLovelaceValueEncoder)

import DbSync.Db.Loader.Encoder (buildCopyRow, bHex, bInt64, bText, bWord64)

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key TxOut = TxOutId
type instance Key TxIn = TxInId
type instance Key CollateralTxIn = CollateralTxInId
type instance Key CollateralTxOut = CollateralTxOutId
type instance Key ReferenceTxIn = ReferenceTxInId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | The @tx_out@ table.
--
-- Address columns are normalised into the @address@ dedup table;
-- @txOutAddressId@ is the FK into it.
--
-- The FK is permanently nullable: 'Nothing' for rows whose owning
-- epoch hasn't been processed by the 'AddressResolver' worker yet,
-- 'Just' once the worker fills it in. The column stays @NULL@-able
-- across all phases, keeping the on-disk shape stable for downstream
-- consumers.
data TxOut = TxOut
  { txOutTxId              :: !TxId             -- ^ FK to tx
  , txOutIndex             :: !Word64           -- ^ Output index within the transaction
  , txOutAddressId         :: !(Maybe AddressId) -- ^ FK to address (NULL during ingest)
  , txOutStakeAddressId    :: !(Maybe StakeAddressId) -- ^ FK to stake_address (NULL when the address has no stake part)
  , txOutValue             :: !DbLovelace       -- ^ Lovelace value
  , txOutDataHash          :: !(Maybe ByteString) -- ^ Datum hash (Alonzo+)
  , txOutInlineDatumId     :: !(Maybe DatumId)  -- ^ FK to datum (NULL without an inline datum)
  , txOutReferenceScriptId :: !(Maybe ScriptId) -- ^ FK to script (NULL without a reference script)
  , txOutConsumedByTxId    :: !(Maybe TxId)     -- ^ FK to consuming tx (NULL during ingest)
  }
  deriving stock (Eq, Show)

-- | The @tx_in@ table.
-- During 'IngestChainHistory', @txInTxOutId@ is 'Nothing'. The hash
-- and index are stored for post-load resolution.
data TxIn = TxIn
  { txInTxInId      :: !TxId             -- ^ The spending transaction
  , txInTxOutId     :: !(Maybe TxId)     -- ^ The tx that created the output (NULL during ingest)
  , txInTxOutIndex  :: !Word64           -- ^ Output index being spent
  , txInTxOutHash   :: !ByteString       -- ^ Hash of the tx being spent (for deferred resolution)
  , txInRedeemerId  :: !(Maybe RedeemerId) -- ^ FK to redeemer (NULL unless a script witnesses the input)
  }
  deriving stock (Eq, Show)

-- | The @collateral_tx_in@ table.
data CollateralTxIn = CollateralTxIn
  { collateralTxInTxInId     :: !TxId
  , collateralTxInTxOutId    :: !(Maybe TxId)
  , collateralTxInTxOutIndex :: !Word64
  , collateralTxInTxOutHash  :: !ByteString
  }
  deriving stock (Eq, Show)

-- | The @reference_tx_in@ table.
data ReferenceTxIn = ReferenceTxIn
  { referenceTxInTxInId     :: !TxId
  , referenceTxInTxOutId    :: !(Maybe TxId)
  , referenceTxInTxOutIndex :: !Word64
  , referenceTxInTxOutHash  :: !ByteString
  }
  deriving stock (Eq, Show)

-- | The @collateral_tx_out@ table — the optional collateral-return
-- output of a Babbage+ phase-2 failed transaction. Schema mirrors
-- the @tx_out@ shape with one addition: @multi_assets_descr@ is a
-- textual dump of the multi-asset map, not normalised on this table.
--
-- @collateralTxOutAddressId@ follows the same lifecycle as
-- 'TxOut.txOutAddressId': permanently nullable, filled in by the
-- 'AddressResolver' worker an epoch after the row is written.
data CollateralTxOut = CollateralTxOut
  { collateralTxOutTxId               :: !TxId
  , collateralTxOutIndex              :: !Word64
  , collateralTxOutAddressId          :: !(Maybe AddressId)
  , collateralTxOutStakeAddressId     :: !(Maybe StakeAddressId)
  , collateralTxOutValue              :: !DbLovelace
  , collateralTxOutDataHash           :: !(Maybe ByteString)
  , collateralTxOutMultiAssetsDescr   :: !Text
  , collateralTxOutInlineDatumId      :: !(Maybe DatumId)
  , collateralTxOutReferenceScriptId  :: !(Maybe ScriptId)
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definitions
-- ---------------------------------------------------------------------------

txOutTableDef :: TableDef
txOutTableDef = TableDef
  { tdName    = "tx_out"
  , tdColumns =
      [ ColumnDef "id"                  PgBigInt   False
      , ColumnDef "tx_id"               PgBigInt   False
      , ColumnDef "index"               PgBigInt   False
        -- address_id is nullable. The AddressResolver worker fills
        -- it in an epoch after the row is written.
      , ColumnDef "address_id"          PgBigInt   True
      , ColumnDef "stake_address_id"    PgBigInt   True
      , ColumnDef "value"               PgNumeric  False
      , ColumnDef "data_hash"           PgBytea    True
      , ColumnDef "inline_datum_id"     PgBigInt   True
      , ColumnDef "reference_script_id" PgBigInt   True
      , ColumnDef "consumed_by_tx_id"   PgBigInt   True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
    -- One row per (tx, output index).
  , tdUniqueConstraints = ["tx_id" :| ["index"]]
  , tdGeneratedColumns = []
  , tdIdentityColumns = []
  , tdParentRefs =
      [ ParentRef "tx_id" "tx" "id"
      ]
  }

txInTableDef :: TableDef
txInTableDef = TableDef
  { tdName    = "tx_in"
  , tdColumns =
      [ ColumnDef "id"            PgBigInt  False
      , ColumnDef "tx_in_id"      PgBigInt  False
      , ColumnDef "tx_out_id"     PgBigInt  True   -- NULL during ingest
      , ColumnDef "tx_out_index"  PgBigInt  False
      , ColumnDef "tx_out_hash"   PgBytea   False  -- for deferred resolution
      , ColumnDef "redeemer_id"   PgBigInt  True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdParentRefs =
      [ ParentRef "tx_in_id" "tx" "id"
      ]
  }

collateralTxInTableDef :: TableDef
collateralTxInTableDef = TableDef
  { tdName    = "collateral_tx_in"
  , tdColumns =
      [ ColumnDef "id"            PgBigInt  False
      , ColumnDef "tx_in_id"      PgBigInt  False
      , ColumnDef "tx_out_id"     PgBigInt  True
      , ColumnDef "tx_out_index"  PgBigInt  False
      , ColumnDef "tx_out_hash"   PgBytea   False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdParentRefs =
      [ ParentRef "tx_in_id" "tx" "id"
      ]
  }

referenceTxInTableDef :: TableDef
referenceTxInTableDef = TableDef
  { tdName    = "reference_tx_in"
  , tdColumns =
      [ ColumnDef "id"            PgBigInt  False
      , ColumnDef "tx_in_id"      PgBigInt  False
      , ColumnDef "tx_out_id"     PgBigInt  True
      , ColumnDef "tx_out_index"  PgBigInt  False
      , ColumnDef "tx_out_hash"   PgBytea   False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdParentRefs =
      [ ParentRef "tx_in_id" "tx" "id"
      ]
  }

-- | @collateral_tx_out@ keeps an explicit @id@ — the address-buffer
-- worker queues @(collateral_tx_out_id, raw, address)@ triples at
-- write time and runs one bulk @UPDATE … WHERE id = ANY($1)@ per
-- epoch; that flow needs the id known at COPY time.
collateralTxOutTableDef :: TableDef
collateralTxOutTableDef = TableDef
  { tdName    = "collateral_tx_out"
  , tdColumns =
      [ ColumnDef "id"                  PgBigInt   False
      , ColumnDef "tx_id"               PgBigInt   False
      , ColumnDef "index"               PgBigInt   False
        -- See note on txOutTableDef.address_id.
      , ColumnDef "address_id"          PgBigInt   True
      , ColumnDef "stake_address_id"    PgBigInt   True
      , ColumnDef "value"               PgNumeric  False
      , ColumnDef "data_hash"           PgBytea    True
      , ColumnDef "multi_assets_descr"  PgText     False
      , ColumnDef "inline_datum_id"     PgBigInt   True
      , ColumnDef "reference_script_id" PgBigInt   True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = []
  , tdParentRefs =
      [ ParentRef "tx_id" "tx" "id"
      ]
  }

-- ---------------------------------------------------------------------------
-- * Column records
-- ---------------------------------------------------------------------------

data TxOutCols = TxOutCols
  { tocId                :: !TableColumn
  , tocTxId              :: !TableColumn
  , tocIndex             :: !TableColumn
  , tocAddressId         :: !TableColumn
  , tocStakeAddressId    :: !TableColumn
  , tocValue             :: !TableColumn
  , tocDataHash          :: !TableColumn
  , tocInlineDatumId     :: !TableColumn
  , tocReferenceScriptId :: !TableColumn
  , tocConsumedByTxId    :: !TableColumn
  }

txOutCols :: TxOutCols
txOutCols =
  let c = TableColumn txOutTableDef
  in TxOutCols
       { tocId                = c "id"
       , tocTxId              = c "tx_id"
       , tocIndex             = c "index"
       , tocAddressId         = c "address_id"
       , tocStakeAddressId    = c "stake_address_id"
       , tocValue             = c "value"
       , tocDataHash          = c "data_hash"
       , tocInlineDatumId     = c "inline_datum_id"
       , tocReferenceScriptId = c "reference_script_id"
       , tocConsumedByTxId    = c "consumed_by_tx_id"
       }

txOutColsList :: [TableColumn]
txOutColsList =
  [ txOutCols.tocId
  , txOutCols.tocTxId
  , txOutCols.tocIndex
  , txOutCols.tocAddressId
  , txOutCols.tocStakeAddressId
  , txOutCols.tocValue
  , txOutCols.tocDataHash
  , txOutCols.tocInlineDatumId
  , txOutCols.tocReferenceScriptId
  , txOutCols.tocConsumedByTxId
  ]

data TxInCols = TxInCols
  { ticId          :: !TableColumn
  , ticTxInId      :: !TableColumn
  , ticTxOutId     :: !TableColumn
  , ticTxOutIndex  :: !TableColumn
  , ticTxOutHash   :: !TableColumn
  , ticRedeemerId  :: !TableColumn
  }

txInCols :: TxInCols
txInCols =
  let c = TableColumn txInTableDef
  in TxInCols
       { ticId          = c "id"
       , ticTxInId      = c "tx_in_id"
       , ticTxOutId     = c "tx_out_id"
       , ticTxOutIndex  = c "tx_out_index"
       , ticTxOutHash   = c "tx_out_hash"
       , ticRedeemerId  = c "redeemer_id"
       }

txInColsList :: [TableColumn]
txInColsList =
  [ txInCols.ticId
  , txInCols.ticTxInId
  , txInCols.ticTxOutId
  , txInCols.ticTxOutIndex
  , txInCols.ticTxOutHash
  , txInCols.ticRedeemerId
  ]

data CollateralTxInCols = CollateralTxInCols
  { cticId          :: !TableColumn
  , cticTxInId      :: !TableColumn
  , cticTxOutId     :: !TableColumn
  , cticTxOutIndex  :: !TableColumn
  , cticTxOutHash   :: !TableColumn
  }

collateralTxInCols :: CollateralTxInCols
collateralTxInCols =
  let c = TableColumn collateralTxInTableDef
  in CollateralTxInCols
       { cticId          = c "id"
       , cticTxInId      = c "tx_in_id"
       , cticTxOutId     = c "tx_out_id"
       , cticTxOutIndex  = c "tx_out_index"
       , cticTxOutHash   = c "tx_out_hash"
       }

collateralTxInColsList :: [TableColumn]
collateralTxInColsList =
  [ collateralTxInCols.cticId
  , collateralTxInCols.cticTxInId
  , collateralTxInCols.cticTxOutId
  , collateralTxInCols.cticTxOutIndex
  , collateralTxInCols.cticTxOutHash
  ]

data ReferenceTxInCols = ReferenceTxInCols
  { rticId          :: !TableColumn
  , rticTxInId      :: !TableColumn
  , rticTxOutId     :: !TableColumn
  , rticTxOutIndex  :: !TableColumn
  , rticTxOutHash   :: !TableColumn
  }

referenceTxInCols :: ReferenceTxInCols
referenceTxInCols =
  let c = TableColumn referenceTxInTableDef
  in ReferenceTxInCols
       { rticId          = c "id"
       , rticTxInId      = c "tx_in_id"
       , rticTxOutId     = c "tx_out_id"
       , rticTxOutIndex  = c "tx_out_index"
       , rticTxOutHash   = c "tx_out_hash"
       }

referenceTxInColsList :: [TableColumn]
referenceTxInColsList =
  [ referenceTxInCols.rticId
  , referenceTxInCols.rticTxInId
  , referenceTxInCols.rticTxOutId
  , referenceTxInCols.rticTxOutIndex
  , referenceTxInCols.rticTxOutHash
  ]

data CollateralTxOutCols = CollateralTxOutCols
  { ctocId                :: !TableColumn
  , ctocTxId              :: !TableColumn
  , ctocIndex             :: !TableColumn
  , ctocAddressId         :: !TableColumn
  , ctocStakeAddressId    :: !TableColumn
  , ctocValue             :: !TableColumn
  , ctocDataHash          :: !TableColumn
  , ctocMultiAssetsDescr  :: !TableColumn
  , ctocInlineDatumId     :: !TableColumn
  , ctocReferenceScriptId :: !TableColumn
  }

collateralTxOutCols :: CollateralTxOutCols
collateralTxOutCols =
  let c = TableColumn collateralTxOutTableDef
  in CollateralTxOutCols
       { ctocId                = c "id"
       , ctocTxId              = c "tx_id"
       , ctocIndex             = c "index"
       , ctocAddressId         = c "address_id"
       , ctocStakeAddressId    = c "stake_address_id"
       , ctocValue             = c "value"
       , ctocDataHash          = c "data_hash"
       , ctocMultiAssetsDescr  = c "multi_assets_descr"
       , ctocInlineDatumId     = c "inline_datum_id"
       , ctocReferenceScriptId = c "reference_script_id"
       }

collateralTxOutColsList :: [TableColumn]
collateralTxOutColsList =
  [ collateralTxOutCols.ctocId
  , collateralTxOutCols.ctocTxId
  , collateralTxOutCols.ctocIndex
  , collateralTxOutCols.ctocAddressId
  , collateralTxOutCols.ctocStakeAddressId
  , collateralTxOutCols.ctocValue
  , collateralTxOutCols.ctocDataHash
  , collateralTxOutCols.ctocMultiAssetsDescr
  , collateralTxOutCols.ctocInlineDatumId
  , collateralTxOutCols.ctocReferenceScriptId
  ]

-- ---------------------------------------------------------------------------
-- * Per-module column-record registry
-- ---------------------------------------------------------------------------

utxoColumnRecords :: [(TableDef, [TableColumn])]
utxoColumnRecords =
  [ (txOutTableDef,            txOutColsList)
  , (txInTableDef,             txInColsList)
  , (collateralTxInTableDef,   collateralTxInColsList)
  , (collateralTxOutTableDef,  collateralTxOutColsList)
  , (referenceTxInTableDef,    referenceTxInColsList)
  ]

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodeTxOutCopy :: TxOutId -> TxOut -> ByteString
encodeTxOutCopy (TxOutId oid) txo =
  buildCopyRow
    [ Just $ bInt64 oid
    , Just $ bInt64 (getTxId $ txOutTxId txo)
    , Just $ bWord64 (txOutIndex txo)
    , bInt64 . getAddressId <$> txOutAddressId txo
    , bInt64 . getStakeAddressId <$> txOutStakeAddressId txo
    , Just $ bWord64 (unDbLovelace $ txOutValue txo)
    , bHex <$> txOutDataHash txo
    , bInt64 . getDatumId <$> txOutInlineDatumId txo
    , bInt64 . getScriptId <$> txOutReferenceScriptId txo
    , bInt64 . getTxId <$> txOutConsumedByTxId txo
    ]

encodeTxInCopy :: TxIn -> ByteString
encodeTxInCopy ti =
  buildCopyRow
    [ Just $ bInt64 (getTxId $ txInTxInId ti)
    , bInt64 . getTxId <$> txInTxOutId ti
    , Just $ bWord64 (txInTxOutIndex ti)
    , Just $ bHex (txInTxOutHash ti)
    , bInt64 . getRedeemerId <$> txInRedeemerId ti
    ]

encodeCollateralTxInCopy :: CollateralTxIn -> ByteString
encodeCollateralTxInCopy ci =
  buildCopyRow
    [ Just $ bInt64 (getTxId $ collateralTxInTxInId ci)
    , bInt64 . getTxId <$> collateralTxInTxOutId ci
    , Just $ bWord64 (collateralTxInTxOutIndex ci)
    , Just $ bHex (collateralTxInTxOutHash ci)
    ]

encodeReferenceTxInCopy :: ReferenceTxIn -> ByteString
encodeReferenceTxInCopy ri =
  buildCopyRow
    [ Just $ bInt64 (getTxId $ referenceTxInTxInId ri)
    , bInt64 . getTxId <$> referenceTxInTxOutId ri
    , Just $ bWord64 (referenceTxInTxOutIndex ri)
    , Just $ bHex (referenceTxInTxOutHash ri)
    ]

encodeCollateralTxOutCopy :: CollateralTxOutId -> CollateralTxOut -> ByteString
encodeCollateralTxOutCopy (CollateralTxOutId rid) co =
  buildCopyRow
    [ Just $ bInt64 rid
    , Just $ bInt64 (getTxId $ collateralTxOutTxId co)
    , Just $ bWord64 (collateralTxOutIndex co)
    , bInt64 . getAddressId <$> collateralTxOutAddressId co
    , bInt64 . getStakeAddressId <$> collateralTxOutStakeAddressId co
    , Just $ bWord64 (unDbLovelace $ collateralTxOutValue co)
    , bHex <$> collateralTxOutDataHash co
    , Just $ bText (collateralTxOutMultiAssetsDescr co)
    , bInt64 . getDatumId <$> collateralTxOutInlineDatumId co
    , bInt64 . getScriptId <$> collateralTxOutReferenceScriptId co
    ]

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------

-- | Encoder for a 'TxOut', excluding the auto-generated @id@.
-- Field order matches the column order in 'txOutTableDef'.
txOutEncoder :: E.Params TxOut
txOutEncoder = mconcat
  [ txOutTxId             >$< idEncoder      getTxId
  , txOutIndex            >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , txOutAddressId        >$< maybeIdEncoder getAddressId
  , txOutStakeAddressId   >$< maybeIdEncoder getStakeAddressId
  , txOutValue            >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , txOutDataHash         >$< E.param (E.nullable E.bytea)
  , txOutInlineDatumId    >$< maybeIdEncoder getDatumId
  , txOutReferenceScriptId >$< maybeIdEncoder getScriptId
  , txOutConsumedByTxId   >$< maybeIdEncoder getTxId
  ]

-- | Decoder for the data columns of a 'TxOut' (excluding @id@).
txOutDecoder :: D.Row TxOut
txOutDecoder = TxOut
  <$> idDecoder TxId
  <*> D.column (D.nonNullable $ fromIntegral <$> D.int8)
  <*> maybeIdDecoder AddressId
  <*> maybeIdDecoder StakeAddressId
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> D.column (D.nullable D.bytea)
  <*> maybeIdDecoder DatumId
  <*> maybeIdDecoder ScriptId
  <*> maybeIdDecoder TxId

-- | Decoder for a full @tx_out@ row, including @id@.
entityTxOutDecoder :: D.Row (TxOutId, TxOut)
entityTxOutDecoder = (,)
  <$> idDecoder TxOutId
  <*> txOutDecoder

-- | Encoder for a 'TxIn', excluding the auto-generated @id@.
txInEncoder :: E.Params TxIn
txInEncoder = mconcat
  [ txInTxInId     >$< idEncoder      getTxId
  , txInTxOutId    >$< maybeIdEncoder getTxId
  , txInTxOutIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , txInTxOutHash  >$< E.param (E.nonNullable E.bytea)
  , txInRedeemerId >$< maybeIdEncoder getRedeemerId
  ]

-- | Decoder for the data columns of a 'TxIn' (excluding @id@).
txInDecoder :: D.Row TxIn
txInDecoder = TxIn
  <$> idDecoder TxId
  <*> maybeIdDecoder TxId
  <*> D.column (D.nonNullable $ fromIntegral <$> D.int8)
  <*> D.column (D.nonNullable D.bytea)
  <*> maybeIdDecoder RedeemerId

-- | Decoder for a full @tx_in@ row, including @id@.
entityTxInDecoder :: D.Row (TxInId, TxIn)
entityTxInDecoder = (,)
  <$> idDecoder TxInId
  <*> txInDecoder

-- | Encoder for a 'CollateralTxIn', excluding the auto-generated @id@.
collateralTxInEncoder :: E.Params CollateralTxIn
collateralTxInEncoder = mconcat
  [ collateralTxInTxInId     >$< idEncoder      getTxId
  , collateralTxInTxOutId    >$< maybeIdEncoder getTxId
  , collateralTxInTxOutIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , collateralTxInTxOutHash  >$< E.param (E.nonNullable E.bytea)
  ]

-- | Decoder for the data columns of a 'CollateralTxIn' (excluding @id@).
collateralTxInDecoder :: D.Row CollateralTxIn
collateralTxInDecoder = CollateralTxIn
  <$> idDecoder TxId
  <*> maybeIdDecoder TxId
  <*> D.column (D.nonNullable $ fromIntegral <$> D.int8)
  <*> D.column (D.nonNullable D.bytea)

-- | Decoder for a full @collateral_tx_in@ row, including @id@.
entityCollateralTxInDecoder :: D.Row (CollateralTxInId, CollateralTxIn)
entityCollateralTxInDecoder = (,)
  <$> idDecoder CollateralTxInId
  <*> collateralTxInDecoder

-- | Encoder for a 'ReferenceTxIn', excluding the auto-generated @id@.
referenceTxInEncoder :: E.Params ReferenceTxIn
referenceTxInEncoder = mconcat
  [ referenceTxInTxInId     >$< idEncoder      getTxId
  , referenceTxInTxOutId    >$< maybeIdEncoder getTxId
  , referenceTxInTxOutIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , referenceTxInTxOutHash  >$< E.param (E.nonNullable E.bytea)
  ]

-- | Decoder for the data columns of a 'ReferenceTxIn' (excluding @id@).
referenceTxInDecoder :: D.Row ReferenceTxIn
referenceTxInDecoder = ReferenceTxIn
  <$> idDecoder TxId
  <*> maybeIdDecoder TxId
  <*> D.column (D.nonNullable $ fromIntegral <$> D.int8)
  <*> D.column (D.nonNullable D.bytea)

-- | Decoder for a full @reference_tx_in@ row, including @id@.
entityReferenceTxInDecoder :: D.Row (ReferenceTxInId, ReferenceTxIn)
entityReferenceTxInDecoder = (,)
  <$> idDecoder ReferenceTxInId
  <*> referenceTxInDecoder

-- | Encoder for a 'CollateralTxOut', excluding the auto-generated @id@.
collateralTxOutEncoder :: E.Params CollateralTxOut
collateralTxOutEncoder = mconcat
  [ collateralTxOutTxId              >$< idEncoder      getTxId
  , collateralTxOutIndex             >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , collateralTxOutAddressId         >$< maybeIdEncoder getAddressId
  , collateralTxOutStakeAddressId    >$< maybeIdEncoder getStakeAddressId
  , collateralTxOutValue             >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , collateralTxOutDataHash          >$< E.param (E.nullable E.bytea)
  , collateralTxOutMultiAssetsDescr  >$< E.param (E.nonNullable E.text)
  , collateralTxOutInlineDatumId     >$< maybeIdEncoder getDatumId
  , collateralTxOutReferenceScriptId >$< maybeIdEncoder getScriptId
  ]

collateralTxOutDecoder :: D.Row CollateralTxOut
collateralTxOutDecoder = CollateralTxOut
  <$> idDecoder TxId
  <*> D.column (D.nonNullable $ fromIntegral <$> D.int8)
  <*> maybeIdDecoder AddressId
  <*> maybeIdDecoder StakeAddressId
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> D.column (D.nullable D.bytea)
  <*> D.column (D.nonNullable D.text)
  <*> maybeIdDecoder DatumId
  <*> maybeIdDecoder ScriptId

entityCollateralTxOutDecoder :: D.Row (CollateralTxOutId, CollateralTxOut)
entityCollateralTxOutDecoder = (,)
  <$> idDecoder CollateralTxOutId
  <*> collateralTxOutDecoder
