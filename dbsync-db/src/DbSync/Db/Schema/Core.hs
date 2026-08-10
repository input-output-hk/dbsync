{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema types for the @core@ extractor, which owns five tables:
--
--   * @block@, @tx@, @slot_leader@ — block extraction (always-on).
--   * @stake_address@, @pool_hash@ — shared dedup tables, written
--     unconditionally by the block pipeline so they exist regardless
--     of which optional extractors are enabled.
module DbSync.Db.Schema.Core
  ( -- * Schema types
    Block (..)
  , Tx (..)
  , SlotLeader (..)
  , StakeAddress (..)
  , PoolHash (..)

    -- * Table definitions (for DDL generation)
  , blockTableDef
  , txTableDef
  , slotLeaderTableDef
  , stakeAddressTableDef
  , poolHashTableDef

    -- * Column records (compile-time-safe column references)
  , BlockCols (..)
  , blockCols
  , blockColsList
  , TxCols (..)
  , txCols
  , txColsList
  , SlotLeaderCols (..)
  , slotLeaderCols
  , slotLeaderColsList
  , StakeAddressCols (..)
  , stakeAddressCols
  , stakeAddressColsList
  , PoolHashCols (..)
  , poolHashCols
  , poolHashColsList

    -- * Per-module column-record registry
  , coreColumnRecords

    -- * COPY encoding
  , encodeBlockCopy
  , encodeTxCopy
  , encodeSlotLeaderCopy
  , encodeStakeAddressCopy
  , encodePoolHashCopy

    -- * Hasql encoders \/ decoders
  , blockEncoder
  , blockDecoder
  , entityBlockDecoder
  , txEncoder
  , txDecoder
  , entityTxDecoder
  , slotLeaderEncoder
  , slotLeaderDecoder
  , entitySlotLeaderDecoder
  , stakeAddressEncoder
  , stakeAddressDecoder
  , entityStakeAddressDecoder
  , poolHashEncoder
  , poolHashDecoder
  , entityPoolHashDecoder

    -- * Internal encoding helpers (exported for testing)
  , encodeInt64
  , encodeWord64
  , encodeBool
  , encodeHex
  , encodeUTCTime
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (localTimeToUTC, utc, utcToLocalTime)

import qualified Data.ByteString.Char8 as BS8
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
  ( BlockId (..)
  , PoolHashId (..)
  , SlotLeaderId (..)
  , StakeAddressId (..)
  , TxId (..)
  , idDecoder
  , idEncoder
  , maybeIdDecoder
  , maybeIdEncoder
  )
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , ParentRef (..)
  , PgType (..)
  , TableColumn (..)
  , TableDef (..)
  , TableMode (..)
  )
import DbSync.Db.Types
  ( DbLovelace
  , DbWord64
  , dbLovelaceValueDecoder
  , dbLovelaceValueEncoder
  , maybeDbWord64Decoder
  , maybeDbWord64Encoder
  , unDbLovelace
  , unDbWord64
  )
import DbSync.Db.Loader.Encoder
  ( buildCopyRow
  , bBool, bHex, bInt64, bText, bUTCTime, bWord16, bWord64
  )

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key Block = BlockId
type instance Key Tx = TxId
type instance Key SlotLeader = SlotLeaderId
type instance Key StakeAddress = StakeAddressId
type instance Key PoolHash = PoolHashId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | The @id@ column is not a field here. It lives in
-- @'Key' Block = 'BlockId'@, paired through 'Entity'. Every row record in
-- this package follows that rule.
data Block = Block
  { blockHash           :: !ByteString       -- ^ hash32type
  , blockEpochNo        :: !(Maybe Word64)   -- ^ word31type
  , blockSlotNo         :: !(Maybe Word64)   -- ^ word63type
  , blockEpochSlotNo    :: !(Maybe Word64)   -- ^ word31type
  , blockBlockNo        :: !(Maybe Word64)   -- ^ word31type
  , blockPreviousId     :: !(Maybe BlockId)  -- ^ FK to block (noreference)
  , blockSlotLeaderId   :: !SlotLeaderId     -- ^ FK to slot_leader (noreference)
  , blockSize           :: !Word64           -- ^ word31type
  , blockTime           :: !UTCTime          -- ^ timestamp
  , blockTxCount        :: !Word64
  , blockProtoMajor     :: !Word16           -- ^ word31type
  , blockProtoMinor     :: !Word16           -- ^ word31type
  , blockVrfKey         :: !(Maybe Text)     -- ^ Shelley+
  , blockOpCert         :: !(Maybe ByteString) -- ^ hash32type, Shelley+
  , blockOpCertCounter  :: !(Maybe Word64)   -- ^ hash63type, Shelley+
  }
  deriving stock (Eq, Show)

data Tx = Tx
  { txHash              :: !ByteString       -- ^ hash32type
  , txBlockId           :: !BlockId          -- ^ FK to block (noreference)
  , txBlockIndex        :: !Word64           -- ^ word31type — index within the block
  , txOutSum            :: !DbLovelace       -- ^ lovelace
  , txFee               :: !DbLovelace       -- ^ lovelace
  , txDeposit           :: !(Maybe Int64)    -- ^ allows negative values
  , txSize              :: !Word64           -- ^ word31type
  , txInvalidBefore     :: !(Maybe DbWord64) -- ^ word64type — Allegra+
  , txInvalidHereafter  :: !(Maybe DbWord64) -- ^ word64type — Allegra+
  , txValidContract     :: !Bool             -- ^ Alonzo+: False if script fails phase 2
  , txScriptSize        :: !Word64           -- ^ word31type — Alonzo+
  , txTreasuryDonation  :: !DbLovelace       -- ^ lovelace — Conway+, default 0
  }
  deriving stock (Eq, Show)

data SlotLeader = SlotLeader
  { slotLeaderHash        :: !ByteString       -- ^ hash28type
  , slotLeaderPoolHashId  :: !(Maybe PoolHashId) -- ^ non-null when block mined by pool
  , slotLeaderDescription :: !Text
  }
  deriving stock (Eq, Show)

-- | Dedup table, one row per unique stake credential hash.
data StakeAddress = StakeAddress
  { stakeAddressHashRaw    :: !ByteString        -- ^ 28-byte stake credential hash
  , stakeAddressView       :: !Text              -- ^ Bech32 representation
  , stakeAddressScriptHash :: !(Maybe ByteString) -- ^ Script hash if script-based
  }
  deriving stock (Eq, Show)

-- | Dedup table, one row per unique pool key hash.
data PoolHash = PoolHash
  { poolHashHashRaw :: !ByteString  -- ^ Pool key hash (28 bytes)
  , poolHashView    :: !Text        -- ^ Bech32 representation
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definitions
-- ---------------------------------------------------------------------------

-- | Created as UNLOGGED during 'IngestChainHistory'.
blockTableDef :: TableDef
blockTableDef = TableDef
  { tdName    = "block"
  , tdColumns =
      [ ColumnDef "id"              PgBigInt    False
      , ColumnDef "hash"            PgBytea     False
      , ColumnDef "epoch_no"        PgBigInt    True
      , ColumnDef "slot_no"         PgBigInt    True
      , ColumnDef "epoch_slot_no"   PgBigInt    True
      , ColumnDef "block_no"        PgBigInt    True
      , ColumnDef "previous_id"     PgBigInt    True
      , ColumnDef "slot_leader_id"  PgBigInt    False
      , ColumnDef "size"            PgBigInt    False
      , ColumnDef "time"            PgTimestamp  False
      , ColumnDef "tx_count"        PgBigInt    False
      , ColumnDef "proto_major"     PgSmallInt  False
      , ColumnDef "proto_minor"     PgSmallInt  False
      , ColumnDef "vrf_key"         PgText      True
      , ColumnDef "op_cert"         PgBytea     True
      , ColumnDef "op_cert_counter" PgBigInt    True
      ]
  , tdMode    = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = [pure "hash"]
  , tdGeneratedColumns = []
  , tdIdentityColumns = []
  , tdParentRefs = []
  }

txTableDef :: TableDef
txTableDef = TableDef
  { tdName    = "tx"
  , tdColumns =
      [ ColumnDef "id"                PgBigInt    False
      , ColumnDef "hash"              PgBytea     False
      , ColumnDef "block_id"          PgBigInt    False
      , ColumnDef "block_index"       PgBigInt    False
      , ColumnDef "out_sum"           PgNumeric   False
      , ColumnDef "fee"               PgNumeric   False
      , ColumnDef "deposit"           PgBigInt    True
      , ColumnDef "size"              PgBigInt    False
      , ColumnDef "invalid_before"    PgNumeric   True
      , ColumnDef "invalid_hereafter" PgNumeric   True
      , ColumnDef "valid_contract"    PgBoolean   False
      , ColumnDef "script_size"       PgBigInt    False
      , ColumnDef "treasury_donation" PgNumeric   False
      ]
  , tdMode    = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
    -- Unique by protocol. Indexed so post-load UPDATEs that join
    -- tx_in / collateral_tx_in / reference_tx_in to tx on hash
    -- use a lookup instead of seq-scanning the whole tx heap.
  , tdUniqueConstraints = [pure "hash"]
  , tdGeneratedColumns = []
  , tdIdentityColumns = []
  , tdParentRefs =
      [ ParentRef "block_id" "block" "id"
      ]
  }

slotLeaderTableDef :: TableDef
slotLeaderTableDef = TableDef
  { tdName    = "slot_leader"
  , tdColumns =
      [ ColumnDef "id"           PgBigInt  False
      , ColumnDef "hash"         PgBytea   False
      , ColumnDef "pool_hash_id" PgBigInt  True
      , ColumnDef "description"  PgText    False
      ]
  , tdMode    = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = []
  , tdParentRefs = []
  }

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
    -- Unique by 28-byte credential hash. The Follow dedup resolver
    -- SELECTs on this column for every unique stake address per
    -- block; without the index every resolve sequential-scans the
    -- whole table.
  , tdUniqueConstraints = [pure "hash_raw"]
  , tdGeneratedColumns = []
  , tdIdentityColumns = []
  , tdParentRefs = []
  }

poolHashTableDef :: TableDef
poolHashTableDef = TableDef
  { tdName    = "pool_hash"
  , tdColumns =
      [ ColumnDef "id"       PgBigInt  False
      , ColumnDef "hash_raw" PgBytea   False
      , ColumnDef "view"     PgText    False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = []
  , tdParentRefs = []
  }

-- ---------------------------------------------------------------------------
-- * Column records
-- ---------------------------------------------------------------------------

data BlockCols = BlockCols
  { bcId             :: !TableColumn
  , bcHash           :: !TableColumn
  , bcEpochNo        :: !TableColumn
  , bcSlotNo         :: !TableColumn
  , bcEpochSlotNo    :: !TableColumn
  , bcBlockNo        :: !TableColumn
  , bcPreviousId     :: !TableColumn
  , bcSlotLeaderId   :: !TableColumn
  , bcSize           :: !TableColumn
  , bcTime           :: !TableColumn
  , bcTxCount        :: !TableColumn
  , bcProtoMajor     :: !TableColumn
  , bcProtoMinor     :: !TableColumn
  , bcVrfKey         :: !TableColumn
  , bcOpCert         :: !TableColumn
  , bcOpCertCounter  :: !TableColumn
  }

blockCols :: BlockCols
blockCols =
  let c = TableColumn blockTableDef
  in BlockCols
       { bcId             = c "id"
       , bcHash           = c "hash"
       , bcEpochNo        = c "epoch_no"
       , bcSlotNo         = c "slot_no"
       , bcEpochSlotNo    = c "epoch_slot_no"
       , bcBlockNo        = c "block_no"
       , bcPreviousId     = c "previous_id"
       , bcSlotLeaderId   = c "slot_leader_id"
       , bcSize           = c "size"
       , bcTime           = c "time"
       , bcTxCount        = c "tx_count"
       , bcProtoMajor     = c "proto_major"
       , bcProtoMinor     = c "proto_minor"
       , bcVrfKey         = c "vrf_key"
       , bcOpCert         = c "op_cert"
       , bcOpCertCounter  = c "op_cert_counter"
       }

blockColsList :: [TableColumn]
blockColsList =
  [ blockCols.bcId
  , blockCols.bcHash
  , blockCols.bcEpochNo
  , blockCols.bcSlotNo
  , blockCols.bcEpochSlotNo
  , blockCols.bcBlockNo
  , blockCols.bcPreviousId
  , blockCols.bcSlotLeaderId
  , blockCols.bcSize
  , blockCols.bcTime
  , blockCols.bcTxCount
  , blockCols.bcProtoMajor
  , blockCols.bcProtoMinor
  , blockCols.bcVrfKey
  , blockCols.bcOpCert
  , blockCols.bcOpCertCounter
  ]

data TxCols = TxCols
  { tcId               :: !TableColumn
  , tcHash             :: !TableColumn
  , tcBlockId          :: !TableColumn
  , tcBlockIndex       :: !TableColumn
  , tcOutSum           :: !TableColumn
  , tcFee              :: !TableColumn
  , tcDeposit          :: !TableColumn
  , tcSize             :: !TableColumn
  , tcInvalidBefore    :: !TableColumn
  , tcInvalidHereafter :: !TableColumn
  , tcValidContract    :: !TableColumn
  , tcScriptSize       :: !TableColumn
  , tcTreasuryDonation :: !TableColumn
  }

txCols :: TxCols
txCols =
  let c = TableColumn txTableDef
  in TxCols
       { tcId               = c "id"
       , tcHash             = c "hash"
       , tcBlockId          = c "block_id"
       , tcBlockIndex       = c "block_index"
       , tcOutSum           = c "out_sum"
       , tcFee              = c "fee"
       , tcDeposit          = c "deposit"
       , tcSize             = c "size"
       , tcInvalidBefore    = c "invalid_before"
       , tcInvalidHereafter = c "invalid_hereafter"
       , tcValidContract    = c "valid_contract"
       , tcScriptSize       = c "script_size"
       , tcTreasuryDonation = c "treasury_donation"
       }

txColsList :: [TableColumn]
txColsList =
  [ txCols.tcId
  , txCols.tcHash
  , txCols.tcBlockId
  , txCols.tcBlockIndex
  , txCols.tcOutSum
  , txCols.tcFee
  , txCols.tcDeposit
  , txCols.tcSize
  , txCols.tcInvalidBefore
  , txCols.tcInvalidHereafter
  , txCols.tcValidContract
  , txCols.tcScriptSize
  , txCols.tcTreasuryDonation
  ]

data SlotLeaderCols = SlotLeaderCols
  { slcId          :: !TableColumn
  , slcHash        :: !TableColumn
  , slcPoolHashId  :: !TableColumn
  , slcDescription :: !TableColumn
  }

slotLeaderCols :: SlotLeaderCols
slotLeaderCols =
  let c = TableColumn slotLeaderTableDef
  in SlotLeaderCols
       { slcId          = c "id"
       , slcHash        = c "hash"
       , slcPoolHashId  = c "pool_hash_id"
       , slcDescription = c "description"
       }

slotLeaderColsList :: [TableColumn]
slotLeaderColsList =
  [ slotLeaderCols.slcId
  , slotLeaderCols.slcHash
  , slotLeaderCols.slcPoolHashId
  , slotLeaderCols.slcDescription
  ]

data StakeAddressCols = StakeAddressCols
  { sacId         :: !TableColumn
  , sacHashRaw    :: !TableColumn
  , sacView       :: !TableColumn
  , sacScriptHash :: !TableColumn
  }

stakeAddressCols :: StakeAddressCols
stakeAddressCols =
  let c = TableColumn stakeAddressTableDef
  in StakeAddressCols
       { sacId         = c "id"
       , sacHashRaw    = c "hash_raw"
       , sacView       = c "view"
       , sacScriptHash = c "script_hash"
       }

stakeAddressColsList :: [TableColumn]
stakeAddressColsList =
  [ stakeAddressCols.sacId
  , stakeAddressCols.sacHashRaw
  , stakeAddressCols.sacView
  , stakeAddressCols.sacScriptHash
  ]

data PoolHashCols = PoolHashCols
  { phcId      :: !TableColumn
  , phcHashRaw :: !TableColumn
  , phcView    :: !TableColumn
  }

poolHashCols :: PoolHashCols
poolHashCols =
  let c = TableColumn poolHashTableDef
  in PoolHashCols
       { phcId      = c "id"
       , phcHashRaw = c "hash_raw"
       , phcView    = c "view"
       }

poolHashColsList :: [TableColumn]
poolHashColsList =
  [ poolHashCols.phcId
  , poolHashCols.phcHashRaw
  , poolHashCols.phcView
  ]

-- ---------------------------------------------------------------------------
-- * Per-module column-record registry
-- ---------------------------------------------------------------------------

coreColumnRecords :: [(TableDef, [TableColumn])]
coreColumnRecords =
  [ (blockTableDef,        blockColsList)
  , (txTableDef,           txColsList)
  , (slotLeaderTableDef,   slotLeaderColsList)
  , (stakeAddressTableDef, stakeAddressColsList)
  , (poolHashTableDef,     poolHashColsList)
  ]

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodeBlockCopy :: BlockId -> Block -> ByteString
encodeBlockCopy (BlockId bid) blk =
  buildCopyRow
    [ Just $ bInt64 bid
    , Just $ bHex (blockHash blk)
    , bWord64 <$> blockEpochNo blk
    , bWord64 <$> blockSlotNo blk
    , bWord64 <$> blockEpochSlotNo blk
    , bWord64 <$> blockBlockNo blk
    , bInt64 . getBlockId <$> blockPreviousId blk
    , Just $ bInt64 (getSlotLeaderId $ blockSlotLeaderId blk)
    , Just $ bWord64 (blockSize blk)
    , Just $ bUTCTime (blockTime blk)
    , Just $ bWord64 (blockTxCount blk)
    , Just $ bWord16 (blockProtoMajor blk)
    , Just $ bWord16 (blockProtoMinor blk)
    , bText <$> blockVrfKey blk
    , bHex <$> blockOpCert blk
    , bWord64 <$> blockOpCertCounter blk
    ]

encodeTxCopy :: TxId -> Tx -> ByteString
encodeTxCopy (TxId tid) tx =
  buildCopyRow
    [ Just $ bInt64 tid
    , Just $ bHex (txHash tx)
    , Just $ bInt64 (getBlockId $ txBlockId tx)
    , Just $ bWord64 (txBlockIndex tx)
    , Just $ bWord64 (unDbLovelace $ txOutSum tx)
    , Just $ bWord64 (unDbLovelace $ txFee tx)
    , bInt64 <$> txDeposit tx
    , Just $ bWord64 (txSize tx)
    , bWord64 . unDbWord64 <$> txInvalidBefore tx
    , bWord64 . unDbWord64 <$> txInvalidHereafter tx
    , Just $ bBool (txValidContract tx)
    , Just $ bWord64 (txScriptSize tx)
    , Just $ bWord64 (unDbLovelace $ txTreasuryDonation tx)
    ]

encodeSlotLeaderCopy :: SlotLeaderId -> SlotLeader -> ByteString
encodeSlotLeaderCopy (SlotLeaderId slid) sl =
  buildCopyRow
    [ Just $ bInt64 slid
    , Just $ bHex (slotLeaderHash sl)
    , bInt64 . getPoolHashId <$> slotLeaderPoolHashId sl
    , Just $ bText (slotLeaderDescription sl)
    ]

encodeStakeAddressCopy :: StakeAddressId -> StakeAddress -> ByteString
encodeStakeAddressCopy (StakeAddressId sid) sa =
  buildCopyRow
    [ Just $ bInt64 sid
    , Just $ bHex (stakeAddressHashRaw sa)
    , Just $ bText (stakeAddressView sa)
    , bHex <$> stakeAddressScriptHash sa
    ]

encodePoolHashCopy :: PoolHashId -> PoolHash -> ByteString
encodePoolHashCopy (PoolHashId pid) ph =
  buildCopyRow
    [ Just $ bInt64 pid
    , Just $ bHex (poolHashHashRaw ph)
    , Just $ bText (poolHashView ph)
    ]

-- ---------------------------------------------------------------------------
-- * Internal encoding helpers
-- ---------------------------------------------------------------------------
--
-- The COPY text spellings, in 'ByteString' form. The runtime path uses the
-- 'Builder' versions in 'DbSync.Db.Loader.Encoder'; only the encoder tests
-- call these.

encodeInt64 :: Int64 -> ByteString
encodeInt64 = BS8.pack . show

encodeWord64 :: Word64 -> ByteString
encodeWord64 = BS8.pack . show

-- | @t@ or @f@.
encodeBool :: Bool -> ByteString
encodeBool True  = "t"
encodeBool False = "f"

-- | Hex with the @\\x@ prefix that bytea COPY expects.
encodeHex :: ByteString -> ByteString
encodeHex bs = "\\x" <> toHex bs
  where
    toHex :: ByteString -> ByteString
    toHex = BS8.concatMap (BS8.pack . hexByte)

    hexByte :: Char -> [Char]
    hexByte c =
      let n = fromEnum c
          hi = n `div` 16
          lo = n `mod` 16
      in [hexDigit hi, hexDigit lo]

    hexDigit :: Int -> Char
    hexDigit n
      | n < 10    = toEnum (n + fromEnum '0')
      | otherwise = toEnum (n - 10 + fromEnum 'a')

-- | @YYYY-MM-DD HH:MM:SS@, with no timezone.
encodeUTCTime :: UTCTime -> ByteString
encodeUTCTime = BS8.pack . formatTime defaultTimeLocale "%F %T"

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------
--
-- A @\<row>Encoder@ and @\<row>Decoder@ pair omits the @id@ column. An
-- @entity\<Row>Decoder@ reads @id@ first, so its column order matches
-- @SELECT *@ on the table.

-- | 'UTCTime' \<-> @TIMESTAMP WITHOUT TIME ZONE@. Hasql's 'D.timestamp'
-- and 'E.timestamp' speak 'LocalTime', and the column is naive UTC by
-- convention, so both sides shift through 'utc'.
utcTimeAsTimestampDecoder :: D.Value UTCTime
utcTimeAsTimestampDecoder = localTimeToUTC utc <$> D.timestamp

utcTimeAsTimestampEncoder :: E.Value UTCTime
utcTimeAsTimestampEncoder = utcToLocalTime utc >$< E.timestamp

blockEncoder :: E.Params Block
blockEncoder = mconcat
  [ blockHash           >$< E.param (E.nonNullable E.bytea)
  , blockEpochNo        >$< E.param (E.nullable    $ fromIntegral >$< E.int8)
  , blockSlotNo         >$< E.param (E.nullable    $ fromIntegral >$< E.int8)
  , blockEpochSlotNo    >$< E.param (E.nullable    $ fromIntegral >$< E.int8)
  , blockBlockNo        >$< E.param (E.nullable    $ fromIntegral >$< E.int8)
  , blockPreviousId     >$< maybeIdEncoder getBlockId
  , blockSlotLeaderId   >$< idEncoder      getSlotLeaderId
  , blockSize           >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , blockTime           >$< E.param (E.nonNullable utcTimeAsTimestampEncoder)
  , blockTxCount        >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , blockProtoMajor     >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
  , blockProtoMinor     >$< E.param (E.nonNullable $ fromIntegral >$< E.int2)
  , blockVrfKey         >$< E.param (E.nullable E.text)
  , blockOpCert         >$< E.param (E.nullable E.bytea)
  , blockOpCertCounter  >$< E.param (E.nullable    $ fromIntegral >$< E.int8)
  ]

blockDecoder :: D.Row Block
blockDecoder = Block
  <$> D.column (D.nonNullable D.bytea)
  <*> D.column (D.nullable    $ fromIntegral <$> D.int8)
  <*> D.column (D.nullable    $ fromIntegral <$> D.int8)
  <*> D.column (D.nullable    $ fromIntegral <$> D.int8)
  <*> D.column (D.nullable    $ fromIntegral <$> D.int8)
  <*> maybeIdDecoder BlockId
  <*> idDecoder      SlotLeaderId
  <*> D.column (D.nonNullable $ fromIntegral <$> D.int8)
  <*> D.column (D.nonNullable utcTimeAsTimestampDecoder)
  <*> D.column (D.nonNullable $ fromIntegral <$> D.int8)
  <*> D.column (D.nonNullable $ fromIntegral <$> D.int2)
  <*> D.column (D.nonNullable $ fromIntegral <$> D.int2)
  <*> D.column (D.nullable D.text)
  <*> D.column (D.nullable D.bytea)
  <*> D.column (D.nullable    $ fromIntegral <$> D.int8)

entityBlockDecoder :: D.Row (BlockId, Block)
entityBlockDecoder = (,)
  <$> idDecoder BlockId
  <*> blockDecoder

txEncoder :: E.Params Tx
txEncoder = mconcat
  [ txHash             >$< E.param (E.nonNullable E.bytea)
  , txBlockId          >$< idEncoder      getBlockId
  , txBlockIndex       >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , txOutSum           >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , txFee              >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , txDeposit          >$< E.param (E.nullable E.int8)
  , txSize             >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , txInvalidBefore    >$< maybeDbWord64Encoder
  , txInvalidHereafter >$< maybeDbWord64Encoder
  , txValidContract    >$< E.param (E.nonNullable E.bool)
  , txScriptSize       >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , txTreasuryDonation >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  ]

txDecoder :: D.Row Tx
txDecoder = Tx
  <$> D.column (D.nonNullable D.bytea)
  <*> idDecoder BlockId
  <*> D.column (D.nonNullable $ fromIntegral <$> D.int8)
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> D.column (D.nullable D.int8)
  <*> D.column (D.nonNullable $ fromIntegral <$> D.int8)
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> D.column (D.nonNullable D.bool)
  <*> D.column (D.nonNullable $ fromIntegral <$> D.int8)
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)

entityTxDecoder :: D.Row (TxId, Tx)
entityTxDecoder = (,)
  <$> idDecoder TxId
  <*> txDecoder

slotLeaderEncoder :: E.Params SlotLeader
slotLeaderEncoder = mconcat
  [ slotLeaderHash         >$< E.param (E.nonNullable E.bytea)
  , slotLeaderPoolHashId   >$< maybeIdEncoder getPoolHashId
  , slotLeaderDescription  >$< E.param (E.nonNullable E.text)
  ]

slotLeaderDecoder :: D.Row SlotLeader
slotLeaderDecoder = SlotLeader
  <$> D.column (D.nonNullable D.bytea)
  <*> maybeIdDecoder PoolHashId
  <*> D.column (D.nonNullable D.text)

entitySlotLeaderDecoder :: D.Row (SlotLeaderId, SlotLeader)
entitySlotLeaderDecoder = (,)
  <$> idDecoder SlotLeaderId
  <*> slotLeaderDecoder

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

poolHashEncoder :: E.Params PoolHash
poolHashEncoder = mconcat
  [ poolHashHashRaw >$< E.param (E.nonNullable E.bytea)
  , poolHashView    >$< E.param (E.nonNullable E.text)
  ]

poolHashDecoder :: D.Row PoolHash
poolHashDecoder = PoolHash
  <$> D.column (D.nonNullable D.bytea)
  <*> D.column (D.nonNullable D.text)

entityPoolHashDecoder :: D.Row (PoolHashId, PoolHash)
entityPoolHashDecoder = (,)
  <$> idDecoder PoolHashId
  <*> poolHashDecoder
