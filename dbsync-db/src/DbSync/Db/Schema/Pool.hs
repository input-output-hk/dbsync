{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema types for the Pool extractor tables: pool_hash, pool_update,
-- pool_metadata_ref, pool_owner, pool_retire, pool_relay.
module DbSync.Db.Schema.Pool
  ( -- * Schema types
    PoolHash (..)
  , PoolUpdate (..)
  , PoolMetadataRef (..)
  , PoolOwner (..)
  , PoolRetire (..)
  , PoolRelay (..)

    -- * Table definitions
  , poolHashTableDef
  , poolUpdateTableDef
  , poolMetadataRefTableDef
  , poolOwnerTableDef
  , poolRetireTableDef
  , poolRelayTableDef

    -- * COPY encoding
  , encodePoolHashCopy
  , encodePoolUpdateCopy
  , encodePoolMetadataRefCopy
  , encodePoolOwnerCopy
  , encodePoolRetireCopy
  , encodePoolRelayCopy

    -- * Hasql encoders \/ decoders
  , poolHashEncoder
  , poolHashDecoder
  , entityPoolHashDecoder
  , poolUpdateEncoder
  , poolUpdateDecoder
  , entityPoolUpdateDecoder
  , poolMetadataRefEncoder
  , poolMetadataRefDecoder
  , entityPoolMetadataRefDecoder
  , poolOwnerEncoder
  , poolOwnerDecoder
  , entityPoolOwnerDecoder
  , poolRetireEncoder
  , poolRetireDecoder
  , entityPoolRetireDecoder
  , poolRelayEncoder
  , poolRelayDecoder
  , entityPoolRelayDecoder
  ) where

import Cardano.Prelude

import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Text.Read as TR

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

type instance Key PoolHash = PoolHashId
type instance Key PoolUpdate = PoolUpdateId
type instance Key PoolMetadataRef = PoolMetadataRefId
type instance Key PoolOwner = PoolOwnerId
type instance Key PoolRetire = PoolRetireId
type instance Key PoolRelay = PoolRelayId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | The @pool_hash@ table (dedup table).
-- One row per unique pool key hash.
data PoolHash = PoolHash
  { poolHashHashRaw :: !ByteString  -- ^ Pool key hash (28 bytes)
  , poolHashView    :: !Text        -- ^ Bech32 representation
  }
  deriving stock (Eq, Show)

-- | The @pool_update@ table.
data PoolUpdate = PoolUpdate
  { poolUpdateHashId        :: !PoolHashId
  , poolUpdateCertIndex     :: !Word16
  , poolUpdateVrfKeyHash    :: !ByteString    -- ^ VRF verification key hash (32 bytes)
  , poolUpdatePledge        :: !DbLovelace
  , poolUpdateActiveEpochNo :: !Word64
  , poolUpdateMetaId        :: !(Maybe PoolMetadataRefId)
  , poolUpdateMargin        :: !Double
  , poolUpdateFixedCost     :: !DbLovelace
  , poolUpdateRegisteredTxId :: !TxId
  , poolUpdateRewardAddrId  :: !StakeAddressId
  , poolUpdateDeposit       :: !(Maybe DbLovelace)
  }
  deriving stock (Eq, Show)

-- | The @pool_metadata_ref@ table.
data PoolMetadataRef = PoolMetadataRef
  { poolMetadataRefPoolId        :: !PoolHashId
  , poolMetadataRefUrl           :: !Text
  , poolMetadataRefHash          :: !ByteString
  , poolMetadataRefRegisteredTxId :: !TxId
  }
  deriving stock (Eq, Show)

-- | The @pool_owner@ table.
data PoolOwner = PoolOwner
  { poolOwnerAddrId       :: !StakeAddressId
  , poolOwnerPoolUpdateId :: !PoolUpdateId
  }
  deriving stock (Eq, Show)

-- | The @pool_retire@ table.
data PoolRetire = PoolRetire
  { poolRetireHashId        :: !PoolHashId
  , poolRetireCertIndex     :: !Word16
  , poolRetireAnnouncedTxId :: !TxId
  , poolRetireRetiringEpoch :: !Word64
  }
  deriving stock (Eq, Show)

-- | The @pool_relay@ table.
data PoolRelay = PoolRelay
  { poolRelayUpdateId   :: !PoolUpdateId
  , poolRelayIpv4       :: !(Maybe Text)
  , poolRelayIpv6       :: !(Maybe Text)
  , poolRelayDnsName    :: !(Maybe Text)
  , poolRelayDnsSrvName :: !(Maybe Text)
  , poolRelayPort       :: !(Maybe Word16)
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definitions
-- ---------------------------------------------------------------------------

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
  }

poolUpdateTableDef :: TableDef
poolUpdateTableDef = TableDef
  { tdName    = "pool_update"
  , tdColumns =
      [ ColumnDef "id"               PgBigInt   False
      , ColumnDef "hash_id"          PgBigInt   False
      , ColumnDef "cert_index"       PgBigInt   False
      , ColumnDef "vrf_key_hash"     PgBytea    False
      , ColumnDef "pledge"           PgNumeric  False
      , ColumnDef "active_epoch_no"  PgBigInt   False
      , ColumnDef "meta_id"          PgBigInt   True
      , ColumnDef "margin"           PgText     False
      , ColumnDef "fixed_cost"       PgNumeric  False
      , ColumnDef "registered_tx_id" PgBigInt   False
      , ColumnDef "reward_addr_id"   PgBigInt   False
      , ColumnDef "deposit"          PgNumeric  True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  }

poolMetadataRefTableDef :: TableDef
poolMetadataRefTableDef = TableDef
  { tdName    = "pool_metadata_ref"
  , tdColumns =
      [ ColumnDef "id"               PgBigInt  False
      , ColumnDef "pool_id"          PgBigInt  False
      , ColumnDef "url"              PgText    False
      , ColumnDef "hash"             PgBytea   False
      , ColumnDef "registered_tx_id" PgBigInt  False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  }

poolOwnerTableDef :: TableDef
poolOwnerTableDef = TableDef
  { tdName    = "pool_owner"
  , tdColumns =
      [ ColumnDef "id"             PgBigInt  False
      , ColumnDef "addr_id"        PgBigInt  False
      , ColumnDef "pool_update_id" PgBigInt  False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  }

poolRetireTableDef :: TableDef
poolRetireTableDef = TableDef
  { tdName    = "pool_retire"
  , tdColumns =
      [ ColumnDef "id"              PgBigInt  False
      , ColumnDef "hash_id"         PgBigInt  False
      , ColumnDef "cert_index"      PgBigInt  False
      , ColumnDef "announced_tx_id" PgBigInt  False
      , ColumnDef "retiring_epoch"  PgBigInt  False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  }

poolRelayTableDef :: TableDef
poolRelayTableDef = TableDef
  { tdName    = "pool_relay"
  , tdColumns =
      [ ColumnDef "id"           PgBigInt  False
      , ColumnDef "update_id"    PgBigInt  False
      , ColumnDef "ipv4"         PgText    True
      , ColumnDef "ipv6"         PgText    True
      , ColumnDef "dns_name"     PgText    True
      , ColumnDef "dns_srv_name" PgText    True
      , ColumnDef "port"         PgBigInt  True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  }

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodePoolHashCopy :: PoolHashId -> PoolHash -> ByteString
encodePoolHashCopy (PoolHashId pid) ph =
  buildCopyRow
    [ Just $ bInt64 pid
    , Just $ bHex (poolHashHashRaw ph)
    , Just $ bText (poolHashView ph)
    ]

encodePoolUpdateCopy :: PoolUpdateId -> PoolUpdate -> ByteString
encodePoolUpdateCopy (PoolUpdateId puid) pu =
  buildCopyRow
    [ Just $ bInt64 puid
    , Just $ bInt64 (getPoolHashId $ poolUpdateHashId pu)
    , Just $ bInt64 (fromIntegral $ poolUpdateCertIndex pu)
    , Just $ bHex (poolUpdateVrfKeyHash pu)
    , Just $ bWord64 (unDbLovelace $ poolUpdatePledge pu)
    , Just $ bWord64 (poolUpdateActiveEpochNo pu)
    , bInt64 . getPoolMetadataRefId <$> poolUpdateMetaId pu
    , Just $ bDouble (poolUpdateMargin pu)
    , Just $ bWord64 (unDbLovelace $ poolUpdateFixedCost pu)
    , Just $ bInt64 (getTxId $ poolUpdateRegisteredTxId pu)
    , Just $ bInt64 (getStakeAddressId $ poolUpdateRewardAddrId pu)
    , bWord64 . unDbLovelace <$> poolUpdateDeposit pu
    ]

encodePoolMetadataRefCopy :: PoolMetadataRefId -> PoolMetadataRef -> ByteString
encodePoolMetadataRefCopy (PoolMetadataRefId pmid) pm =
  buildCopyRow
    [ Just $ bInt64 pmid
    , Just $ bInt64 (getPoolHashId $ poolMetadataRefPoolId pm)
    , Just $ bText (poolMetadataRefUrl pm)
    , Just $ bHex (poolMetadataRefHash pm)
    , Just $ bInt64 (getTxId $ poolMetadataRefRegisteredTxId pm)
    ]

encodePoolOwnerCopy :: PoolOwnerId -> PoolOwner -> ByteString
encodePoolOwnerCopy (PoolOwnerId poid) po =
  buildCopyRow
    [ Just $ bInt64 poid
    , Just $ bInt64 (getStakeAddressId $ poolOwnerAddrId po)
    , Just $ bInt64 (getPoolUpdateId $ poolOwnerPoolUpdateId po)
    ]

encodePoolRetireCopy :: PoolRetireId -> PoolRetire -> ByteString
encodePoolRetireCopy (PoolRetireId prid) pr =
  buildCopyRow
    [ Just $ bInt64 prid
    , Just $ bInt64 (getPoolHashId $ poolRetireHashId pr)
    , Just $ bInt64 (fromIntegral $ poolRetireCertIndex pr)
    , Just $ bInt64 (getTxId $ poolRetireAnnouncedTxId pr)
    , Just $ bWord64 (poolRetireRetiringEpoch pr)
    ]

encodePoolRelayCopy :: PoolRelayId -> PoolRelay -> ByteString
encodePoolRelayCopy (PoolRelayId prid) pr =
  buildCopyRow
    [ Just $ bInt64 prid
    , Just $ bInt64 (getPoolUpdateId $ poolRelayUpdateId pr)
    , bText <$> poolRelayIpv4 pr
    , bText <$> poolRelayIpv6 pr
    , bText <$> poolRelayDnsName pr
    , bText <$> poolRelayDnsSrvName pr
    , bInt64 . fromIntegral <$> poolRelayPort pr
    ]

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------

-- | The original schema stores @pool_update.margin@ as @text@ holding
-- a Haskell 'show' of the rational margin (e.g. @"5.0e-2"@). We
-- mirror that representation: encode via 'show', decode via
-- 'TR.readMaybe' wrapped in 'D.refine' so a malformed row surfaces
-- as a parse error rather than a runtime panic.
doubleAsTextEncoder :: E.Value Double
doubleAsTextEncoder = T.pack . show >$< E.text

doubleAsTextDecoder :: D.Value Double
doubleAsTextDecoder = D.refine parseDouble D.text
  where
    parseDouble :: Text -> Either Text Double
    parseDouble t = case TR.readMaybe (T.unpack t) of
      Just d  -> Right d
      Nothing -> Left ("pool_update.margin: not a Double: " <> t)

-- PoolHash -----------------------------------------------------------------

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

-- PoolUpdate ---------------------------------------------------------------

poolUpdateEncoder :: E.Params PoolUpdate
poolUpdateEncoder = mconcat
  [ poolUpdateHashId        >$< idEncoder getPoolHashId
  , (fromIntegral :: Word16 -> Int64) . poolUpdateCertIndex
                            >$< E.param (E.nonNullable E.int8)
  , poolUpdateVrfKeyHash    >$< E.param (E.nonNullable E.bytea)
  , poolUpdatePledge        >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , poolUpdateActiveEpochNo >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , poolUpdateMetaId        >$< maybeIdEncoder getPoolMetadataRefId
  , poolUpdateMargin        >$< E.param (E.nonNullable doubleAsTextEncoder)
  , poolUpdateFixedCost     >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , poolUpdateRegisteredTxId >$< idEncoder getTxId
  , poolUpdateRewardAddrId  >$< idEncoder getStakeAddressId
  , poolUpdateDeposit       >$< maybeDbLovelaceEncoder
  ]

poolUpdateDecoder :: D.Row PoolUpdate
poolUpdateDecoder = PoolUpdate
  <$> idDecoder PoolHashId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> D.column (D.nonNullable D.bytea)
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> maybeIdDecoder PoolMetadataRefId
  <*> D.column (D.nonNullable doubleAsTextDecoder)
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> idDecoder TxId
  <*> idDecoder StakeAddressId
  <*> maybeDbLovelaceDecoder

entityPoolUpdateDecoder :: D.Row (PoolUpdateId, PoolUpdate)
entityPoolUpdateDecoder = (,)
  <$> idDecoder PoolUpdateId
  <*> poolUpdateDecoder

-- PoolMetadataRef ----------------------------------------------------------

poolMetadataRefEncoder :: E.Params PoolMetadataRef
poolMetadataRefEncoder = mconcat
  [ poolMetadataRefPoolId         >$< idEncoder getPoolHashId
  , poolMetadataRefUrl            >$< E.param (E.nonNullable E.text)
  , poolMetadataRefHash           >$< E.param (E.nonNullable E.bytea)
  , poolMetadataRefRegisteredTxId >$< idEncoder getTxId
  ]

poolMetadataRefDecoder :: D.Row PoolMetadataRef
poolMetadataRefDecoder = PoolMetadataRef
  <$> idDecoder PoolHashId
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable D.bytea)
  <*> idDecoder TxId

entityPoolMetadataRefDecoder :: D.Row (PoolMetadataRefId, PoolMetadataRef)
entityPoolMetadataRefDecoder = (,)
  <$> idDecoder PoolMetadataRefId
  <*> poolMetadataRefDecoder

-- PoolOwner ----------------------------------------------------------------

poolOwnerEncoder :: E.Params PoolOwner
poolOwnerEncoder = mconcat
  [ poolOwnerAddrId       >$< idEncoder getStakeAddressId
  , poolOwnerPoolUpdateId >$< idEncoder getPoolUpdateId
  ]

poolOwnerDecoder :: D.Row PoolOwner
poolOwnerDecoder = PoolOwner
  <$> idDecoder StakeAddressId
  <*> idDecoder PoolUpdateId

entityPoolOwnerDecoder :: D.Row (PoolOwnerId, PoolOwner)
entityPoolOwnerDecoder = (,)
  <$> idDecoder PoolOwnerId
  <*> poolOwnerDecoder

-- PoolRetire ---------------------------------------------------------------

poolRetireEncoder :: E.Params PoolRetire
poolRetireEncoder = mconcat
  [ poolRetireHashId        >$< idEncoder getPoolHashId
  , (fromIntegral :: Word16 -> Int64) . poolRetireCertIndex
                            >$< E.param (E.nonNullable E.int8)
  , poolRetireAnnouncedTxId >$< idEncoder getTxId
  , poolRetireRetiringEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  ]

poolRetireDecoder :: D.Row PoolRetire
poolRetireDecoder = PoolRetire
  <$> idDecoder PoolHashId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> idDecoder TxId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))

entityPoolRetireDecoder :: D.Row (PoolRetireId, PoolRetire)
entityPoolRetireDecoder = (,)
  <$> idDecoder PoolRetireId
  <*> poolRetireDecoder

-- PoolRelay ----------------------------------------------------------------

poolRelayEncoder :: E.Params PoolRelay
poolRelayEncoder = mconcat
  [ poolRelayUpdateId   >$< idEncoder getPoolUpdateId
  , poolRelayIpv4       >$< E.param (E.nullable E.text)
  , poolRelayIpv6       >$< E.param (E.nullable E.text)
  , poolRelayDnsName    >$< E.param (E.nullable E.text)
  , poolRelayDnsSrvName >$< E.param (E.nullable E.text)
  , (fmap (fromIntegral :: Word16 -> Int64) . poolRelayPort)
                        >$< E.param (E.nullable E.int8)
  ]

poolRelayDecoder :: D.Row PoolRelay
poolRelayDecoder = PoolRelay
  <$> idDecoder PoolUpdateId
  <*> D.column (D.nullable D.text)
  <*> D.column (D.nullable D.text)
  <*> D.column (D.nullable D.text)
  <*> D.column (D.nullable D.text)
  <*> (fmap fromIntegral <$> D.column (D.nullable D.int8))

entityPoolRelayDecoder :: D.Row (PoolRelayId, PoolRelay)
entityPoolRelayDecoder = (,)
  <$> idDecoder PoolRelayId
  <*> poolRelayDecoder

-- ---------------------------------------------------------------------------
-- * Internal helpers
-- ---------------------------------------------------------------------------

-- | Encode a 'Double' as decimal ASCII into a 'Builder'.
bDouble :: Double -> Builder
bDouble = byteString . BS8.pack . show
