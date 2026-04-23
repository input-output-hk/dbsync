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
  ) where

import Cardano.Prelude

import qualified Data.ByteString.Char8 as BS8

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
  }

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodePoolHashCopy :: PoolHashId -> PoolHash -> ByteString
encodePoolHashCopy (PoolHashId pid) ph =
  encodeToCopyRow
    [ Just $ encodeInt64 pid
    , Just $ encodeHex (poolHashHashRaw ph)
    , Just $ TE.encodeUtf8 (poolHashView ph)
    ]

encodePoolUpdateCopy :: PoolUpdateId -> PoolUpdate -> ByteString
encodePoolUpdateCopy (PoolUpdateId puid) pu =
  encodeToCopyRow
    [ Just $ encodeInt64 puid
    , Just $ encodeInt64 (getPoolHashId $ poolUpdateHashId pu)
    , Just $ encodeInt64 (fromIntegral $ poolUpdateCertIndex pu)
    , Just $ encodeHex (poolUpdateVrfKeyHash pu)
    , Just $ encodeWord64 (unDbLovelace $ poolUpdatePledge pu)
    , Just $ encodeWord64 (poolUpdateActiveEpochNo pu)
    , encodeInt64 . getPoolMetadataRefId <$> poolUpdateMetaId pu
    , Just $ encodeDouble (poolUpdateMargin pu)
    , Just $ encodeWord64 (unDbLovelace $ poolUpdateFixedCost pu)
    , Just $ encodeInt64 (getTxId $ poolUpdateRegisteredTxId pu)
    , Just $ encodeInt64 (getStakeAddressId $ poolUpdateRewardAddrId pu)
    , encodeWord64 . unDbLovelace <$> poolUpdateDeposit pu
    ]

encodePoolMetadataRefCopy :: PoolMetadataRefId -> PoolMetadataRef -> ByteString
encodePoolMetadataRefCopy (PoolMetadataRefId pmid) pm =
  encodeToCopyRow
    [ Just $ encodeInt64 pmid
    , Just $ encodeInt64 (getPoolHashId $ poolMetadataRefPoolId pm)
    , Just $ TE.encodeUtf8 (poolMetadataRefUrl pm)
    , Just $ encodeHex (poolMetadataRefHash pm)
    , Just $ encodeInt64 (getTxId $ poolMetadataRefRegisteredTxId pm)
    ]

encodePoolOwnerCopy :: PoolOwnerId -> PoolOwner -> ByteString
encodePoolOwnerCopy (PoolOwnerId poid) po =
  encodeToCopyRow
    [ Just $ encodeInt64 poid
    , Just $ encodeInt64 (getStakeAddressId $ poolOwnerAddrId po)
    , Just $ encodeInt64 (getPoolUpdateId $ poolOwnerPoolUpdateId po)
    ]

encodePoolRetireCopy :: PoolRetireId -> PoolRetire -> ByteString
encodePoolRetireCopy (PoolRetireId prid) pr =
  encodeToCopyRow
    [ Just $ encodeInt64 prid
    , Just $ encodeInt64 (getPoolHashId $ poolRetireHashId pr)
    , Just $ encodeInt64 (fromIntegral $ poolRetireCertIndex pr)
    , Just $ encodeInt64 (getTxId $ poolRetireAnnouncedTxId pr)
    , Just $ encodeWord64 (poolRetireRetiringEpoch pr)
    ]

encodePoolRelayCopy :: PoolRelayId -> PoolRelay -> ByteString
encodePoolRelayCopy (PoolRelayId prid) pr =
  encodeToCopyRow
    [ Just $ encodeInt64 prid
    , Just $ encodeInt64 (getPoolUpdateId $ poolRelayUpdateId pr)
    , TE.encodeUtf8 <$> poolRelayIpv4 pr
    , TE.encodeUtf8 <$> poolRelayIpv6 pr
    , TE.encodeUtf8 <$> poolRelayDnsName pr
    , TE.encodeUtf8 <$> poolRelayDnsSrvName pr
    , encodeInt64 . fromIntegral <$> poolRelayPort pr
    ]

-- ---------------------------------------------------------------------------
-- * Internal helpers
-- ---------------------------------------------------------------------------

-- | Encode a 'Double' as a decimal ASCII ByteString.
encodeDouble :: Double -> ByteString
encodeDouble = BS8.pack . show
