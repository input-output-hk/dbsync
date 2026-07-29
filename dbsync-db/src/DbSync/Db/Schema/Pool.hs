{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema types for pool-related tables.
--
-- Tables in this module are owned by three different extractors:
--
--   * @pool@ extractor (block-extracted): @pool_update@,
--     @pool_metadata_ref@, @pool_owner@, @pool_retire@, @pool_relay@.
--   * @pool_stats@ extractor (ledger-derived, epoch-boundary): @pool_stat@.
--   * @off_chain_pools@ extractor (operator-managed via SMASH):
--     @delisted_pool@, @reserved_pool_ticker@.
module DbSync.Db.Schema.Pool
  ( -- * Schema types
    PoolUpdate (..)
  , PoolMetadataRef (..)
  , PoolOwner (..)
  , PoolRetire (..)
  , PoolRelay (..)
  , PoolStat (..)
  , DelistedPool (..)
  , ReservedPoolTicker (..)

    -- * Table definitions
  , poolUpdateTableDef
  , poolMetadataRefTableDef
  , poolOwnerTableDef
  , poolRetireTableDef
  , poolRelayTableDef
  , poolStatTableDef
  , delistedPoolTableDef
  , reservedPoolTickerTableDef

    -- * Column records (compile-time-safe column references)
  , PoolUpdateCols (..), poolUpdateCols, poolUpdateColsList
  , PoolMetadataRefCols (..), poolMetadataRefCols, poolMetadataRefColsList
  , PoolOwnerCols (..), poolOwnerCols, poolOwnerColsList
  , PoolRetireCols (..), poolRetireCols, poolRetireColsList
  , PoolRelayCols (..), poolRelayCols, poolRelayColsList
  , PoolStatCols (..), poolStatCols, poolStatColsList
  , DelistedPoolCols (..), delistedPoolCols, delistedPoolColsList
  , ReservedPoolTickerCols (..), reservedPoolTickerCols, reservedPoolTickerColsList

    -- * Per-module column-record registry
  , poolColumnRecords

    -- * COPY encoding
  , encodePoolUpdateCopy
  , encodePoolMetadataRefCopy
  , encodePoolOwnerCopy
  , encodePoolRetireCopy
  , encodePoolRelayCopy
  , encodePoolStatCopy
  , encodeDelistedPoolCopy
  , encodeReservedPoolTickerCopy

    -- * Hasql encoders \/ decoders
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
  , poolStatEncoder
  , poolStatDecoder
  , entityPoolStatDecoder
  , delistedPoolEncoder
  , delistedPoolDecoder
  , entityDelistedPoolDecoder
  , reservedPoolTickerEncoder
  , reservedPoolTickerDecoder
  , entityReservedPoolTickerDecoder
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
  , DbWord64 (..)
  , bRational
  , dbLovelaceValueDecoder
  , dbLovelaceValueEncoder
  , dbWord64ValueDecoder
  , dbWord64ValueEncoder
  , maybeDbLovelaceDecoder
  , maybeDbLovelaceEncoder
  , rationalAsNumericDecoder
  , rationalAsNumericEncoder
  )
import DbSync.Db.Loader.Encoder (buildCopyRow, bHex, bInt64, bText, bWord64)

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key PoolUpdate = PoolUpdateId
type instance Key PoolMetadataRef = PoolMetadataRefId
type instance Key PoolOwner = PoolOwnerId
type instance Key PoolRetire = PoolRetireId
type instance Key PoolRelay = PoolRelayId
type instance Key PoolStat = PoolStatId
type instance Key DelistedPool = DelistedPoolId
type instance Key ReservedPoolTicker = ReservedPoolTickerId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | The @pool_update@ table.
data PoolUpdate = PoolUpdate
  { poolUpdateHashId        :: !PoolHashId
  , poolUpdateCertIndex     :: !Word16
  , poolUpdateVrfKeyHash    :: !ByteString    -- ^ VRF verification key hash (32 bytes)
  , poolUpdatePledge        :: !DbLovelace
  , poolUpdateActiveEpochNo :: !Word64
  , poolUpdateMetaId        :: !(Maybe PoolMetadataRefId)
  , poolUpdateMargin        :: !Rational
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

-- | The @pool_stat@ table.
-- One row per (pool, epoch); written by the @pool_stats@ extractor
-- from ledger state at each epoch boundary.
data PoolStat = PoolStat
  { poolStatPoolHashId         :: !PoolHashId
  , poolStatEpochNo            :: !Word64
  , poolStatNumberOfBlocks     :: !DbWord64
  , poolStatNumberOfDelegators :: !DbWord64
  , poolStatStake              :: !DbWord64
  , poolStatVotingPower        :: !(Maybe DbWord64)
  }
  deriving stock (Eq, Show)

-- | The @delisted_pool@ table. Single column, unique on @hash_raw@.
-- Maintained by SMASH; @off_chain_pools@ feature.
newtype DelistedPool = DelistedPool
  { delistedPoolHashRaw :: ByteString
  }
  deriving stock (Eq, Show)

-- | The @reserved_pool_ticker@ table. Unique on @name@.
-- Maintained by SMASH; @off_chain_pools@ feature.
data ReservedPoolTicker = ReservedPoolTicker
  { reservedPoolTickerName     :: !Text
  , reservedPoolTickerPoolHash :: !ByteString
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definitions
-- ---------------------------------------------------------------------------

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
      , ColumnDef "margin"           PgNumeric  False
      , ColumnDef "fixed_cost"       PgNumeric  False
      , ColumnDef "registered_tx_id" PgBigInt   False
      , ColumnDef "reward_addr_id"   PgBigInt   False
      , ColumnDef "deposit"          PgNumeric  True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = []
  , tdForeignKeys =
      [ ForeignKey "registered_tx_id" "tx" "id"
      ]
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
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = []
  , tdForeignKeys =
      [ ForeignKey "registered_tx_id" "tx" "id"
      ]
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
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys =
      [ ForeignKey "pool_update_id" "pool_update" "id"
      ]
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
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys =
      [ ForeignKey "announced_tx_id" "tx" "id"
      ]
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
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys =
      [ ForeignKey "update_id" "pool_update" "id"
      ]
  }

poolStatTableDef :: TableDef
poolStatTableDef = TableDef
  { tdName    = "pool_stat"
  , tdColumns =
      [ ColumnDef "id"                   PgBigInt  False
      , ColumnDef "pool_hash_id"         PgBigInt  False
      , ColumnDef "epoch_no"             PgBigInt  False
      , ColumnDef "number_of_blocks"     PgNumeric False
      , ColumnDef "number_of_delegators" PgNumeric False
      , ColumnDef "stake"                PgNumeric False
      , ColumnDef "voting_power"         PgNumeric True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
    -- Natural key: one row per pool per epoch.
  , tdUniqueConstraints = ["pool_hash_id" :| ["epoch_no"]]
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys = []
  }

delistedPoolTableDef :: TableDef
delistedPoolTableDef = TableDef
  { tdName    = "delisted_pool"
  , tdColumns =
      [ ColumnDef "id"       PgBigInt False
      , ColumnDef "hash_raw" PgBytea  False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = [pure "hash_raw"]
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys = []
  }

reservedPoolTickerTableDef :: TableDef
reservedPoolTickerTableDef = TableDef
  { tdName    = "reserved_pool_ticker"
  , tdColumns =
      [ ColumnDef "id"        PgBigInt False
      , ColumnDef "name"      PgText   False
      , ColumnDef "pool_hash" PgBytea  False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = [pure "name"]
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys = []
  }

-- ---------------------------------------------------------------------------
-- * Column records
-- ---------------------------------------------------------------------------

data PoolUpdateCols = PoolUpdateCols
  { pucId             :: !TableColumn
  , pucHashId         :: !TableColumn
  , pucCertIndex      :: !TableColumn
  , pucVrfKeyHash     :: !TableColumn
  , pucPledge         :: !TableColumn
  , pucActiveEpochNo  :: !TableColumn
  , pucMetaId         :: !TableColumn
  , pucMargin         :: !TableColumn
  , pucFixedCost      :: !TableColumn
  , pucRegisteredTxId :: !TableColumn
  , pucRewardAddrId   :: !TableColumn
  , pucDeposit        :: !TableColumn
  }

poolUpdateCols :: PoolUpdateCols
poolUpdateCols =
  let c = TableColumn poolUpdateTableDef
  in PoolUpdateCols
       { pucId             = c "id"
       , pucHashId         = c "hash_id"
       , pucCertIndex      = c "cert_index"
       , pucVrfKeyHash     = c "vrf_key_hash"
       , pucPledge         = c "pledge"
       , pucActiveEpochNo  = c "active_epoch_no"
       , pucMetaId         = c "meta_id"
       , pucMargin         = c "margin"
       , pucFixedCost      = c "fixed_cost"
       , pucRegisteredTxId = c "registered_tx_id"
       , pucRewardAddrId   = c "reward_addr_id"
       , pucDeposit        = c "deposit"
       }

poolUpdateColsList :: [TableColumn]
poolUpdateColsList =
  [ poolUpdateCols.pucId
  , poolUpdateCols.pucHashId
  , poolUpdateCols.pucCertIndex
  , poolUpdateCols.pucVrfKeyHash
  , poolUpdateCols.pucPledge
  , poolUpdateCols.pucActiveEpochNo
  , poolUpdateCols.pucMetaId
  , poolUpdateCols.pucMargin
  , poolUpdateCols.pucFixedCost
  , poolUpdateCols.pucRegisteredTxId
  , poolUpdateCols.pucRewardAddrId
  , poolUpdateCols.pucDeposit
  ]

data PoolMetadataRefCols = PoolMetadataRefCols
  { pmrcId             :: !TableColumn
  , pmrcPoolId         :: !TableColumn
  , pmrcUrl            :: !TableColumn
  , pmrcHash           :: !TableColumn
  , pmrcRegisteredTxId :: !TableColumn
  }

poolMetadataRefCols :: PoolMetadataRefCols
poolMetadataRefCols =
  let c = TableColumn poolMetadataRefTableDef
  in PoolMetadataRefCols
       { pmrcId             = c "id"
       , pmrcPoolId         = c "pool_id"
       , pmrcUrl            = c "url"
       , pmrcHash           = c "hash"
       , pmrcRegisteredTxId = c "registered_tx_id"
       }

poolMetadataRefColsList :: [TableColumn]
poolMetadataRefColsList =
  [ poolMetadataRefCols.pmrcId
  , poolMetadataRefCols.pmrcPoolId
  , poolMetadataRefCols.pmrcUrl
  , poolMetadataRefCols.pmrcHash
  , poolMetadataRefCols.pmrcRegisteredTxId
  ]

data PoolOwnerCols = PoolOwnerCols
  { pocId           :: !TableColumn
  , pocAddrId       :: !TableColumn
  , pocPoolUpdateId :: !TableColumn
  }

poolOwnerCols :: PoolOwnerCols
poolOwnerCols =
  let c = TableColumn poolOwnerTableDef
  in PoolOwnerCols
       { pocId           = c "id"
       , pocAddrId       = c "addr_id"
       , pocPoolUpdateId = c "pool_update_id"
       }

poolOwnerColsList :: [TableColumn]
poolOwnerColsList =
  [ poolOwnerCols.pocId
  , poolOwnerCols.pocAddrId
  , poolOwnerCols.pocPoolUpdateId
  ]

data PoolRetireCols = PoolRetireCols
  { prcId            :: !TableColumn
  , prcHashId        :: !TableColumn
  , prcCertIndex     :: !TableColumn
  , prcAnnouncedTxId :: !TableColumn
  , prcRetiringEpoch :: !TableColumn
  }

poolRetireCols :: PoolRetireCols
poolRetireCols =
  let c = TableColumn poolRetireTableDef
  in PoolRetireCols
       { prcId            = c "id"
       , prcHashId        = c "hash_id"
       , prcCertIndex     = c "cert_index"
       , prcAnnouncedTxId = c "announced_tx_id"
       , prcRetiringEpoch = c "retiring_epoch"
       }

poolRetireColsList :: [TableColumn]
poolRetireColsList =
  [ poolRetireCols.prcId
  , poolRetireCols.prcHashId
  , poolRetireCols.prcCertIndex
  , poolRetireCols.prcAnnouncedTxId
  , poolRetireCols.prcRetiringEpoch
  ]

data PoolRelayCols = PoolRelayCols
  { prlcId         :: !TableColumn
  , prlcUpdateId   :: !TableColumn
  , prlcIpv4       :: !TableColumn
  , prlcIpv6       :: !TableColumn
  , prlcDnsName    :: !TableColumn
  , prlcDnsSrvName :: !TableColumn
  , prlcPort       :: !TableColumn
  }

poolRelayCols :: PoolRelayCols
poolRelayCols =
  let c = TableColumn poolRelayTableDef
  in PoolRelayCols
       { prlcId         = c "id"
       , prlcUpdateId   = c "update_id"
       , prlcIpv4       = c "ipv4"
       , prlcIpv6       = c "ipv6"
       , prlcDnsName    = c "dns_name"
       , prlcDnsSrvName = c "dns_srv_name"
       , prlcPort       = c "port"
       }

poolRelayColsList :: [TableColumn]
poolRelayColsList =
  [ poolRelayCols.prlcId
  , poolRelayCols.prlcUpdateId
  , poolRelayCols.prlcIpv4
  , poolRelayCols.prlcIpv6
  , poolRelayCols.prlcDnsName
  , poolRelayCols.prlcDnsSrvName
  , poolRelayCols.prlcPort
  ]

data PoolStatCols = PoolStatCols
  { pstcId                 :: !TableColumn
  , pstcPoolHashId         :: !TableColumn
  , pstcEpochNo            :: !TableColumn
  , pstcNumberOfBlocks     :: !TableColumn
  , pstcNumberOfDelegators :: !TableColumn
  , pstcStake              :: !TableColumn
  , pstcVotingPower        :: !TableColumn
  }

poolStatCols :: PoolStatCols
poolStatCols =
  let c = TableColumn poolStatTableDef
  in PoolStatCols
       { pstcId                 = c "id"
       , pstcPoolHashId         = c "pool_hash_id"
       , pstcEpochNo            = c "epoch_no"
       , pstcNumberOfBlocks     = c "number_of_blocks"
       , pstcNumberOfDelegators = c "number_of_delegators"
       , pstcStake              = c "stake"
       , pstcVotingPower        = c "voting_power"
       }

poolStatColsList :: [TableColumn]
poolStatColsList =
  [ poolStatCols.pstcId
  , poolStatCols.pstcPoolHashId
  , poolStatCols.pstcEpochNo
  , poolStatCols.pstcNumberOfBlocks
  , poolStatCols.pstcNumberOfDelegators
  , poolStatCols.pstcStake
  , poolStatCols.pstcVotingPower
  ]

data DelistedPoolCols = DelistedPoolCols
  { dpcId      :: !TableColumn
  , dpcHashRaw :: !TableColumn
  }

delistedPoolCols :: DelistedPoolCols
delistedPoolCols =
  let c = TableColumn delistedPoolTableDef
  in DelistedPoolCols
       { dpcId      = c "id"
       , dpcHashRaw = c "hash_raw"
       }

delistedPoolColsList :: [TableColumn]
delistedPoolColsList =
  [ delistedPoolCols.dpcId
  , delistedPoolCols.dpcHashRaw
  ]

data ReservedPoolTickerCols = ReservedPoolTickerCols
  { rptcId       :: !TableColumn
  , rptcName     :: !TableColumn
  , rptcPoolHash :: !TableColumn
  }

reservedPoolTickerCols :: ReservedPoolTickerCols
reservedPoolTickerCols =
  let c = TableColumn reservedPoolTickerTableDef
  in ReservedPoolTickerCols
       { rptcId       = c "id"
       , rptcName     = c "name"
       , rptcPoolHash = c "pool_hash"
       }

reservedPoolTickerColsList :: [TableColumn]
reservedPoolTickerColsList =
  [ reservedPoolTickerCols.rptcId
  , reservedPoolTickerCols.rptcName
  , reservedPoolTickerCols.rptcPoolHash
  ]

-- ---------------------------------------------------------------------------
-- * Per-module column-record registry
-- ---------------------------------------------------------------------------

poolColumnRecords :: [(TableDef, [TableColumn])]
poolColumnRecords =
  [ (poolUpdateTableDef,         poolUpdateColsList)
  , (poolMetadataRefTableDef,    poolMetadataRefColsList)
  , (poolOwnerTableDef,          poolOwnerColsList)
  , (poolRetireTableDef,         poolRetireColsList)
  , (poolRelayTableDef,          poolRelayColsList)
  , (poolStatTableDef,           poolStatColsList)
  , (delistedPoolTableDef,       delistedPoolColsList)
  , (reservedPoolTickerTableDef, reservedPoolTickerColsList)
  ]

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

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
    , Just $ bRational (poolUpdateMargin pu)
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

encodePoolOwnerCopy :: PoolOwner -> ByteString
encodePoolOwnerCopy po =
  buildCopyRow
    [ Just $ bInt64 (getStakeAddressId $ poolOwnerAddrId po)
    , Just $ bInt64 (getPoolUpdateId $ poolOwnerPoolUpdateId po)
    ]

encodePoolRetireCopy :: PoolRetire -> ByteString
encodePoolRetireCopy pr =
  buildCopyRow
    [ Just $ bInt64 (getPoolHashId $ poolRetireHashId pr)
    , Just $ bInt64 (fromIntegral $ poolRetireCertIndex pr)
    , Just $ bInt64 (getTxId $ poolRetireAnnouncedTxId pr)
    , Just $ bWord64 (poolRetireRetiringEpoch pr)
    ]

encodePoolRelayCopy :: PoolRelay -> ByteString
encodePoolRelayCopy pr =
  buildCopyRow
    [ Just $ bInt64 (getPoolUpdateId $ poolRelayUpdateId pr)
    , bText <$> poolRelayIpv4 pr
    , bText <$> poolRelayIpv6 pr
    , bText <$> poolRelayDnsName pr
    , bText <$> poolRelayDnsSrvName pr
    , bInt64 . fromIntegral <$> poolRelayPort pr
    ]

encodePoolStatCopy :: PoolStat -> ByteString
encodePoolStatCopy ps =
  buildCopyRow
    [ Just $ bInt64 (getPoolHashId $ poolStatPoolHashId ps)
    , Just $ bWord64 (poolStatEpochNo ps)
    , Just $ bWord64 (unDbWord64 $ poolStatNumberOfBlocks ps)
    , Just $ bWord64 (unDbWord64 $ poolStatNumberOfDelegators ps)
    , Just $ bWord64 (unDbWord64 $ poolStatStake ps)
    , bWord64 . unDbWord64 <$> poolStatVotingPower ps
    ]

encodeDelistedPoolCopy :: DelistedPool -> ByteString
encodeDelistedPoolCopy dp =
  buildCopyRow [ Just $ bHex (delistedPoolHashRaw dp) ]

encodeReservedPoolTickerCopy :: ReservedPoolTicker -> ByteString
encodeReservedPoolTickerCopy rpt =
  buildCopyRow
    [ Just $ bText (reservedPoolTickerName rpt)
    , Just $ bHex (reservedPoolTickerPoolHash rpt)
    ]

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------

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
  , poolUpdateMargin        >$< E.param (E.nonNullable rationalAsNumericEncoder)
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
  <*> D.column (D.nonNullable rationalAsNumericDecoder)
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

-- PoolStat ----------------------------------------------------------------

poolStatEncoder :: E.Params PoolStat
poolStatEncoder = mconcat
  [ poolStatPoolHashId         >$< idEncoder getPoolHashId
  , poolStatEpochNo            >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , poolStatNumberOfBlocks     >$< E.param (E.nonNullable dbWord64ValueEncoder)
  , poolStatNumberOfDelegators >$< E.param (E.nonNullable dbWord64ValueEncoder)
  , poolStatStake              >$< E.param (E.nonNullable dbWord64ValueEncoder)
  , poolStatVotingPower        >$< E.param (E.nullable dbWord64ValueEncoder)
  ]

poolStatDecoder :: D.Row PoolStat
poolStatDecoder = PoolStat
  <$> idDecoder PoolHashId
  <*> D.column (D.nonNullable $ fromIntegral <$> D.int8)
  <*> D.column (D.nonNullable dbWord64ValueDecoder)
  <*> D.column (D.nonNullable dbWord64ValueDecoder)
  <*> D.column (D.nonNullable dbWord64ValueDecoder)
  <*> D.column (D.nullable dbWord64ValueDecoder)

entityPoolStatDecoder :: D.Row (PoolStatId, PoolStat)
entityPoolStatDecoder = (,)
  <$> idDecoder PoolStatId
  <*> poolStatDecoder

-- DelistedPool ------------------------------------------------------------

delistedPoolEncoder :: E.Params DelistedPool
delistedPoolEncoder =
  delistedPoolHashRaw >$< E.param (E.nonNullable E.bytea)

delistedPoolDecoder :: D.Row DelistedPool
delistedPoolDecoder = DelistedPool
  <$> D.column (D.nonNullable D.bytea)

entityDelistedPoolDecoder :: D.Row (DelistedPoolId, DelistedPool)
entityDelistedPoolDecoder = (,)
  <$> idDecoder DelistedPoolId
  <*> delistedPoolDecoder

-- ReservedPoolTicker ------------------------------------------------------

reservedPoolTickerEncoder :: E.Params ReservedPoolTicker
reservedPoolTickerEncoder = mconcat
  [ reservedPoolTickerName     >$< E.param (E.nonNullable E.text)
  , reservedPoolTickerPoolHash >$< E.param (E.nonNullable E.bytea)
  ]

reservedPoolTickerDecoder :: D.Row ReservedPoolTicker
reservedPoolTickerDecoder = ReservedPoolTicker
  <$> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable D.bytea)

entityReservedPoolTickerDecoder :: D.Row (ReservedPoolTickerId, ReservedPoolTicker)
entityReservedPoolTickerDecoder = (,)
  <$> idDecoder ReservedPoolTickerId
  <*> reservedPoolTickerDecoder
