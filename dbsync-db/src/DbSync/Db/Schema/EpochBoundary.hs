{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema types for boundary-triggered tables beyond @ada_pots@.
--
-- Two flavours of writer feed this module:
--
--   * 'epoch_boundary' extractor (ledger-only): @epoch_param@,
--     @epoch_state@, @cost_model@. Written once per epoch boundary
--     from 'ApplyResult.apNewEpoch' protocol-parameter data.
--   * 'stake_delegation' extractor (block-extracted, Shelley→Babbage):
--     @pot_transfer@, @treasury@, @reserve@. Written when an
--     @MIRCert@ appears in a transaction.
module DbSync.Db.Schema.EpochBoundary
  ( -- * Schema types
    EpochParam (..)
  , EpochState (..)
  , CostModel (..)
  , PotTransfer (..)
  , Treasury (..)
  , Reserve (..)

    -- * Table definitions
  , epochParamTableDef
  , epochStateTableDef
  , costModelTableDef
  , potTransferTableDef
  , treasuryTableDef
  , reserveTableDef

    -- * COPY encoding
  , encodeEpochParamCopy
  , encodeEpochStateCopy
  , encodeCostModelCopy
  , encodePotTransferCopy
  , encodeTreasuryCopy
  , encodeReserveCopy

    -- * Hasql encoders \/ decoders
  , epochParamEncoder
  , epochParamDecoder
  , entityEpochParamDecoder
  , epochStateEncoder
  , epochStateDecoder
  , entityEpochStateDecoder
  , costModelEncoder
  , costModelDecoder
  , entityCostModelDecoder
  , potTransferEncoder
  , potTransferDecoder
  , entityPotTransferDecoder
  , treasuryEncoder
  , treasuryDecoder
  , entityTreasuryDecoder
  , reserveEncoder
  , reserveDecoder
  , entityReserveDecoder
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
import DbSync.Db.Types
  ( DbInt65
  , DbLovelace (..)
  , DbWord64 (..)
  , bDouble
  , bInt65
  , dbInt65Encoder
  , dbInt65Decoder
  , dbLovelaceValueDecoder
  , dbLovelaceValueEncoder
  , doubleAsTextDecoder
  , doubleAsTextEncoder
  , maybeDbLovelaceDecoder
  , maybeDbLovelaceEncoder
  , maybeDbWord64Decoder
  , maybeDbWord64Encoder
  , maybeDoubleAsTextDecoder
  , maybeDoubleAsTextEncoder
  )
import DbSync.Db.Loader.Encoder
  ( buildCopyRow
  , bHex
  , bInt64
  , bText
  , bWord64
  )

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key EpochParam = EpochParamId
type instance Key EpochState = EpochStateId
type instance Key CostModel = CostModelId
type instance Key PotTransfer = PotTransferId
type instance Key Treasury = TreasuryId
type instance Key Reserve = ReserveId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | The @epoch_param@ table — one row per epoch carrying the
-- consensus-level protocol parameters in effect for that epoch.
--
-- 53 columns. Most Conway-era fields are nullable because they have
-- no Shelley\/Allegra\/Mary equivalent. @cost_model_id@ is a
-- nullable FK to 'CostModel' (the Alonzo cost-models become
-- available from Alonzo onwards).
data EpochParam = EpochParam
  { epochParamEpochNo                    :: !Word64
  , epochParamMinFeeA                    :: !Word64
  , epochParamMinFeeB                    :: !Word64
  , epochParamMaxBlockSize               :: !Word64
  , epochParamMaxTxSize                  :: !Word64
  , epochParamMaxBhSize                  :: !Word64
  , epochParamKeyDeposit                 :: !DbLovelace
  , epochParamPoolDeposit                :: !DbLovelace
  , epochParamMaxEpoch                   :: !Word64
  , epochParamOptimalPoolCount           :: !Word64
  , epochParamInfluence                  :: !Double
  , epochParamMonetaryExpandRate         :: !Double
  , epochParamTreasuryGrowthRate         :: !Double
  , epochParamDecentralisation           :: !Double
  , epochParamProtocolMajor              :: !Word16
  , epochParamProtocolMinor              :: !Word16
  , epochParamMinUtxoValue               :: !DbLovelace
  , epochParamMinPoolCost                :: !DbLovelace
  , epochParamNonce                      :: !(Maybe ByteString)
  , epochParamCostModelId                :: !(Maybe CostModelId)
  , epochParamPriceMem                   :: !(Maybe Double)
  , epochParamPriceStep                  :: !(Maybe Double)
  , epochParamMaxTxExMem                 :: !(Maybe DbWord64)
  , epochParamMaxTxExSteps               :: !(Maybe DbWord64)
  , epochParamMaxBlockExMem              :: !(Maybe DbWord64)
  , epochParamMaxBlockExSteps            :: !(Maybe DbWord64)
  , epochParamMaxValSize                 :: !(Maybe DbWord64)
  , epochParamCollateralPercent          :: !(Maybe Word16)
  , epochParamMaxCollateralInputs        :: !(Maybe Word16)
  , epochParamBlockId                    :: !BlockId
  , epochParamExtraEntropy               :: !(Maybe ByteString)
  , epochParamCoinsPerUtxoSize           :: !(Maybe DbLovelace)
  , epochParamPvtMotionNoConfidence      :: !(Maybe Double)
  , epochParamPvtCommitteeNormal         :: !(Maybe Double)
  , epochParamPvtCommitteeNoConfidence   :: !(Maybe Double)
  , epochParamPvtHardForkInitiation      :: !(Maybe Double)
  , epochParamDvtMotionNoConfidence      :: !(Maybe Double)
  , epochParamDvtCommitteeNormal         :: !(Maybe Double)
  , epochParamDvtCommitteeNoConfidence   :: !(Maybe Double)
  , epochParamDvtUpdateToConstitution    :: !(Maybe Double)
  , epochParamDvtHardForkInitiation      :: !(Maybe Double)
  , epochParamDvtPPNetworkGroup          :: !(Maybe Double)
  , epochParamDvtPPEconomicGroup         :: !(Maybe Double)
  , epochParamDvtPPTechnicalGroup        :: !(Maybe Double)
  , epochParamDvtPPGovGroup              :: !(Maybe Double)
  , epochParamDvtTreasuryWithdrawal      :: !(Maybe Double)
  , epochParamCommitteeMinSize           :: !(Maybe DbWord64)
  , epochParamCommitteeMaxTermLength    :: !(Maybe DbWord64)
  , epochParamGovActionLifetime          :: !(Maybe DbWord64)
  , epochParamGovActionDeposit           :: !(Maybe DbWord64)
  , epochParamDrepDeposit                :: !(Maybe DbWord64)
  , epochParamDrepActivity               :: !(Maybe DbWord64)
  , epochParamPvtppSecurityGroup         :: !(Maybe Double)
  , epochParamMinFeeRefScriptCostPerByte :: !(Maybe Double)
  }
  deriving stock (Eq, Show)

-- | The @epoch_state@ table — Conway-era governance snapshot at
-- each epoch boundary. All three FK columns are nullable: the
-- snapshot may reference the latest enacted committee \/
-- no-confidence \/ constitution rows (or 'Nothing' if none has been
-- enacted yet).
data EpochState = EpochState
  { epochStateCommitteeId    :: !(Maybe CommitteeId)
  , epochStateNoConfidenceId :: !(Maybe GovActionProposalId)
  , epochStateConstitutionId :: !(Maybe ConstitutionId)
  , epochStateEpochNo        :: !Word64
  }
  deriving stock (Eq, Show)

-- | The @cost_model@ table — Plutus cost model JSON keyed by its
-- canonical hash. Multiple @epoch_param@ \/ @param_proposal@ rows
-- can reference the same cost model so callers dedup on @hash@
-- before writing.
data CostModel = CostModel
  { costModelCosts :: !Text       -- ^ JSONB body
  , costModelHash  :: !ByteString -- ^ 32-byte Blake2b hash of the canonical CBOR
  }
  deriving stock (Eq, Show)

-- | The @pot_transfer@ table — one row per pot-to-pot MIR
-- certificate (Shelley→Babbage). Carries both deltas signed via
-- 'DbInt65' because each row represents a transfer in one
-- direction.
data PotTransfer = PotTransfer
  { potTransferCertIndex :: !Word16
  , potTransferTreasury  :: !DbInt65
  , potTransferReserves  :: !DbInt65
  , potTransferTxId      :: !TxId
  }
  deriving stock (Eq, Show)

-- | The @treasury@ table — one row per stake address that
-- receives a treasury MIR payout in a given transaction.
data Treasury = Treasury
  { treasuryAddrId    :: !StakeAddressId
  , treasuryCertIndex :: !Word16
  , treasuryAmount    :: !DbInt65
  , treasuryTxId      :: !TxId
  }
  deriving stock (Eq, Show)

-- | The @reserve@ table — one row per stake address that receives
-- a reserves MIR payout in a given transaction.
data Reserve = Reserve
  { reserveAddrId    :: !StakeAddressId
  , reserveCertIndex :: !Word16
  , reserveAmount    :: !DbInt65
  , reserveTxId      :: !TxId
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definitions
-- ---------------------------------------------------------------------------

-- | 53-column @epoch_param@. Doubles ride a @text@ column carrying
-- their @show \@Double@ encoding. Conway-era fields (committee
-- thresholds, gov-action params) are all nullable.
--
-- UNIQUE on @(epoch_no, block_id)@: the same epoch_no may
-- legitimately appear with different block_ids across resyncs.
epochParamTableDef :: TableDef
epochParamTableDef = TableDef
  { tdName    = "epoch_param"
  , tdColumns =
      [ ColumnDef "id"                              PgBigInt   False
      , ColumnDef "epoch_no"                        PgBigInt   False
      , ColumnDef "min_fee_a"                       PgBigInt   False
      , ColumnDef "min_fee_b"                       PgBigInt   False
      , ColumnDef "max_block_size"                  PgBigInt   False
      , ColumnDef "max_tx_size"                     PgBigInt   False
      , ColumnDef "max_bh_size"                     PgBigInt   False
      , ColumnDef "key_deposit"                     PgNumeric  False
      , ColumnDef "pool_deposit"                    PgNumeric  False
      , ColumnDef "max_epoch"                       PgBigInt   False
      , ColumnDef "optimal_pool_count"              PgBigInt   False
      , ColumnDef "influence"                       PgText     False
      , ColumnDef "monetary_expand_rate"            PgText     False
      , ColumnDef "treasury_growth_rate"            PgText     False
      , ColumnDef "decentralisation"                PgText     False
      , ColumnDef "protocol_major"                  PgSmallInt False
      , ColumnDef "protocol_minor"                  PgSmallInt False
      , ColumnDef "min_utxo_value"                  PgNumeric  False
      , ColumnDef "min_pool_cost"                   PgNumeric  False
      , ColumnDef "nonce"                           PgBytea    True
      , ColumnDef "cost_model_id"                   PgBigInt   True
      , ColumnDef "price_mem"                       PgText     True
      , ColumnDef "price_step"                      PgText     True
      , ColumnDef "max_tx_ex_mem"                   PgNumeric  True
      , ColumnDef "max_tx_ex_steps"                 PgNumeric  True
      , ColumnDef "max_block_ex_mem"                PgNumeric  True
      , ColumnDef "max_block_ex_steps"              PgNumeric  True
      , ColumnDef "max_val_size"                    PgNumeric  True
      , ColumnDef "collateral_percent"              PgSmallInt True
      , ColumnDef "max_collateral_inputs"           PgSmallInt True
      , ColumnDef "block_id"                        PgBigInt   False
      , ColumnDef "extra_entropy"                   PgBytea    True
      , ColumnDef "coins_per_utxo_size"             PgNumeric  True
      , ColumnDef "pvt_motion_no_confidence"        PgText     True
      , ColumnDef "pvt_committee_normal"            PgText     True
      , ColumnDef "pvt_committee_no_confidence"     PgText     True
      , ColumnDef "pvt_hard_fork_initiation"        PgText     True
      , ColumnDef "dvt_motion_no_confidence"        PgText     True
      , ColumnDef "dvt_committee_normal"            PgText     True
      , ColumnDef "dvt_committee_no_confidence"     PgText     True
      , ColumnDef "dvt_update_to_constitution"      PgText     True
      , ColumnDef "dvt_hard_fork_initiation"        PgText     True
      , ColumnDef "dvt_pp_network_group"            PgText     True
      , ColumnDef "dvt_pp_economic_group"           PgText     True
      , ColumnDef "dvt_pp_technical_group"          PgText     True
      , ColumnDef "dvt_pp_gov_group"                PgText     True
      , ColumnDef "dvt_treasury_withdrawal"         PgText     True
      , ColumnDef "committee_min_size"              PgNumeric  True
      , ColumnDef "committee_max_term_length"       PgNumeric  True
      , ColumnDef "gov_action_lifetime"             PgNumeric  True
      , ColumnDef "gov_action_deposit"              PgNumeric  True
      , ColumnDef "drep_deposit"                    PgNumeric  True
      , ColumnDef "drep_activity"                   PgNumeric  True
      , ColumnDef "pvtpp_security_group"            PgText     True
      , ColumnDef "min_fee_ref_script_cost_per_byte" PgText    True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = ["epoch_no" :| ["block_id"]]
  , tdGeneratedColumns  = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys =
      [ ForeignKey "block_id" "block" "id"
      ]
  }

-- | 4-column @epoch_state@. All three FK columns are nullable; the
-- writer sets them based on what governance state is enacted at
-- the boundary.
epochStateTableDef :: TableDef
epochStateTableDef = TableDef
  { tdName    = "epoch_state"
  , tdColumns =
      [ ColumnDef "id"               PgBigInt False
      , ColumnDef "committee_id"     PgBigInt True
      , ColumnDef "no_confidence_id" PgBigInt True
      , ColumnDef "constitution_id"  PgBigInt True
      , ColumnDef "epoch_no"         PgBigInt False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns  = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys       = []
  }

-- | 3-column @cost_model@. @costs@ is JSONB; PostgreSQL parses it
-- on insert from the COPY text. UNIQUE on @hash@ — the dedup key.
costModelTableDef :: TableDef
costModelTableDef = TableDef
  { tdName    = "cost_model"
  , tdColumns =
      [ ColumnDef "id"    PgBigInt False
      , ColumnDef "costs" PgJsonb  False
      , ColumnDef "hash"  PgBytea  False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = [pure "hash"]
  , tdGeneratedColumns  = []
  , tdIdentityColumns = []
  , tdForeignKeys       = []
  }

-- | 5-column @pot_transfer@. UNIQUE on @(tx_id, cert_index)@.
potTransferTableDef :: TableDef
potTransferTableDef = TableDef
  { tdName    = "pot_transfer"
  , tdColumns =
      [ ColumnDef "id"         PgBigInt  False
      , ColumnDef "cert_index" PgBigInt  False
      , ColumnDef "treasury"   PgNumeric False
      , ColumnDef "reserves"   PgNumeric False
      , ColumnDef "tx_id"      PgBigInt  False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = ["tx_id" :| ["cert_index"]]
  , tdGeneratedColumns  = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys =
      [ ForeignKey "tx_id" "tx" "id"
      ]
  }

-- | 5-column @treasury@. UNIQUE on @(addr_id, tx_id)@.
treasuryTableDef :: TableDef
treasuryTableDef = TableDef
  { tdName    = "treasury"
  , tdColumns =
      [ ColumnDef "id"         PgBigInt  False
      , ColumnDef "addr_id"    PgBigInt  False
      , ColumnDef "cert_index" PgBigInt  False
      , ColumnDef "amount"     PgNumeric False
      , ColumnDef "tx_id"      PgBigInt  False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = ["addr_id" :| ["tx_id"]]
  , tdGeneratedColumns  = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys =
      [ ForeignKey "tx_id" "tx" "id"
      ]
  }

-- | 5-column @reserve@. UNIQUE on @(addr_id, tx_id)@.
reserveTableDef :: TableDef
reserveTableDef = TableDef
  { tdName    = "reserve"
  , tdColumns =
      [ ColumnDef "id"         PgBigInt  False
      , ColumnDef "addr_id"    PgBigInt  False
      , ColumnDef "cert_index" PgBigInt  False
      , ColumnDef "amount"     PgNumeric False
      , ColumnDef "tx_id"      PgBigInt  False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = ["addr_id" :| ["tx_id"]]
  , tdGeneratedColumns  = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys =
      [ ForeignKey "tx_id" "tx" "id"
      ]
  }

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodeEpochParamCopy :: EpochParam -> ByteString
encodeEpochParamCopy ep =
  buildCopyRow
    [ Just $ bWord64 (epochParamEpochNo ep)
    , Just $ bWord64 (epochParamMinFeeA ep)
    , Just $ bWord64 (epochParamMinFeeB ep)
    , Just $ bWord64 (epochParamMaxBlockSize ep)
    , Just $ bWord64 (epochParamMaxTxSize ep)
    , Just $ bWord64 (epochParamMaxBhSize ep)
    , Just $ bWord64 (unDbLovelace $ epochParamKeyDeposit ep)
    , Just $ bWord64 (unDbLovelace $ epochParamPoolDeposit ep)
    , Just $ bWord64 (epochParamMaxEpoch ep)
    , Just $ bWord64 (epochParamOptimalPoolCount ep)
    , Just $ bDouble (epochParamInfluence ep)
    , Just $ bDouble (epochParamMonetaryExpandRate ep)
    , Just $ bDouble (epochParamTreasuryGrowthRate ep)
    , Just $ bDouble (epochParamDecentralisation ep)
    , Just $ bInt64 (fromIntegral $ epochParamProtocolMajor ep)
    , Just $ bInt64 (fromIntegral $ epochParamProtocolMinor ep)
    , Just $ bWord64 (unDbLovelace $ epochParamMinUtxoValue ep)
    , Just $ bWord64 (unDbLovelace $ epochParamMinPoolCost ep)
    , bHex   <$> epochParamNonce ep
    , bInt64 . getCostModelId <$> epochParamCostModelId ep
    , bDouble <$> epochParamPriceMem ep
    , bDouble <$> epochParamPriceStep ep
    , bWord64 . unDbWord64 <$> epochParamMaxTxExMem ep
    , bWord64 . unDbWord64 <$> epochParamMaxTxExSteps ep
    , bWord64 . unDbWord64 <$> epochParamMaxBlockExMem ep
    , bWord64 . unDbWord64 <$> epochParamMaxBlockExSteps ep
    , bWord64 . unDbWord64 <$> epochParamMaxValSize ep
    , bInt64 . fromIntegral <$> epochParamCollateralPercent ep
    , bInt64 . fromIntegral <$> epochParamMaxCollateralInputs ep
    , Just $ bInt64 (getBlockId $ epochParamBlockId ep)
    , bHex <$> epochParamExtraEntropy ep
    , bWord64 . unDbLovelace <$> epochParamCoinsPerUtxoSize ep
    , bDouble <$> epochParamPvtMotionNoConfidence ep
    , bDouble <$> epochParamPvtCommitteeNormal ep
    , bDouble <$> epochParamPvtCommitteeNoConfidence ep
    , bDouble <$> epochParamPvtHardForkInitiation ep
    , bDouble <$> epochParamDvtMotionNoConfidence ep
    , bDouble <$> epochParamDvtCommitteeNormal ep
    , bDouble <$> epochParamDvtCommitteeNoConfidence ep
    , bDouble <$> epochParamDvtUpdateToConstitution ep
    , bDouble <$> epochParamDvtHardForkInitiation ep
    , bDouble <$> epochParamDvtPPNetworkGroup ep
    , bDouble <$> epochParamDvtPPEconomicGroup ep
    , bDouble <$> epochParamDvtPPTechnicalGroup ep
    , bDouble <$> epochParamDvtPPGovGroup ep
    , bDouble <$> epochParamDvtTreasuryWithdrawal ep
    , bWord64 . unDbWord64 <$> epochParamCommitteeMinSize ep
    , bWord64 . unDbWord64 <$> epochParamCommitteeMaxTermLength ep
    , bWord64 . unDbWord64 <$> epochParamGovActionLifetime ep
    , bWord64 . unDbWord64 <$> epochParamGovActionDeposit ep
    , bWord64 . unDbWord64 <$> epochParamDrepDeposit ep
    , bWord64 . unDbWord64 <$> epochParamDrepActivity ep
    , bDouble <$> epochParamPvtppSecurityGroup ep
    , bDouble <$> epochParamMinFeeRefScriptCostPerByte ep
    ]

encodeEpochStateCopy :: EpochState -> ByteString
encodeEpochStateCopy es =
  buildCopyRow
    [ bInt64 . getCommitteeId          <$> epochStateCommitteeId es
    , bInt64 . getGovActionProposalId  <$> epochStateNoConfidenceId es
    , bInt64 . getConstitutionId       <$> epochStateConstitutionId es
    , Just $ bWord64 (epochStateEpochNo es)
    ]

encodeCostModelCopy :: CostModelId -> CostModel -> ByteString
encodeCostModelCopy (CostModelId cmid) cm =
  buildCopyRow
    [ Just $ bInt64 cmid
    , Just $ bText (costModelCosts cm)
    , Just $ bHex  (costModelHash cm)
    ]

encodePotTransferCopy :: PotTransfer -> ByteString
encodePotTransferCopy pt =
  buildCopyRow
    [ Just $ bInt64 (fromIntegral $ potTransferCertIndex pt)
    , Just $ bInt65 (potTransferTreasury pt)
    , Just $ bInt65 (potTransferReserves pt)
    , Just $ bInt64 (getTxId $ potTransferTxId pt)
    ]

encodeTreasuryCopy :: Treasury -> ByteString
encodeTreasuryCopy t =
  buildCopyRow
    [ Just $ bInt64 (getStakeAddressId $ treasuryAddrId t)
    , Just $ bInt64 (fromIntegral $ treasuryCertIndex t)
    , Just $ bInt65 (treasuryAmount t)
    , Just $ bInt64 (getTxId $ treasuryTxId t)
    ]

encodeReserveCopy :: Reserve -> ByteString
encodeReserveCopy r =
  buildCopyRow
    [ Just $ bInt64 (getStakeAddressId $ reserveAddrId r)
    , Just $ bInt64 (fromIntegral $ reserveCertIndex r)
    , Just $ bInt65 (reserveAmount r)
    , Just $ bInt64 (getTxId $ reserveTxId r)
    ]

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------

-- EpochParam ---------------------------------------------------------------

epochParamEncoder :: E.Params EpochParam
epochParamEncoder = mconcat
  [ epochParamEpochNo                    >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , epochParamMinFeeA                    >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , epochParamMinFeeB                    >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , epochParamMaxBlockSize               >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , epochParamMaxTxSize                  >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , epochParamMaxBhSize                  >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , epochParamKeyDeposit                 >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , epochParamPoolDeposit                >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , epochParamMaxEpoch                   >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , epochParamOptimalPoolCount           >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , epochParamInfluence                  >$< E.param (E.nonNullable doubleAsTextEncoder)
  , epochParamMonetaryExpandRate         >$< E.param (E.nonNullable doubleAsTextEncoder)
  , epochParamTreasuryGrowthRate         >$< E.param (E.nonNullable doubleAsTextEncoder)
  , epochParamDecentralisation           >$< E.param (E.nonNullable doubleAsTextEncoder)
  , epochParamProtocolMajor              >$< E.param (E.nonNullable $ (fromIntegral :: Word16 -> Int16) >$< E.int2)
  , epochParamProtocolMinor              >$< E.param (E.nonNullable $ (fromIntegral :: Word16 -> Int16) >$< E.int2)
  , epochParamMinUtxoValue               >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , epochParamMinPoolCost                >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , epochParamNonce                      >$< E.param (E.nullable E.bytea)
  , epochParamCostModelId                >$< maybeIdEncoder getCostModelId
  , epochParamPriceMem                   >$< maybeDoubleAsTextEncoder
  , epochParamPriceStep                  >$< maybeDoubleAsTextEncoder
  , epochParamMaxTxExMem                 >$< maybeDbWord64Encoder
  , epochParamMaxTxExSteps               >$< maybeDbWord64Encoder
  , epochParamMaxBlockExMem              >$< maybeDbWord64Encoder
  , epochParamMaxBlockExSteps            >$< maybeDbWord64Encoder
  , epochParamMaxValSize                 >$< maybeDbWord64Encoder
  , epochParamCollateralPercent          >$< E.param (E.nullable $ (fromIntegral :: Word16 -> Int16) >$< E.int2)
  , epochParamMaxCollateralInputs        >$< E.param (E.nullable $ (fromIntegral :: Word16 -> Int16) >$< E.int2)
  , epochParamBlockId                    >$< idEncoder getBlockId
  , epochParamExtraEntropy               >$< E.param (E.nullable E.bytea)
  , epochParamCoinsPerUtxoSize           >$< maybeDbLovelaceEncoder
  , epochParamPvtMotionNoConfidence      >$< maybeDoubleAsTextEncoder
  , epochParamPvtCommitteeNormal         >$< maybeDoubleAsTextEncoder
  , epochParamPvtCommitteeNoConfidence   >$< maybeDoubleAsTextEncoder
  , epochParamPvtHardForkInitiation      >$< maybeDoubleAsTextEncoder
  , epochParamDvtMotionNoConfidence      >$< maybeDoubleAsTextEncoder
  , epochParamDvtCommitteeNormal         >$< maybeDoubleAsTextEncoder
  , epochParamDvtCommitteeNoConfidence   >$< maybeDoubleAsTextEncoder
  , epochParamDvtUpdateToConstitution    >$< maybeDoubleAsTextEncoder
  , epochParamDvtHardForkInitiation      >$< maybeDoubleAsTextEncoder
  , epochParamDvtPPNetworkGroup          >$< maybeDoubleAsTextEncoder
  , epochParamDvtPPEconomicGroup         >$< maybeDoubleAsTextEncoder
  , epochParamDvtPPTechnicalGroup        >$< maybeDoubleAsTextEncoder
  , epochParamDvtPPGovGroup              >$< maybeDoubleAsTextEncoder
  , epochParamDvtTreasuryWithdrawal      >$< maybeDoubleAsTextEncoder
  , epochParamCommitteeMinSize           >$< maybeDbWord64Encoder
  , epochParamCommitteeMaxTermLength     >$< maybeDbWord64Encoder
  , epochParamGovActionLifetime          >$< maybeDbWord64Encoder
  , epochParamGovActionDeposit           >$< maybeDbWord64Encoder
  , epochParamDrepDeposit                >$< maybeDbWord64Encoder
  , epochParamDrepActivity               >$< maybeDbWord64Encoder
  , epochParamPvtppSecurityGroup         >$< maybeDoubleAsTextEncoder
  , epochParamMinFeeRefScriptCostPerByte >$< maybeDoubleAsTextEncoder
  ]

epochParamDecoder :: D.Row EpochParam
epochParamDecoder = EpochParam
  <$> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> D.column (D.nonNullable doubleAsTextDecoder)
  <*> D.column (D.nonNullable doubleAsTextDecoder)
  <*> D.column (D.nonNullable doubleAsTextDecoder)
  <*> D.column (D.nonNullable doubleAsTextDecoder)
  <*> (fromIntegral <$> D.column (D.nonNullable D.int2))
  <*> (fromIntegral <$> D.column (D.nonNullable D.int2))
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> D.column (D.nullable D.bytea)
  <*> maybeIdDecoder CostModelId
  <*> maybeDoubleAsTextDecoder
  <*> maybeDoubleAsTextDecoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> (fmap fromIntegral <$> D.column (D.nullable D.int2))
  <*> (fmap fromIntegral <$> D.column (D.nullable D.int2))
  <*> idDecoder BlockId
  <*> D.column (D.nullable D.bytea)
  <*> maybeDbLovelaceDecoder
  <*> maybeDoubleAsTextDecoder
  <*> maybeDoubleAsTextDecoder
  <*> maybeDoubleAsTextDecoder
  <*> maybeDoubleAsTextDecoder
  <*> maybeDoubleAsTextDecoder
  <*> maybeDoubleAsTextDecoder
  <*> maybeDoubleAsTextDecoder
  <*> maybeDoubleAsTextDecoder
  <*> maybeDoubleAsTextDecoder
  <*> maybeDoubleAsTextDecoder
  <*> maybeDoubleAsTextDecoder
  <*> maybeDoubleAsTextDecoder
  <*> maybeDoubleAsTextDecoder
  <*> maybeDoubleAsTextDecoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDoubleAsTextDecoder
  <*> maybeDoubleAsTextDecoder

entityEpochParamDecoder :: D.Row (EpochParamId, EpochParam)
entityEpochParamDecoder = (,)
  <$> idDecoder EpochParamId
  <*> epochParamDecoder

-- EpochState ---------------------------------------------------------------

epochStateEncoder :: E.Params EpochState
epochStateEncoder = mconcat
  [ epochStateCommitteeId    >$< maybeIdEncoder getCommitteeId
  , epochStateNoConfidenceId >$< maybeIdEncoder getGovActionProposalId
  , epochStateConstitutionId >$< maybeIdEncoder getConstitutionId
  , epochStateEpochNo        >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  ]

epochStateDecoder :: D.Row EpochState
epochStateDecoder = EpochState
  <$> maybeIdDecoder CommitteeId
  <*> maybeIdDecoder GovActionProposalId
  <*> maybeIdDecoder ConstitutionId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))

entityEpochStateDecoder :: D.Row (EpochStateId, EpochState)
entityEpochStateDecoder = (,)
  <$> idDecoder EpochStateId
  <*> epochStateDecoder

-- CostModel ----------------------------------------------------------------

costModelEncoder :: E.Params CostModel
costModelEncoder = mconcat
  [ costModelCosts >$< E.param (E.nonNullable E.text)
  , costModelHash  >$< E.param (E.nonNullable E.bytea)
  ]

costModelDecoder :: D.Row CostModel
costModelDecoder = CostModel
  <$> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable D.bytea)

entityCostModelDecoder :: D.Row (CostModelId, CostModel)
entityCostModelDecoder = (,)
  <$> idDecoder CostModelId
  <*> costModelDecoder

-- PotTransfer --------------------------------------------------------------

potTransferEncoder :: E.Params PotTransfer
potTransferEncoder = mconcat
  [ (fromIntegral :: Word16 -> Int64) . potTransferCertIndex
                       >$< E.param (E.nonNullable E.int8)
  , potTransferTreasury >$< E.param (E.nonNullable dbInt65Encoder)
  , potTransferReserves >$< E.param (E.nonNullable dbInt65Encoder)
  , potTransferTxId     >$< idEncoder getTxId
  ]

potTransferDecoder :: D.Row PotTransfer
potTransferDecoder = PotTransfer
  <$> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> D.column (D.nonNullable dbInt65Decoder)
  <*> D.column (D.nonNullable dbInt65Decoder)
  <*> idDecoder TxId

entityPotTransferDecoder :: D.Row (PotTransferId, PotTransfer)
entityPotTransferDecoder = (,)
  <$> idDecoder PotTransferId
  <*> potTransferDecoder

-- Treasury -----------------------------------------------------------------

treasuryEncoder :: E.Params Treasury
treasuryEncoder = mconcat
  [ treasuryAddrId    >$< idEncoder getStakeAddressId
  , (fromIntegral :: Word16 -> Int64) . treasuryCertIndex
                      >$< E.param (E.nonNullable E.int8)
  , treasuryAmount    >$< E.param (E.nonNullable dbInt65Encoder)
  , treasuryTxId      >$< idEncoder getTxId
  ]

treasuryDecoder :: D.Row Treasury
treasuryDecoder = Treasury
  <$> idDecoder StakeAddressId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> D.column (D.nonNullable dbInt65Decoder)
  <*> idDecoder TxId

entityTreasuryDecoder :: D.Row (TreasuryId, Treasury)
entityTreasuryDecoder = (,)
  <$> idDecoder TreasuryId
  <*> treasuryDecoder

-- Reserve ------------------------------------------------------------------

reserveEncoder :: E.Params Reserve
reserveEncoder = mconcat
  [ reserveAddrId    >$< idEncoder getStakeAddressId
  , (fromIntegral :: Word16 -> Int64) . reserveCertIndex
                     >$< E.param (E.nonNullable E.int8)
  , reserveAmount    >$< E.param (E.nonNullable dbInt65Encoder)
  , reserveTxId      >$< idEncoder getTxId
  ]

reserveDecoder :: D.Row Reserve
reserveDecoder = Reserve
  <$> idDecoder StakeAddressId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> D.column (D.nonNullable dbInt65Decoder)
  <*> idDecoder TxId

entityReserveDecoder :: D.Row (ReserveId, Reserve)
entityReserveDecoder = (,)
  <$> idDecoder ReserveId
  <*> reserveDecoder
