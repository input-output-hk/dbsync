{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema type for the @ada_pots@ table.
--
-- Records the protocol-level ada accounting at each epoch boundary:
-- treasury, reserves, rewards, utxo, fees, and the various deposit
-- pots. Populated by the @EpochBoundary@ extractor when the ledger
-- subsystem reports a 'NewEpoch' event with attached
-- @AdaPots@ data.
--
-- Per upstream's documentation:
--
-- > This is only populated for the Shelley and later eras, and only on
-- > epoch boundaries. The treasury and rewards fields will be correct
-- > for the whole epoch, but all other fields change block by block.
module DbSync.Db.Schema.AdaPots
  ( -- * Schema type
    AdaPots (..)

    -- * Table definition
  , adaPotsTableDef

    -- * Column records (compile-time-safe column references)
  , AdaPotsCols (..), adaPotsCols, adaPotsColsList

    -- * Per-module column-record registry
  , adaPotsColumnRecords

    -- * COPY encoding
  , encodeAdaPotsCopy

    -- * Hasql encoders \/ decoders
  , adaPotsEncoder
  , adaPotsDecoder
  , entityAdaPotsDecoder
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
  )
import DbSync.Db.Loader.Encoder (buildCopyRow, bInt64, bWord64)

-- ---------------------------------------------------------------------------
-- * Key type family instance
-- ---------------------------------------------------------------------------

type instance Key AdaPots = AdaPotsId

-- ---------------------------------------------------------------------------
-- * Schema type
-- ---------------------------------------------------------------------------

-- | The @ada_pots@ table.
--
-- One row per epoch boundary, capturing the protocol-level ada
-- accounting at the transition slot.
data AdaPots = AdaPots
  { adaPotsSlotNo            :: !Word64
      -- ^ The slot at which this snapshot was taken (the boundary
      -- block's slot number).
  , adaPotsEpochNo           :: !Word64
      -- ^ The /new/ epoch number that just started.
  , adaPotsTreasury          :: !DbLovelace
  , adaPotsReserves          :: !DbLovelace
  , adaPotsRewards           :: !DbLovelace
  , adaPotsUtxo              :: !DbLovelace
      -- ^ Note: upstream applies a @fixUTxOPots@ correction at apply
      -- time so this matches @maxLovelaceSupply - sum(other pots)@.
  , adaPotsDepositsStake     :: !DbLovelace
  , adaPotsFees              :: !DbLovelace
  , adaPotsBlockId           :: !BlockId
      -- ^ FK to the @block@ row that triggered this snapshot.
  , adaPotsDepositsDrep      :: !DbLovelace
      -- ^ Conway+ only; zero in earlier eras.
  , adaPotsDepositsProposal  :: !DbLovelace
      -- ^ Conway+ only; zero in earlier eras.
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definition
-- ---------------------------------------------------------------------------

-- | DDL definition for the @ada_pots@ table.
--
-- UNLOGGED during 'IngestChainHistory' (matches the rest of the
-- extractor tables). The PK and FK to @block@ are added later in
-- 'PreparingForVolatileTail' alongside indexes.
adaPotsTableDef :: TableDef
adaPotsTableDef = TableDef
  { tdName    = "ada_pots"
  , tdColumns =
      [ ColumnDef "id"                 PgBigInt False
      , ColumnDef "slot_no"            PgBigInt False
      , ColumnDef "epoch_no"           PgBigInt False
      , ColumnDef "treasury"           PgNumeric False
      , ColumnDef "reserves"           PgNumeric False
      , ColumnDef "rewards"            PgNumeric False
      , ColumnDef "utxo"               PgNumeric False
      , ColumnDef "deposits_stake"     PgNumeric False
      , ColumnDef "fees"               PgNumeric False
      , ColumnDef "block_id"           PgBigInt False
      , ColumnDef "deposits_drep"      PgNumeric False
      , ColumnDef "deposits_proposal"  PgNumeric False
      ]
  , tdMode           = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdForeignKeys = []
  }

-- ---------------------------------------------------------------------------
-- * Column records
-- ---------------------------------------------------------------------------

data AdaPotsCols = AdaPotsCols
  { apcId               :: !TableColumn
  , apcSlotNo           :: !TableColumn
  , apcEpochNo          :: !TableColumn
  , apcTreasury         :: !TableColumn
  , apcReserves         :: !TableColumn
  , apcRewards          :: !TableColumn
  , apcUtxo             :: !TableColumn
  , apcDepositsStake    :: !TableColumn
  , apcFees             :: !TableColumn
  , apcBlockId          :: !TableColumn
  , apcDepositsDrep     :: !TableColumn
  , apcDepositsProposal :: !TableColumn
  }

adaPotsCols :: AdaPotsCols
adaPotsCols =
  let c = TableColumn adaPotsTableDef
  in AdaPotsCols
       { apcId               = c "id"
       , apcSlotNo           = c "slot_no"
       , apcEpochNo          = c "epoch_no"
       , apcTreasury         = c "treasury"
       , apcReserves         = c "reserves"
       , apcRewards          = c "rewards"
       , apcUtxo             = c "utxo"
       , apcDepositsStake    = c "deposits_stake"
       , apcFees             = c "fees"
       , apcBlockId          = c "block_id"
       , apcDepositsDrep     = c "deposits_drep"
       , apcDepositsProposal = c "deposits_proposal"
       }

adaPotsColsList :: [TableColumn]
adaPotsColsList =
  [ adaPotsCols.apcId
  , adaPotsCols.apcSlotNo
  , adaPotsCols.apcEpochNo
  , adaPotsCols.apcTreasury
  , adaPotsCols.apcReserves
  , adaPotsCols.apcRewards
  , adaPotsCols.apcUtxo
  , adaPotsCols.apcDepositsStake
  , adaPotsCols.apcFees
  , adaPotsCols.apcBlockId
  , adaPotsCols.apcDepositsDrep
  , adaPotsCols.apcDepositsProposal
  ]

-- ---------------------------------------------------------------------------
-- * Per-module column-record registry
-- ---------------------------------------------------------------------------

adaPotsColumnRecords :: [(TableDef, [TableColumn])]
adaPotsColumnRecords =
  [ (adaPotsTableDef, adaPotsColsList)
  ]

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

-- | Encode an 'AdaPots' record as a single COPY text row.
--
-- Field order must match 'adaPotsTableDef' exactly.
encodeAdaPotsCopy :: AdaPots -> ByteString
encodeAdaPotsCopy pots =
  buildCopyRow
    [ Just $ bWord64 (adaPotsSlotNo pots)
    , Just $ bWord64 (adaPotsEpochNo pots)
    , Just $ bWord64 (unDbLovelace $ adaPotsTreasury pots)
    , Just $ bWord64 (unDbLovelace $ adaPotsReserves pots)
    , Just $ bWord64 (unDbLovelace $ adaPotsRewards pots)
    , Just $ bWord64 (unDbLovelace $ adaPotsUtxo pots)
    , Just $ bWord64 (unDbLovelace $ adaPotsDepositsStake pots)
    , Just $ bWord64 (unDbLovelace $ adaPotsFees pots)
    , Just $ bInt64 (getBlockId $ adaPotsBlockId pots)
    , Just $ bWord64 (unDbLovelace $ adaPotsDepositsDrep pots)
    , Just $ bWord64 (unDbLovelace $ adaPotsDepositsProposal pots)
    ]

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------

-- | Parameter order matches the INSERT column list in
-- 'DbSync.Db.Statement.EpochBoundary.insertAdaPotsRowStmt'.
adaPotsEncoder :: E.Params AdaPots
adaPotsEncoder = mconcat
  [ adaPotsSlotNo           >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , adaPotsEpochNo          >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , adaPotsTreasury         >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , adaPotsReserves         >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , adaPotsRewards          >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , adaPotsUtxo             >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , adaPotsDepositsStake    >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , adaPotsFees             >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , adaPotsBlockId          >$< idEncoder getBlockId
  , adaPotsDepositsDrep     >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , adaPotsDepositsProposal >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  ]

adaPotsDecoder :: D.Row AdaPots
adaPotsDecoder =
  (AdaPots . fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> idDecoder BlockId
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)

entityAdaPotsDecoder :: D.Row (AdaPotsId, AdaPots)
entityAdaPotsDecoder = (,)
  <$> idDecoder AdaPotsId
  <*> adaPotsDecoder
