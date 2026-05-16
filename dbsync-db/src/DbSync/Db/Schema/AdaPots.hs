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

    -- * COPY encoding
  , encodeAdaPotsCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Db.Writer.Copy.Encoder (buildCopyRow, bInt64, bWord64)

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
  , tdForeignKeys = []
  }

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

-- | Encode an 'AdaPots' record as a single COPY text row.
--
-- Field order must match 'adaPotsTableDef' exactly.
encodeAdaPotsCopy :: AdaPotsId -> AdaPots -> ByteString
encodeAdaPotsCopy (AdaPotsId apid) pots =
  buildCopyRow
    [ Just $ bInt64 apid
    , Just $ bWord64 (adaPotsSlotNo pots)
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
