{-# LANGUAGE OverloadedStrings #-}

-- | Generic block and transaction types.
--
-- These types are the era-independent representation of blocks and transactions.
-- Era-specific converters (fromShelleyBlock, fromConwayBlock, etc.) produce
-- these types from raw cardano-ledger types. Adapted from the existing
-- @Cardano.DbSync.Era.Shelley.Generic@ module hierarchy.
--
-- All fields match the original @cardano-db-sync@ Generic types, ensuring
-- full schema parity with the existing database.
module DbSync.Block.Types
  ( -- * Types
    GenericBlock (..)
  , GenericTx (..)
  , GenericTxIn (..)
  , GenericTxOut (..)
  , GenericTxCertificate (..)
  , GenericTxWithdrawal (..)
  , BlockEra (..)
  ) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.Time.Clock (UTCTime)

-- * Types

-- | Supported blockchain eras.
data BlockEra
  = Byron
  | Shelley
  | Allegra
  | Mary
  | Alonzo
  | Babbage
  | Conway
  | Dijkstra
  deriving stock (Eq, Show, Bounded, Enum)

-- | Era-independent block representation.
-- Produced by era-specific converters from cardano-ledger types.
--
-- Fields match the original @Cardano.DbSync.Era.Shelley.Generic.Block@
-- plus @SlotDetails@ fields (@blkEpochSlotNo@, @blkTime@) that the
-- original computes separately but we fold in during parsing.
data GenericBlock = GenericBlock
  { blkEra           :: !BlockEra
  , blkHash          :: !ByteString       -- ^ 32-byte block header hash
  , blkPreviousHash  :: !ByteString       -- ^ 32-byte previous block hash (empty for genesis)
  , blkSlotNo        :: !SlotNo
  , blkBlockNo       :: !BlockNo
  , blkEpochNo       :: !EpochNo
  , blkEpochSlotNo   :: !Word64           -- ^ Slot number within the epoch (from SlotDetails)
  , blkSize          :: !Word64
  , blkTime          :: !UTCTime          -- ^ Block time computed from slot via epoch schedule
  , blkSlotLeader    :: !ByteString       -- ^ Pool key hash or genesis key (28 bytes)
  , blkProtoMajor    :: !Word16
  , blkProtoMinor    :: !Word16
  , blkVrfKey        :: !(Maybe Text)     -- ^ VRF verification key (Bech32), Shelley+
  , blkOpCert        :: !(Maybe ByteString) -- ^ Operational certificate key (32 bytes), Shelley+
  , blkOpCertCounter :: !(Maybe Word64)   -- ^ Op cert counter, Shelley+
  , blkTxs           :: ![GenericTx]
  }
  deriving stock (Show)

-- | Era-independent transaction representation.
-- Contains all data extractable from a transaction across all eras.
-- Fields match the original @Cardano.DbSync.Era.Shelley.Generic.Tx@
-- ensuring full schema parity with the @tx@ database table.
data GenericTx = GenericTx
  { txHash              :: !ByteString     -- ^ 32-byte transaction hash
  , txBlockIndex        :: !Word64         -- ^ Index within the block (word31type in DB)
  , txSize              :: !Word64         -- ^ Transaction size in bytes (word31type in DB)
  , txFee               :: !Word64         -- ^ Fee in Lovelace
  , txOutSum            :: !Word64         -- ^ Sum of all output values in Lovelace
  , txValidContract     :: !Bool           -- ^ False for failed Plutus scripts (Alonzo+)
  , txScriptSize        :: !Word64         -- ^ Total Plutus script sizes in bytes
  , txTreasuryDonation  :: !Word64         -- ^ Treasury donation in Lovelace (Conway+, default 0)
  , txInvalidBefore     :: !(Maybe Word64) -- ^ Slot before which tx is invalid (Allegra+)
  , txInvalidHereafter  :: !(Maybe Word64) -- ^ Slot at/after which tx is invalid (Allegra+)
  , txInputs            :: ![GenericTxIn]
  , txOutputs           :: ![GenericTxOut]
  , txCollateralInputs  :: ![GenericTxIn]
  , txReferenceInputs   :: ![GenericTxIn]
  , txCollateralOutput  :: !(Maybe GenericTxOut)
  , txCertificates      :: ![GenericTxCertificate]
  , txWithdrawals       :: ![GenericTxWithdrawal]
  , txMetadata          :: !(Maybe ByteString)  -- ^ Raw CBOR metadata
  , txMint              :: ![(ByteString, ByteString, Integer)]
      -- ^ [(policy_id, asset_name, quantity)]
  -- TODO: Add governance fields (proposals, voting procedures)
  -- TODO: Add script/datum/redeemer fields
  }
  deriving stock (Show)

-- | A transaction input reference.
data GenericTxIn = GenericTxIn
  { txInHash  :: !ByteString  -- ^ Hash of the transaction being spent
  , txInIndex :: !Word16      -- ^ Output index being spent
  }
  deriving stock (Eq, Show)

-- | A transaction output.
data GenericTxOut = GenericTxOut
  { txOutIndex       :: !Word16
  , txOutAddress     :: !Text         -- ^ Bech32 or Byron base58 address
  , txOutAddressRaw  :: !ByteString   -- ^ Raw address bytes
  , txOutValue       :: !Word64       -- ^ Lovelace value
  , txOutDataHash    :: !(Maybe ByteString)
  , txOutInlineDatum :: !(Maybe ByteString)
  , txOutRefScript   :: !(Maybe ByteString)
  , txOutMultiAssets  :: ![(ByteString, ByteString, Integer)]
      -- ^ [(policy_id, asset_name, quantity)]
  }
  deriving stock (Show)

-- | A certificate within a transaction (stake registration, delegation, etc.).
-- Placeholder — will be expanded with specific certificate types.
data GenericTxCertificate = GenericTxCertificate
  { txCertIndex :: !Word16
  , txCertBytes :: !ByteString  -- ^ Raw CBOR for now
  }
  deriving stock (Show)

-- | A withdrawal within a transaction.
data GenericTxWithdrawal = GenericTxWithdrawal
  { txwRewardAddress :: !ByteString  -- ^ Stake address credential
  , txwAmount        :: !Word64      -- ^ Amount in Lovelace
  }
  deriving stock (Show)
