{-# LANGUAGE OverloadedStrings #-}

-- | Generic block and transaction types.
--
-- These types are the era-independent representation of blocks and transactions.
-- Era-specific converters (fromShelleyBlock, fromConwayBlock, etc.) produce
-- these types from raw cardano-ledger types. Adapted from the existing
-- @Cardano.DbSync.Era.Shelley.Generic@ module hierarchy.
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
  deriving stock (Eq, Show, Bounded, Enum)

-- | Era-independent block representation.
-- Produced by era-specific converters from cardano-ledger types.
data GenericBlock = GenericBlock
  { blkEra          :: !BlockEra
  , blkHash         :: !ByteString       -- ^ 32-byte block header hash
  , blkPreviousHash :: !ByteString       -- ^ 32-byte previous block hash
  , blkSlotNo       :: !SlotNo
  , blkBlockNo      :: !BlockNo
  , blkEpochNo      :: !EpochNo
  , blkSize         :: !Word64
  , blkSlotLeader   :: !ByteString       -- ^ Pool key hash or genesis key
  , blkProtoMajor   :: !Word16
  , blkProtoMinor   :: !Word16
  , blkVrfKey       :: !(Maybe ByteString)
  , blkOpCert       :: !(Maybe ByteString)
  , blkOpCertCounter :: !(Maybe Word64)
  , blkTxs          :: ![GenericTx]
  }
  deriving stock (Show)

-- | Era-independent transaction representation.
-- Contains all data extractable from a transaction across all eras.
data GenericTx = GenericTx
  { txHash              :: !ByteString     -- ^ 32-byte transaction hash
  , txBlockIndex        :: !Word16         -- ^ Index within the block
  , txSize              :: !Word32
  , txFee               :: !Word64         -- ^ Fee in Lovelace
  , txValidContract     :: !Bool           -- ^ False for failed Plutus scripts
  , txInputs            :: ![GenericTxIn]
  , txOutputs           :: ![GenericTxOut]
  , txCollateralInputs  :: ![GenericTxIn]
  , txReferenceInputs   :: ![GenericTxIn]
  , txCollateralOutput  :: !(Maybe GenericTxOut)
  , txCertificates      :: ![GenericTxCertificate]
  , txWithdrawals       :: ![GenericTxWithdrawal]
  , txMetadata          :: !(Maybe ByteString)  -- ^ Raw CBOR metadata
  , txScriptSizes       :: !Word64         -- ^ Total Plutus script sizes
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
