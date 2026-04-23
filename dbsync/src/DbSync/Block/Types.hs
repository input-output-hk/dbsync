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
  , CertAction (..)
  , PoolRegistrationData (..)
  , PoolRelayData (..)
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
  , blkIsEBB         :: !Bool              -- ^ True for Byron Epoch Boundary Blocks
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
  , txMetadata          :: Maybe ByteString
      -- ^ Raw CBOR metadata. Intentionally lazy — @serialize'@ is deferred
      -- until the Metadata extractor forces this field. If the extractor is
      -- disabled, @serialize'@ never runs (zero cost). Matches the original
      -- cardano-db-sync design.
  , txMint              :: ![(ByteString, ByteString, Integer)]
      -- ^ [(policy_id, asset_name, quantity)]
  , txCborRaw           :: Maybe ByteString
      -- ^ Raw CBOR-encoded transaction bytes (for tx_cbor table).
      -- Intentionally lazy — @serialize'@ is deferred until the CBOR
      -- extractor forces this field. This prevents accumulation of large
      -- pinned ByteStrings during parsing. @Nothing@ for Byron-era
      -- transactions where serialisation is non-trivial.
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

-- | A certificate within a transaction.
--
-- Carries structured certificate data so extractors can dispatch on
-- the certificate kind without re-deserializing CBOR.
data GenericTxCertificate = GenericTxCertificate
  { txCertIndex  :: !Word16
  , txCertAction :: !CertAction
  }
  deriving stock (Show)

-- | Discriminated union of all certificate kinds across eras.
--
-- Stake-related certs are consumed by the StakeDelegation extractor.
-- Pool-related certs are consumed by the Pool extractor.
-- Governance certs are consumed by the Governance extractor (future).
data CertAction
  -- Stake delegation certificates (Shelley+)
  = CertStakeRegistration
      !ByteString              -- ^ Stake credential hash (raw 28 bytes)
      !(Maybe Word64)          -- ^ Deposit (Conway+ only; Nothing for Shelley-Babbage)
  | CertStakeDeregistration
      !ByteString              -- ^ Stake credential hash
  | CertDelegation
      !ByteString              -- ^ Stake credential hash
      !ByteString              -- ^ Pool key hash (28 bytes)

  -- Pool certificates (Shelley+)
  | CertPoolRegistration !PoolRegistrationData
  | CertPoolRetirement
      !ByteString              -- ^ Pool key hash
      !Word64                  -- ^ Retiring epoch number

  -- Conway combined delegation certificates
  | CertConwayRegDeleg
      !ByteString              -- ^ Stake credential hash
      !ByteString              -- ^ Pool key hash
      !(Maybe Word64)          -- ^ Deposit
  | CertConwayDelegVote
      !ByteString              -- ^ Stake credential hash
      !ByteString              -- ^ DRep credential hash (or special: always-abstain / always-no-confidence)
  | CertConwayDelegStakeVote
      !ByteString              -- ^ Stake credential hash
      !ByteString              -- ^ Pool key hash
      !ByteString              -- ^ DRep credential hash

  -- Conway governance certificates (for future Governance extractor)
  | CertDRepRegistration
      !ByteString              -- ^ DRep credential hash
      !Word64                  -- ^ Deposit
      !(Maybe ByteString)      -- ^ Anchor URL hash (if present)
  | CertDRepDeregistration
      !ByteString              -- ^ DRep credential hash
      !Word64                  -- ^ Deposit refund
  | CertDRepUpdate
      !ByteString              -- ^ DRep credential hash
      !(Maybe ByteString)      -- ^ Anchor URL hash (if present)
  | CertCommitteeAuth
      !ByteString              -- ^ Cold key credential hash
      !ByteString              -- ^ Hot key credential hash
  | CertCommitteeResign
      !ByteString              -- ^ Cold key credential hash
      !(Maybe ByteString)      -- ^ Anchor hash

  -- MIR certificates (pre-Conway only)
  | CertMIR !ByteString        -- ^ Raw CBOR of MIR cert

  -- Fallback for unhandled/future certificate types
  | CertOther !ByteString      -- ^ Raw CBOR bytes
  deriving stock (Show)

-- | Pool registration data extracted from a @PoolRegistration@ certificate.
data PoolRegistrationData = PoolRegistrationData
  { prdPoolHash    :: !ByteString          -- ^ Pool key hash (28 bytes)
  , prdVrfKeyHash  :: !ByteString          -- ^ VRF verification key hash (32 bytes)
  , prdPledge      :: !Word64              -- ^ Pledge in Lovelace
  , prdCost        :: !Word64              -- ^ Fixed cost in Lovelace
  , prdMargin      :: !Double              -- ^ Pool margin (rational as Double)
  , prdRewardAddr  :: !ByteString          -- ^ Serialised reward account
  , prdOwners      :: ![ByteString]        -- ^ Stake key hashes of pool owners
  , prdRelays      :: ![PoolRelayData]     -- ^ Pool relay definitions
  , prdMetadata    :: !(Maybe (Text, ByteString))
      -- ^ @(metadataURL, metadataHash)@ if present
  }
  deriving stock (Show)

-- | Pool relay information from a pool registration certificate.
data PoolRelayData
  = PoolRelaySingleAddr
      !(Maybe Word16)          -- ^ Port
      !(Maybe Text)            -- ^ IPv4 address
      !(Maybe Text)            -- ^ IPv6 address
  | PoolRelayDnsName
      !(Maybe Word16)          -- ^ Port
      !Text                    -- ^ DNS A/AAAA record name
  | PoolRelayDnsSrv
      !Text                    -- ^ DNS SRV record name
  deriving stock (Show)

-- | A withdrawal within a transaction.
data GenericTxWithdrawal = GenericTxWithdrawal
  { txwRewardAddress :: !ByteString  -- ^ Serialised reward account (29 bytes)
  , txwAmount        :: !Word64      -- ^ Amount in Lovelace
  }
  deriving stock (Show)
