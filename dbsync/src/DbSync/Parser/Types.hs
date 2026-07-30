{-# LANGUAGE OverloadedStrings #-}

-- | Generic block and transaction types.
--
-- These types are the era-independent representation of blocks and transactions.
-- Era-specific converters (fromShelleyBlock, fromConwayBlock, etc.) produce
-- these types from raw cardano-ledger types. Adapted from the original
-- project's @Cardano.DbSync.Era.Shelley.Generic@ module hierarchy
-- (here flattened into "DbSync.Parser.Types" plus the projection modules
-- under "DbSync.Ledger").
--
-- All fields match the original @cardano-db-sync@ Generic types, ensuring
-- full schema parity with the existing database.
module DbSync.Parser.Types
  ( -- * Generic block / tx types
    GenericBlock (..)
  , GenericTx (..)
  , GenericTxIn (..)
  , GenericTxOut (..)
  , CredHash (..)
  , rewardAddrCredHash
  , GenericTxCertificate (..)
  , GenericTxWithdrawal (..)
  , GenericTxScript (..)
  , GenericTxDatum (..)
  , GenericTxRedeemer (..)
  , CertAction (..)
  , DRepIdent (..)
  , AnchorData (..)
  , MirPot (..)
  , MirAction (..)
  , PoolRegistrationData (..)
  , PoolRelayData (..)
  , BlockEra (..)

    -- * Governance types
  , GovActionRef (..)
  , GenericGovAction (..)
  , GenericGovActionProposal (..)
  , GenericVoter (..)
  , GenericVotingProcedure (..)

    -- * Era classification
  , EraStakeModel (..)
  , classifyEra

    -- * Cardano point alias
  , CardanoPoint
  ) where

import Cardano.Prelude

import Cardano.Ledger.Metadata (Metadatum)
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime)

import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Network.Block (Point)

import DbSync.Db.Types (ScriptPurpose, ScriptType, Vote)
import DbSync.Parser.ParamProposal (GenericParamProposal)

-- ---------------------------------------------------------------------------
-- * Cardano point alias
-- ---------------------------------------------------------------------------

-- | A point on the Cardano blockchain.
--
-- Defined here (rather than in 'DbSync.ChainSync.Connection') so that low-level
-- types — 'DbSync.Worker.Ledger.Types', 'DbSync.Worker.Ledger.Snapshot', etc. — can refer
-- to it without pulling in the entire ChainSync wiring (which itself
-- depends on 'DbSync.App.Env').
type CardanoPoint = Point (CardanoBlock StandardCrypto)

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

-- | Whether an era participates in the in-epoch stake-slice counter.
-- Pre-Shelley eras have no stake distribution; Shelley onward share
-- the same counter model.
data EraStakeModel
  = NoStakeSlices
  | StandardSlices
  deriving stock (Eq, Show)

-- | Classify a 'BlockEra' for stake-slice purposes. Exhaustive on
-- 'BlockEra' so a future era added there is flagged here at compile
-- time.
classifyEra :: BlockEra -> EraStakeModel
classifyEra = \case
  Byron    -> NoStakeSlices
  Shelley  -> StandardSlices
  Allegra  -> StandardSlices
  Mary     -> StandardSlices
  Alonzo   -> StandardSlices
  Babbage  -> StandardSlices
  Conway   -> StandardSlices
  Dijkstra -> StandardSlices

-- | Era-independent block representation.
-- Produced by era-specific converters from cardano-ledger types.
--
-- Fields match the original project's
-- @Cardano.DbSync.Era.Shelley.Generic.Block@ plus @SlotDetails@ fields
-- (@blkEpochSlotNo@, @blkTime@) that the original computes separately
-- but we fold in during parsing.
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
-- Fields match the original project's
-- @Cardano.DbSync.Era.Shelley.Generic.Tx@ ensuring full schema parity
-- with the @tx@ database table.
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
  , txMetadata          :: Maybe (Map Word64 Metadatum)
      -- ^ Per-key metadata, structured. Intentionally lazy — populated
      -- by the per-era 'DbSync.Parser.Metadata.from*Metadata' helpers
      -- and only forced when the Metadata extractor reads it.
  , txMint              :: ![(ByteString, ByteString, Integer)]
      -- ^ [(policy_id, asset_name, quantity)]
  , txCborRaw           :: Maybe ByteString
      -- ^ Raw CBOR-encoded transaction bytes (for tx_cbor table).
      -- Intentionally lazy — @serialize'@ is deferred until the CBOR
      -- extractor forces this field. This prevents accumulation of large
      -- pinned ByteStrings during parsing. @Nothing@ for Byron-era
      -- transactions where serialisation is non-trivial.
  , txScripts           :: ![GenericTxScript]
      -- ^ Scripts from the witness set and (where supported)
      -- auxiliary data.
  , txDatums            :: ![GenericTxDatum]
      -- ^ Plutus datum witnesses.
  , txRedeemers         :: ![GenericTxRedeemer]
      -- ^ Plutus redeemer witnesses.
  , txExtraKeyWitnesses :: ![ByteString]
      -- ^ 28-byte required-signer key hashes.
  , txParamProposal     :: ![GenericParamProposal]
      -- ^ Pre-Conway genesis-key parameter proposals from the
      -- @Update@ field. Conway+ leaves this empty; parameter
      -- changes ride 'GovParameterChange' instead.
  , txProposals         :: ![GenericGovActionProposal]
      -- ^ Conway+ governance proposals from the tx body.
  , txVotingProcedures  :: ![GenericVotingProcedure]
      -- ^ Conway+ voting procedures from the tx body.
  , txVotingAnchors     :: ![AnchorData]
      -- ^ Flat union of anchors referenced by certs/proposals/votes
      -- in this tx. The @voting_anchor.type@ enum is set by the call
      -- site that writes the row (gov_action / drep / vote /
      -- committee_dereg / constitution), so this convenience list
      -- carries no per-anchor type tag.
  }
  deriving stock (Show)

-- | A script attached to a transaction (witness set or auxiliary
-- data).
--
-- 'gtsJson' carries the JSON rendering for native scripts that
-- have one ('MultiSig', 'Timelock'); 'Nothing' for Plutus scripts
-- and for Dijkstra native scripts. 'gtsBytes' carries the raw
-- script CBOR when 'gtsJson' is absent.
data GenericTxScript = GenericTxScript
  { gtsHash           :: !ByteString
  , gtsType           :: !ScriptType
  , gtsJson           :: !(Maybe Text)
  , gtsBytes          :: !(Maybe ByteString)
  , gtsSerialisedSize :: !(Maybe Word64)
  }
  deriving stock (Eq, Show)

-- | A Plutus datum witness. 'gtdBytes' (the CBOR payload copy) and
-- 'gtdValue' (the @datum.value@ JSONB rendering) are both lazy: the
-- dedup resolver keys on 'gtdHash' and drops the row on a hash hit, so
-- neither is forced unless this is the first sighting actually written.
data GenericTxDatum = GenericTxDatum
  { gtdHash  :: !ByteString
  , gtdBytes :: ByteString
  , gtdValue :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | A Plutus redeemer. The embedded datum carries its own dedup
-- key ('gtrDataHash') so the extractor can write the
-- @redeemer_data@ row once per unique datum and link multiple
-- @redeemer@ rows to it. 'gtrDataBytes' and 'gtrDataValue' are lazy
-- for the same reason as their 'GenericTxDatum' counterparts: a
-- @redeemer_data@ hash hit drops the row without forcing them.
data GenericTxRedeemer = GenericTxRedeemer
  { gtrUnitMem    :: !Word64
  , gtrUnitSteps  :: !Word64
  , gtrPurpose    :: !ScriptPurpose
  , gtrIndex      :: !Word64
  , gtrScriptHash :: !(Maybe ByteString)
  , gtrDataHash   :: !ByteString
  , gtrDataBytes  :: ByteString
  , gtrDataValue  :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | A transaction input reference.
data GenericTxIn = GenericTxIn
  { txInHash       :: !ByteString  -- ^ Hash of the transaction being spent
  , txInIndex      :: !Word16      -- ^ Output index being spent
  , txInRedeemerIx :: !(Maybe Word64)
      -- ^ Position in 'txRedeemers' of the spend redeemer witnessing
      -- this input; 'Nothing' when not script-witnessed.
  }
  deriving stock (Eq, Show)

-- | A transaction output.
data GenericTxOut = GenericTxOut
  { txOutIndex       :: !Word16
  , txOutAddressRaw  :: !ByteString   -- ^ Raw address bytes
  , txOutValue       :: !Word64       -- ^ Lovelace value
  , txOutDataHash    :: !(Maybe ByteString)
  , txOutInlineDatum :: !(Maybe GenericTxDatum)
      -- ^ Babbage+ inline datum (hash + CBOR + JSON), 'Nothing' for a
      -- hash-only or datum-less output. Drives @tx_out.inline_datum_id@
      -- and the deduplicated @datum@ row.
  , txOutRefScript   :: !(Maybe GenericTxScript)
      -- ^ Babbage+ output reference script. Drives
      -- @tx_out.reference_script_id@ and the deduplicated @script@ row.
  , txOutMultiAssets  :: ![(ByteString, ByteString, Integer)]
      -- ^ [(policy_id, asset_name, quantity)]
  }
  deriving stock (Show)

-- | A credential flattened to its 28-byte hash together with whether
-- that hash is a script hash (vs. a stake/verification-key hash). The
-- flag selects the reward-address header nibble (@0xE_@ key vs @0xF_@
-- script) and whether @stake_address.script_hash@ is populated, so it
-- must travel with the bytes rather than be discarded at parse time.
data CredHash = CredHash
  { chHash     :: !ByteString
  , chIsScript :: !Bool
  }
  deriving stock (Eq, Show)

-- | Split a serialised reward address (@header || credential_hash@)
-- into its 28-byte credential and the script\/key flag carried by
-- header bit 4 (@0xF_@ script, @0xE_@ key). An empty input yields an
-- empty key credential rather than panicking on malformed data.
rewardAddrCredHash :: ByteString -> CredHash
rewardAddrCredHash bs = case BS.uncons bs of
  Just (header, cred) -> CredHash cred (header .&. 0x10 /= 0)
  Nothing             -> CredHash bs False

-- | A certificate within a transaction.
--
-- Carries structured certificate data so extractors can dispatch on
-- the certificate kind without re-deserializing CBOR.
data GenericTxCertificate = GenericTxCertificate
  { txCertIndex      :: !Word16
  , txCertAction     :: !CertAction
  , txCertRedeemerIx :: !(Maybe Word64)
      -- ^ Position in 'txRedeemers' of the cert redeemer witnessing
      -- this certificate; 'Nothing' when not script-witnessed.
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
      !CredHash                -- ^ Stake credential
      !(Maybe Word64)          -- ^ Deposit (Conway+ only; Nothing for Shelley-Babbage)
  | CertStakeDeregistration
      !CredHash                -- ^ Stake credential
  | CertDelegation
      !CredHash                -- ^ Stake credential
      !ByteString              -- ^ Pool key hash (28 bytes)

  -- Pool certificates (Shelley+)
  | CertPoolRegistration !PoolRegistrationData
  | CertPoolRetirement
      !ByteString              -- ^ Pool key hash
      !Word64                  -- ^ Retiring epoch number

  -- Conway combined delegation certificates
  | CertConwayRegDeleg
      !CredHash                -- ^ Stake credential
      !ByteString              -- ^ Pool key hash
      !(Maybe Word64)          -- ^ Deposit
  | CertConwayDelegVote
      !CredHash                -- ^ Stake credential
      !DRepIdent               -- ^ DRep target (credential or special abstain/no-confidence)
      !(Maybe Word64)          -- ^ Registration deposit when the cert also registers the stake key (RegDeleg variant); Nothing for a pure delegation
  | CertConwayDelegStakeVote
      !CredHash                -- ^ Stake credential
      !ByteString              -- ^ Pool key hash
      !DRepIdent               -- ^ DRep target
      !(Maybe Word64)          -- ^ Registration deposit when the cert also registers the stake key (RegDeleg variant); Nothing for a pure delegation

  -- Conway governance certificates
  | CertDRepRegistration
      !CredHash                -- ^ DRep credential
      !Word64                  -- ^ Deposit
      !(Maybe AnchorData)      -- ^ Anchor (URL + hash) if present
  | CertDRepDeregistration
      !CredHash                -- ^ DRep credential
      !Word64                  -- ^ Deposit refund
  | CertDRepUpdate
      !CredHash                -- ^ DRep credential
      !(Maybe AnchorData)      -- ^ Anchor (URL + hash) if present
  | CertCommitteeAuth
      !CredHash                -- ^ Cold key credential
      !CredHash                -- ^ Hot key credential
  | CertCommitteeResign
      !CredHash                -- ^ Cold key credential
      !(Maybe AnchorData)      -- ^ Anchor (URL + hash) if present

  -- | Move-Instantaneous-Reward cert (Shelley-Babbage). The
  -- 'stake_delegation' extractor writes @treasury@ \/ @reserve@ rows
  -- for 'MirToStakeAddresses' and a single @pot_transfer@ row for
  -- 'MirPotToPot'. Conway+ removes MIR from the cert sum, so the
  -- type-level guarantee is that this constructor never appears on
  -- a Conway+ block.
  | CertMir
      !MirPot
      !MirAction

  -- Fallback for unhandled/future certificate types
  | CertOther !ByteString      -- ^ Raw CBOR bytes
  deriving stock (Show)

-- | Which protocol pot a MIR certificate names.
data MirPot
  = MirReserves
  | MirTreasury
  deriving stock (Eq, Show)

-- | Payload of a MIR certificate.
data MirAction
  = MirToStakeAddresses ![(CredHash, Integer)]
      -- ^ Per-recipient (stake credential, delta-coin). Positive
      --   means \"credit the recipient from the named pot\".
  | MirPotToPot !Integer
      -- ^ Pot-to-pot transfer. Positive means \"send this much from
      --   the named pot to the other pot\".
  deriving stock (Eq, Show)

-- | DRep delegation target — a specific DRep credential, or one of
-- the protocol-defined \"always abstain\" / \"always no-confidence\"
-- meta-DReps.
data DRepIdent
  = DRepCred !CredHash
      -- ^ DRep credential (key or script)
  | DRepAlwaysAbstain
  | DRepAlwaysNoConfidence
  deriving stock (Eq, Show)

-- | The (URL, data-hash) pair pinning off-chain text to a governance
-- cert or proposal. Both fields are mandatory in the ledger's
-- 'Cardano.Ledger.BaseTypes.Anchor'.
data AnchorData = AnchorData
  { adUrl  :: !Text
  , adHash :: !ByteString
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Governance types
-- ---------------------------------------------------------------------------

-- | Reference to a previously-submitted governance action. Pairs the
-- proposing tx's 32-byte hash with the proposal's index within that
-- tx; the orchestrator resolves the pair to a 'GovActionProposalId'
-- via the per-block scratchpad (Ingest) or a SELECT (Follow).
data GovActionRef = GovActionRef
  { garTxHash :: !ByteString   -- ^ 32-byte proposing-tx hash
  , garIndex  :: !Word64       -- ^ proposal index within the tx
  }
  deriving stock (Eq, Show)

-- | A governance action's payload. The first 'Maybe GovActionRef' on
-- the amendment-style arms is the previous-action reference; 'Nothing'
-- means the proposal is starting a fresh amendment chain.
--
-- 'GovParameterChange' carries the embedded 'GenericParamProposal'
-- (with its optional cost-model map) and an optional guardrail
-- script hash. 'GovNewConstitution' carries the new constitution's
-- anchor and an optional script hash.
data GenericGovAction
  = GovParameterChange  !(Maybe GovActionRef) !GenericParamProposal !(Maybe ByteString)
  | GovHardForkInit     !(Maybe GovActionRef) !Word16 !Word16
      -- ^ Target @(major, minor)@ protocol version.
  | GovTreasuryWithdraw ![(CredHash, Word64)] !(Maybe ByteString)
      -- ^ @[(reward-account, lovelace)]@ + optional guardrail script hash.
  | GovNoConfidence     !(Maybe GovActionRef)
  | GovUpdateCommittee
      !(Maybe GovActionRef)
      !(Set.Set ByteString)
        -- ^ Cold keys to remove (28-byte credential hashes).
      ![(CredHash, Word64)]
        -- ^ Members to add: @(cold-key cred, expiration epoch)@.
      !Word64 !Word64
        -- ^ Quorum @(numerator, denominator)@.
  | GovNewConstitution  !(Maybe GovActionRef) !AnchorData !(Maybe ByteString)
  | GovInfoAction
  deriving stock (Show)

-- | A governance-action proposal as carried by a Conway+ tx body.
--
-- 'ggapDescriptionJson' is the canonical JSON encoding of the action
-- (the ledger's @Aeson.encode@ output); the @gov_action_proposal.description@
-- column stores it verbatim.
data GenericGovActionProposal = GenericGovActionProposal
  { ggapTxIndex         :: !Word64
  , ggapReturnAddrCred  :: !CredHash      -- ^ Stake credential of the return-deposit address.
  , ggapDeposit         :: !Word64        -- ^ Deposit amount in Lovelace.
  , ggapAnchor          :: !AnchorData    -- ^ Anchor URL+hash for the off-chain document.
  , ggapAction          :: !GenericGovAction
  , ggapDescriptionJson :: !Text
  }
  deriving stock (Show)

-- | A governance voter — one of three voter kinds matching the
-- @voter_role@ enum on @voting_procedure@.
data GenericVoter
  = VoterDRep      !DRepIdent
  | VoterStakePool !ByteString
      -- ^ 28-byte pool key hash.
  | VoterCommittee !ByteString !Bool
      -- ^ 28-byte committee hot-key credential plus @has_script@ flag.
  deriving stock (Eq, Show)

-- | A single vote cast by a 'GenericVoter' on a governance action.
data GenericVotingProcedure = GenericVotingProcedure
  { gvpTxIndex     :: !Word16
  , gvpVoter       :: !GenericVoter
  , gvpGovActionId :: !GovActionRef
  , gvpVote        :: !Vote
  , gvpAnchor      :: !(Maybe AnchorData)
  , gvpRedeemerIx  :: !(Maybe Word64)
      -- ^ Position in 'txRedeemers' of the vote redeemer witnessing
      -- this voter; a redeemer points at the voter, so all votes cast
      -- by the voter in one tx share it. 'Nothing' when not
      -- script-witnessed.
  }
  deriving stock (Show)

-- ---------------------------------------------------------------------------

-- | Pool registration data extracted from a @PoolRegistration@ certificate.
data PoolRegistrationData = PoolRegistrationData
  { prdPoolHash    :: !ByteString          -- ^ Pool key hash (28 bytes)
  , prdVrfKeyHash  :: !ByteString          -- ^ VRF verification key hash (32 bytes)
  , prdPledge      :: !Word64              -- ^ Pledge in Lovelace
  , prdCost        :: !Word64              -- ^ Fixed cost in Lovelace
  , prdMargin      :: !Rational            -- ^ Pool margin in [0, 1]
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
  , txwRedeemerIx    :: !(Maybe Word64)
      -- ^ Position in 'txRedeemers' of the reward redeemer witnessing
      -- this withdrawal; 'Nothing' when not script-witnessed.
  }
  deriving stock (Show)
