{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Domain-specific newtypes, enum types, and COPY\/hasql encoding
-- helpers for database column types.
--
-- This module groups types referenced from multiple
-- @Schema/<Domain>.hs@ modules so the schema modules can import them
-- without a dependency cycle.
--
-- __Encoding rule for @numeric@ columns:__ 'DbLovelace' \/ 'DbWord64' \/
-- 'Word128' are stored in PostgreSQL @numeric@ and must be encoded\/
-- decoded through 'Sci.Scientific'. Going via @int8@ silently
-- truncates aggregation results (e.g. @SUM(tx.out_sum)@) once they
-- exceed @maxBound \@Int64@. The decoders here use
-- 'Sci.toBoundedInteger' \/ 'floor' rather than 'Sci.coefficient' so
-- a value normalised by PostgreSQL (e.g. @Scientific 38 16@ for
-- @380_000_000_000_000_000@) is reconstructed correctly.
module DbSync.Db.Types
  ( -- * Numeric domain types
    DbLovelace (..)
  , DbWord64 (..)
  , DbInt65 (..)
  , toDbInt65
  , fromDbInt65

    -- * Enum types — one per PostgreSQL domain enum
  , ScriptPurpose (..)
  , ScriptType (..)
  , RewardSource (..)
  , SyncState (..)
  , Vote (..)
  , VoterRole (..)
  , GovActionType (..)
  , AnchorType (..)

    -- * Newtype wrappers
  , PoolUrl (..)
  , VoteUrl (..)
  , VoteMetaHash (..)

    -- * Scientific / Word conversion helpers
    --
    -- $scientificConversions
  , scientificToWord64
  , scientificToWord128
  , word64ToScientific
  , word128ToScientific

    -- * Hasql encoders \/ decoders for numeric domain types
  , dbLovelaceValueEncoder
  , dbLovelaceValueDecoder
  , dbLovelaceEncoder
  , dbLovelaceDecoder
  , maybeDbLovelaceEncoder
  , maybeDbLovelaceDecoder
  , dbWord64ValueEncoder
  , dbWord64ValueDecoder
  , dbWord64Encoder
  , dbWord64Decoder
  , maybeDbWord64Encoder
  , maybeDbWord64Decoder
  , dbInt65Encoder
  , dbInt65Decoder
  , word128Encoder
  , word128Decoder

    -- * COPY encoding helpers
  , bInt65
  , bWord128
  , bScriptPurpose
  , bScriptType
  , bRewardSource
  , bSyncState
  , bVote
  , bVoterRole
  , bGovActionType
  , bAnchorType
  ) where

import Cardano.Prelude

import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Functor.Contravariant ((>$<))
import qualified Data.Scientific as Sci
import Data.WideWord (Word128)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import DbSync.Db.Writer.Copy.Encoder (bInt64)

-- ---------------------------------------------------------------------------
-- * Numeric domain types
-- ---------------------------------------------------------------------------

-- | Lovelace values stored as PostgreSQL @numeric(20,0)@.
--
-- Uses a newtype rather than raw 'Word64' so that:
--
--   * The column type is unambiguous at the Haskell level.
--   * Encoders\/decoders can be swapped in later without changing call sites.
--   * Values that exceed @Int64@ range are handled correctly via @numeric@.
newtype DbLovelace = DbLovelace { unDbLovelace :: Word64 }
  deriving stock (Eq, Ord)
  deriving newtype (Show, Read)

-- | Large unsigned integers stored as PostgreSQL @numeric@.
--
-- Same motivation as 'DbLovelace' but for non-monetary Word64 columns
-- (e.g. @invalid_before@, @invalid_hereafter@).
newtype DbWord64 = DbWord64 { unDbWord64 :: Word64 }
  deriving stock (Eq, Ord)
  deriving newtype (Show, Read, Num)

-- | A signed 65-bit integer stored as PostgreSQL @numeric@.
--
-- 'DbInt65' is the in-memory representation upstream uses for signed
-- amounts that may briefly exceed 'Int64' range during ledger
-- accounting (e.g. @ada_pots.*@, @pot_transfer.*@,
-- @treasury.amount@, @reserve.amount@, @ma_tx_mint.quantity@ when
-- burning more than was minted in the same epoch).
--
-- The encoding uses a 'Word64' with bit 63 as the sign bit and the
-- remaining 63 bits as the magnitude. 'minBound' is special-cased
-- (sign bit set, magnitude zero) so it survives the 'abs' that would
-- otherwise overflow.
--
-- Most callers never reach for the constructor — use 'toDbInt65' to
-- pack an 'Int64' and 'fromDbInt65' to recover one. The derived
-- 'Show' / 'Read' instances surface the raw 'Word64' bit pattern;
-- prefer 'fromDbInt65' for human-readable output.
newtype DbInt65 = DbInt65 { unDbInt65 :: Word64 }
  deriving stock (Eq, Ord)
  deriving newtype (Show, Read)

-- | Pack an 'Int64' into the sign-magnitude 'DbInt65' representation.
--
-- The 'minBound' edge case (negate would overflow) is encoded as
-- @sign bit set, magnitude zero@ — the only representation 'fromDbInt65'
-- maps back to 'minBound'.
toDbInt65 :: Int64 -> DbInt65
toDbInt65 n
  | n >= 0          = DbInt65 (fromIntegral n)
  | n == minBound   = DbInt65 (setBit 0 63)
  | otherwise       = DbInt65 (setBit (fromIntegral (abs n)) 63)

-- | Recover the 'Int64' from a 'DbInt65'.
fromDbInt65 :: DbInt65 -> Int64
fromDbInt65 (DbInt65 w)
  | testBit w 63 =
      let magnitude = clearBit w 63
      in if magnitude == 0
           then minBound
           else negate (fromIntegral magnitude)
  | otherwise = fromIntegral w

-- ---------------------------------------------------------------------------
-- * Enum types
-- ---------------------------------------------------------------------------
--
-- Each enum is paired with a COPY builder ('b<Name>') a few sections
-- down. Hasql encoders\/decoders are deliberately deferred to the
-- per-schema-module Statement wiring (Phase 2 of SCHEMA-PLAN.md).

-- | What a Plutus script is being run for. Stored in @redeemer.purpose@.
data ScriptPurpose
  = Spend
  | Mint
  | Cert
  | Rewrd
  | Vote
  | Propose
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

-- | The flavour of script attached to a @script@ row.
data ScriptType
  = MultiSig
  | Timelock
  | PlutusV1
  | PlutusV2
  | PlutusV3
  | PlutusV4
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

-- | Where a reward originated. Stored in @reward.type@ /
-- @reward_rest.type@.
data RewardSource
  = RwdLeader
  | RwdMember
  | RwdReserves
  | RwdTreasury
  | RwdDepositRefund
  | RwdProposalRefund
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

-- | Whether the local tip is lagging or following the global chain
-- tip. Stored in the legacy @epoch_sync_time.state@ column.
data SyncState
  = SyncLagging
  | SyncFollowing
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

-- | A governance vote. Stored in @voting_procedure.vote@.
data Vote
  = VoteYes
  | VoteNo
  | VoteAbstain
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

-- | The role a voter is acting in. Stored in
-- @voting_procedure.voter_role@.
data VoterRole
  = ConstitutionalCommittee
  | DRep
  | SPO
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

-- | The flavour of a Conway governance action. Stored in
-- @gov_action_proposal.type@.
--
-- Note: the constructor 'NewCommitteeType' has the trailing @Type@
-- to dodge a clash with the @committee@ table; the PG value is
-- @"NewCommittee"@.
data GovActionType
  = ParameterChange
  | HardForkInitiation
  | TreasuryWithdrawals
  | NoConfidence
  | NewCommitteeType
  | NewConstitution
  | InfoAction
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

-- | What kind of off-chain document an anchor URL points at. Stored
-- in @voting_anchor.type@.
data AnchorType
  = GovActionAnchor
  | DrepAnchor
  | OtherAnchor
  | VoteAnchor
  | CommitteeDeRegAnchor
  | ConstitutionAnchor
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

-- ---------------------------------------------------------------------------
-- * Newtype wrappers
-- ---------------------------------------------------------------------------

-- | A pool metadata URL. Wrapped to avoid mixing it with arbitrary
-- 'Text' (e.g. an asset name).
newtype PoolUrl = PoolUrl { unPoolUrl :: Text }
  deriving stock (Eq, Ord)
  deriving newtype (Show, Read)

-- | A vote anchor URL. Wrapped for the same reason as 'PoolUrl'.
newtype VoteUrl = VoteUrl { unVoteUrl :: Text }
  deriving stock (Eq, Ord)
  deriving newtype (Show, Read)

-- | The raw binary hash of a vote metadata document.
newtype VoteMetaHash = VoteMetaHash { unVoteMetaHash :: ByteString }
  deriving stock (Eq, Ord)
  deriving newtype (Show, Read)

-- ---------------------------------------------------------------------------
-- * COPY encoding helpers
-- ---------------------------------------------------------------------------

-- | Encode a 'DbInt65' as a decimal-ASCII signed integer.
--
-- The wire format is just the 'Int64' decimal — PostgreSQL's
-- @numeric@ accepts that without ceremony. The bit-packing in
-- 'DbInt65' is a Haskell-side memory optimisation only.
{-# INLINE bInt65 #-}
bInt65 :: DbInt65 -> Builder
bInt65 = bInt64 . fromDbInt65

-- | Encode a 'Word128' as decimal ASCII.
--
-- The only column that uses this is @epoch.out_sum@, which can grow
-- past 'maxBound \@Word64' once the cumulative output sum of an
-- entire epoch is involved. PostgreSQL stores it as @numeric(39,0)@.
{-# INLINE bWord128 #-}
bWord128 :: Word128 -> Builder
bWord128 = byteString . BS8.pack . show . toInteger

-- ---------------------------------------------------------------------------
-- ** Per-enum COPY builders
-- ---------------------------------------------------------------------------
--
-- The PG strings here are the source of truth — the original schema's
-- @CHECK (column IN (…))@ constraints reject any other value, so a
-- mismatch between the Haskell constructor and the PG string is a
-- silent data-corruption bug.

bScriptPurpose :: ScriptPurpose -> Builder
bScriptPurpose = byteString . \case
  Spend   -> "spend"
  Mint    -> "mint"
  Cert    -> "cert"
  Rewrd   -> "reward"
  Vote    -> "vote"
  Propose -> "propose"

bScriptType :: ScriptType -> Builder
bScriptType = byteString . \case
  MultiSig -> "multisig"
  Timelock -> "timelock"
  PlutusV1 -> "plutusV1"
  PlutusV2 -> "plutusV2"
  PlutusV3 -> "plutusV3"
  PlutusV4 -> "plutusV4"

bRewardSource :: RewardSource -> Builder
bRewardSource = byteString . \case
  RwdLeader         -> "leader"
  RwdMember         -> "member"
  RwdReserves       -> "reserves"
  RwdTreasury       -> "treasury"
  RwdDepositRefund  -> "refund"
  RwdProposalRefund -> "proposal_refund"

bSyncState :: SyncState -> Builder
bSyncState = byteString . \case
  SyncLagging   -> "lagging"
  SyncFollowing -> "following"

bVote :: Vote -> Builder
bVote = byteString . \case
  VoteYes     -> "Yes"
  VoteNo      -> "No"
  VoteAbstain -> "Abstain"

bVoterRole :: VoterRole -> Builder
bVoterRole = byteString . \case
  ConstitutionalCommittee -> "ConstitutionalCommittee"
  DRep                    -> "DRep"
  SPO                     -> "SPO"

bGovActionType :: GovActionType -> Builder
bGovActionType = byteString . \case
  ParameterChange     -> "ParameterChange"
  HardForkInitiation  -> "HardForkInitiation"
  TreasuryWithdrawals -> "TreasuryWithdrawals"
  NoConfidence        -> "NoConfidence"
  NewCommitteeType    -> "NewCommittee"
  NewConstitution     -> "NewConstitution"
  InfoAction          -> "InfoAction"

bAnchorType :: AnchorType -> Builder
bAnchorType = byteString . \case
  GovActionAnchor      -> "gov_action"
  DrepAnchor           -> "drep"
  OtherAnchor          -> "other"
  VoteAnchor           -> "vote"
  CommitteeDeRegAnchor -> "committee_dereg"
  ConstitutionAnchor   -> "constitution"

-- ---------------------------------------------------------------------------
-- * Scientific / Word conversions
-- ---------------------------------------------------------------------------

-- $scientificConversions
-- Helpers for moving between unsigned 'Word' types and 'Sci.Scientific'.
-- 'Sci.toBoundedInteger' is exact when the value fits in the target
-- 'Bounded' range; the 'floor' fallback handles values PostgreSQL has
-- normalised (e.g. @Scientific 38 16@ instead of a large literal).

-- | 'Sci.Scientific' → 'Word64', exact when the value fits in
-- @[0, 'maxBound' \@Word64]@; falls back to 'floor' for values that
-- 'Sci.toBoundedInteger' rejects on bounds.
scientificToWord64 :: Sci.Scientific -> Word64
scientificToWord64 s = case Sci.toBoundedInteger s of
  Just w  -> w
  Nothing -> fromInteger (floor s)

-- | 'Sci.Scientific' → 'Word128', going through 'Integer' so the
-- @numeric@ exponent is honoured. 'Word128' is unbounded by 'Int64'
-- so 'Sci.toBoundedInteger' isn't useful here.
scientificToWord128 :: Sci.Scientific -> Word128
scientificToWord128 = fromInteger . floor

-- | 'Word64' → 'Sci.Scientific' with a base-10 exponent of zero —
-- the canonical encoding for an integer @numeric@ value.
word64ToScientific :: Word64 -> Sci.Scientific
word64ToScientific w = Sci.scientific (toInteger w) 0

-- | 'Word128' → 'Sci.Scientific'. Goes through 'Integer' since
-- 'Word128' has no 'Integral' instance.
word128ToScientific :: Word128 -> Sci.Scientific
word128ToScientific = fromInteger . toInteger

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders for numeric domain types
-- ---------------------------------------------------------------------------

-- | 'E.Value'-level encoder for 'DbLovelace' against a @numeric@ column.
-- Use this inside @mconcat@ encoders that need a 'E.Value' (e.g. when
-- the field is wrapped in 'E.nullable').
dbLovelaceValueEncoder :: E.Value DbLovelace
dbLovelaceValueEncoder = (word64ToScientific . unDbLovelace) >$< E.numeric

-- | 'D.Value'-level decoder for 'DbLovelace'. Pair to 'dbLovelaceValueEncoder'.
dbLovelaceValueDecoder :: D.Value DbLovelace
dbLovelaceValueDecoder = DbLovelace . scientificToWord64 <$> D.numeric

-- | 'E.Params'-level encoder for a non-nullable 'DbLovelace' column.
dbLovelaceEncoder :: E.Params DbLovelace
dbLovelaceEncoder = E.param (E.nonNullable dbLovelaceValueEncoder)

-- | Row decoder consuming exactly one non-null 'DbLovelace' column.
dbLovelaceDecoder :: D.Row DbLovelace
dbLovelaceDecoder = D.column (D.nonNullable dbLovelaceValueDecoder)

-- | 'E.Params'-level encoder for a nullable 'DbLovelace' column.
maybeDbLovelaceEncoder :: E.Params (Maybe DbLovelace)
maybeDbLovelaceEncoder = E.param (E.nullable dbLovelaceValueEncoder)

-- | Row decoder consuming exactly one nullable 'DbLovelace' column.
maybeDbLovelaceDecoder :: D.Row (Maybe DbLovelace)
maybeDbLovelaceDecoder = D.column (D.nullable dbLovelaceValueDecoder)

-- | 'E.Value'-level encoder for 'DbWord64' against a @numeric@ column.
dbWord64ValueEncoder :: E.Value DbWord64
dbWord64ValueEncoder = (word64ToScientific . unDbWord64) >$< E.numeric

-- | 'D.Value'-level decoder for 'DbWord64'. Pair to 'dbWord64ValueEncoder'.
dbWord64ValueDecoder :: D.Value DbWord64
dbWord64ValueDecoder = DbWord64 . scientificToWord64 <$> D.numeric

dbWord64Encoder :: E.Params DbWord64
dbWord64Encoder = E.param (E.nonNullable dbWord64ValueEncoder)

dbWord64Decoder :: D.Row DbWord64
dbWord64Decoder = D.column (D.nonNullable dbWord64ValueDecoder)

maybeDbWord64Encoder :: E.Params (Maybe DbWord64)
maybeDbWord64Encoder = E.param (E.nullable dbWord64ValueEncoder)

maybeDbWord64Decoder :: D.Row (Maybe DbWord64)
maybeDbWord64Decoder = D.column (D.nullable dbWord64ValueDecoder)

-- | 'DbInt65' is a sign-magnitude 'Word64' that always fits in 'Int64',
-- so it rides the @int8@ column type — not @numeric@. The schema
-- columns it backs (@ada_pots.*@, @pot_transfer.*@, @treasury.amount@,
-- @reserve.amount@) are still declared @numeric@; PostgreSQL accepts
-- the implicit @int8@ → @numeric@ cast on input, and we recover
-- exactness on read because individual rows always fit in 'Int64'.
dbInt65Encoder :: E.Value DbInt65
dbInt65Encoder = fromDbInt65 >$< E.int8

dbInt65Decoder :: D.Value DbInt65
dbInt65Decoder = toDbInt65 <$> D.int8

-- | 'Word128' encoder for the @epoch.out_sum@ column (and any other
-- @numeric(39,0)@ columns). Goes through 'Sci.Scientific' so values
-- larger than 'maxBound' @Word64' round-trip correctly.
word128Encoder :: E.Value Word128
word128Encoder = word128ToScientific >$< E.numeric

word128Decoder :: D.Value Word128
word128Decoder = scientificToWord128 <$> D.numeric
