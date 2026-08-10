{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Domain-specific newtypes, enum types, and COPY\/hasql encoding
-- helpers, shared by the @Schema.<Domain>@ modules.
--
-- __Encoding rule for @numeric@ columns:__ 'DbLovelace', 'DbWord64' and
-- 'Word128' must go through 'Sci.Scientific'. An @int8@ round-trip
-- silently truncates an aggregate such as @SUM(tx.out_sum)@ once it
-- passes @maxBound \@Int64@. The decoders use 'Sci.toBoundedInteger' and
-- 'floor', not 'Sci.coefficient', so a value PostgreSQL normalised — for
-- example @Scientific 38 16@ — reconstructs correctly.
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
  , bRational
  , bScriptPurpose
  , bScriptType
  , bRewardSource
  , bSyncState
  , bVote
  , bVoterRole
  , bGovActionType
  , bAnchorType

    -- * Rational-as-numeric hasql codecs (PostgreSQL @numeric@ column type)
  , rationalToScientific
  , rationalAsNumericEncoder
  , rationalAsNumericDecoder
  , maybeRationalAsNumericEncoder
  , maybeRationalAsNumericDecoder

    -- * Double-as-numeric hasql codecs (PostgreSQL @numeric@ column type)
  , doubleAsNumericEncoder
  , doubleAsNumericDecoder

    -- * Hasql encoders \/ decoders for enum types
  , scriptPurposeEncoder
  , scriptPurposeDecoder
  , scriptTypeEncoder
  , scriptTypeDecoder
  , rewardSourceEncoder
  , rewardSourceDecoder
  , voteEncoder
  , voteDecoder
  , voterRoleEncoder
  , voterRoleDecoder
  , govActionTypeEncoder
  , govActionTypeDecoder
  , anchorTypeEncoder
  , anchorTypeDecoder

    -- * Hasql encoders \/ decoders for newtype wrappers
  , voteUrlEncoder
  , voteUrlDecoder
  ) where

import Cardano.Prelude

import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Functor.Contravariant ((>$<))
import qualified Data.Scientific as Sci
import Data.WideWord (Word128)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import DbSync.Db.Loader.Encoder (bInt64)

-- ---------------------------------------------------------------------------
-- * Numeric domain types
-- ---------------------------------------------------------------------------

-- | Lovelace values stored as PostgreSQL @numeric(20,0)@. The newtype
-- keeps the column type unambiguous and carries values past @Int64@ range.
newtype DbLovelace = DbLovelace { unDbLovelace :: Word64 }
  deriving stock (Eq, Ord)
  deriving newtype (Show, Read)

-- | Large unsigned integers stored as PostgreSQL @numeric@, for
-- non-monetary columns such as @invalid_before@ and @invalid_hereafter@.
newtype DbWord64 = DbWord64 { unDbWord64 :: Word64 }
  deriving stock (Eq, Ord)
  deriving newtype (Show, Read, Num)

-- | A signed 65-bit integer stored as PostgreSQL @numeric@, for the
-- signed ledger amounts in @ada_pots@, @pot_transfer@,
-- @treasury.amount@, @reserve.amount@ and @ma_tx_mint.quantity@.
--
-- Bit 63 of the 'Word64' is the sign and the other 63 bits are the
-- magnitude. 'minBound' is special-cased as sign bit set with magnitude
-- zero, so it survives the 'abs' that would otherwise overflow.
--
-- Use 'toDbInt65' and 'fromDbInt65' rather than the constructor. The
-- derived 'Show' and 'Read' expose the raw bit pattern.
newtype DbInt65 = DbInt65 { unDbInt65 :: Word64 }
  deriving stock (Eq, Ord)
  deriving newtype (Show, Read)

-- | 'minBound' would overflow under negation, so it encodes as sign bit
-- set with magnitude zero. That is the only pattern 'fromDbInt65' maps
-- back to 'minBound'.
toDbInt65 :: Int64 -> DbInt65
toDbInt65 n
  | n >= 0          = DbInt65 (fromIntegral n)
  | n == minBound   = DbInt65 (setBit 0 63)
  | otherwise       = DbInt65 (setBit (fromIntegral (abs n)) 63)

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
-- Each enum has a COPY builder ('b\<Name>') and a hasql codec further
-- down. The two must emit the same strings.

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
-- @pot_reward.type@.
data RewardSource
  = RwdLeader
  | RwdMember
  | RwdReserves
  | RwdTreasury
  | RwdDepositRefund
  | RwdProposalRefund
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

instance NFData RewardSource where
  rnf RwdLeader = ()
  rnf RwdMember = ()
  rnf RwdReserves = ()
  rnf RwdTreasury = ()
  rnf RwdDepositRefund = ()
  rnf RwdProposalRefund = ()

-- | Whether the local tip is lagging or following the global chain
-- tip. Stored in the @epoch_sync_time.state@ column.
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

-- | The wire value is the plain 'Int64' decimal, which @numeric@ accepts.
-- The bit-packing in 'DbInt65' is a Haskell-side memory optimisation only.
{-# INLINE bInt65 #-}
bInt65 :: DbInt65 -> Builder
bInt65 = bInt64 . fromDbInt65

-- | Only @epoch.out_sum@ uses this. An epoch's cumulative output sum can
-- pass @maxBound \@Word64@, so the column is @numeric(39,0)@.
{-# INLINE bWord128 #-}
bWord128 :: Word128 -> Builder
bWord128 = byteString . BS8.pack . show . toInteger

-- | Writes fixed-notation decimal through 'rationalToScientific', so the
-- wire value matches 'rationalAsNumericEncoder'.
{-# INLINE bRational #-}
bRational :: Rational -> Builder
bRational =
  byteString . BS8.pack . Sci.formatScientific Sci.Fixed Nothing . rationalToScientific

-- ---------------------------------------------------------------------------
-- ** Per-enum COPY builders
-- ---------------------------------------------------------------------------
--
-- The strings here are the source of truth for the column values. The
-- columns are plain @text@ with no @CHECK@, so a mismatch between a
-- constructor and its string corrupts data silently.

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

-- | The 'E.Value' form, for use inside an @mconcat@ encoder that wraps the
-- field in 'E.nullable'.
dbLovelaceValueEncoder :: E.Value DbLovelace
dbLovelaceValueEncoder = (word64ToScientific . unDbLovelace) >$< E.numeric

dbLovelaceValueDecoder :: D.Value DbLovelace
dbLovelaceValueDecoder = DbLovelace . scientificToWord64 <$> D.numeric

dbLovelaceEncoder :: E.Params DbLovelace
dbLovelaceEncoder = E.param (E.nonNullable dbLovelaceValueEncoder)

dbLovelaceDecoder :: D.Row DbLovelace
dbLovelaceDecoder = D.column (D.nonNullable dbLovelaceValueDecoder)

maybeDbLovelaceEncoder :: E.Params (Maybe DbLovelace)
maybeDbLovelaceEncoder = E.param (E.nullable dbLovelaceValueEncoder)

maybeDbLovelaceDecoder :: D.Row (Maybe DbLovelace)
maybeDbLovelaceDecoder = D.column (D.nullable dbLovelaceValueDecoder)

dbWord64ValueEncoder :: E.Value DbWord64
dbWord64ValueEncoder = (word64ToScientific . unDbWord64) >$< E.numeric

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

-- | 'DbInt65' always fits in 'Int64', so this codec uses @int8@, not
-- @numeric@. The columns stay @numeric@: PostgreSQL applies the implicit
-- @int8@ → @numeric@ cast on input, and the read stays exact because one
-- row never exceeds 'Int64'.
dbInt65Encoder :: E.Value DbInt65
dbInt65Encoder = fromDbInt65 >$< E.int8

dbInt65Decoder :: D.Value DbInt65
dbInt65Decoder = toDbInt65 <$> D.int8

-- | For @epoch.out_sum@ and any other @numeric(39,0)@ column. It goes
-- through 'Sci.Scientific', so a value larger than @maxBound \@Word64@
-- round-trips correctly.
word128Encoder :: E.Value Word128
word128Encoder = word128ToScientific >$< E.numeric

word128Decoder :: D.Value Word128
word128Decoder = scientificToWord128 <$> D.numeric

-- | Fractional-digit cap when encoding a 'Rational' to @numeric@. 80
-- covers every terminating decimal with a 'Word64'-bounded denominator,
-- which is at most 63 digits, so a ledger rational encodes exactly. A
-- non-terminating expansion truncates here instead of looping.
rationalNumericScale :: Int
rationalNumericScale = 80

-- | Exact for decimals terminating within 'rationalNumericScale'
-- digits; truncated toward zero otherwise.
rationalToScientific :: Rational -> Sci.Scientific
rationalToScientific r =
  case Sci.fromRationalRepetend (Just rationalNumericScale) r of
    Right (s, Nothing) -> s
    _ -> Sci.scientific (truncate (r * 10 ^ rationalNumericScale)) (negate rationalNumericScale)

rationalAsNumericEncoder :: E.Value Rational
rationalAsNumericEncoder = rationalToScientific >$< E.numeric

-- | Exact: a @numeric@ value is always a terminating decimal.
rationalAsNumericDecoder :: D.Value Rational
rationalAsNumericDecoder = toRational <$> D.numeric

maybeRationalAsNumericEncoder :: E.Params (Maybe Rational)
maybeRationalAsNumericEncoder = E.param (E.nullable rationalAsNumericEncoder)

maybeRationalAsNumericDecoder :: D.Row (Maybe Rational)
maybeRationalAsNumericDecoder = D.column (D.nullable rationalAsNumericDecoder)

doubleAsNumericEncoder :: E.Value Double
doubleAsNumericEncoder = Sci.fromFloatDigits >$< E.numeric

doubleAsNumericDecoder :: D.Value Double
doubleAsNumericDecoder = Sci.toRealFloat <$> D.numeric

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders for enum types
-- ---------------------------------------------------------------------------
--
-- Every string here must match the per-enum COPY builder above. Drift
-- between the two corrupts data silently.

scriptPurposeEncoder :: E.Value ScriptPurpose
scriptPurposeEncoder = scriptPurposeToText >$< E.text
  where
    scriptPurposeToText = \case
      Spend   -> "spend"
      Mint    -> "mint"
      Cert    -> "cert"
      Rewrd   -> "reward"
      Vote    -> "vote"
      Propose -> "propose"

scriptPurposeDecoder :: D.Value ScriptPurpose
scriptPurposeDecoder = D.refine textToScriptPurpose D.text
  where
    textToScriptPurpose = \case
      "spend"   -> Right Spend
      "mint"    -> Right Mint
      "cert"    -> Right Cert
      "reward"  -> Right Rewrd
      "vote"    -> Right Vote
      "propose" -> Right Propose
      other     -> Left $ "unknown ScriptPurpose: " <> other

scriptTypeEncoder :: E.Value ScriptType
scriptTypeEncoder = scriptTypeToText >$< E.text
  where
    scriptTypeToText = \case
      MultiSig -> "multisig"
      Timelock -> "timelock"
      PlutusV1 -> "plutusV1"
      PlutusV2 -> "plutusV2"
      PlutusV3 -> "plutusV3"
      PlutusV4 -> "plutusV4"

scriptTypeDecoder :: D.Value ScriptType
scriptTypeDecoder = D.refine textToScriptType D.text
  where
    textToScriptType = \case
      "multisig" -> Right MultiSig
      "timelock" -> Right Timelock
      "plutusV1" -> Right PlutusV1
      "plutusV2" -> Right PlutusV2
      "plutusV3" -> Right PlutusV3
      "plutusV4" -> Right PlutusV4
      other      -> Left $ "unknown ScriptType: " <> other

rewardSourceEncoder :: E.Value RewardSource
rewardSourceEncoder = rewardSourceToText >$< E.text
  where
    rewardSourceToText = \case
      RwdLeader         -> "leader"
      RwdMember         -> "member"
      RwdReserves       -> "reserves"
      RwdTreasury       -> "treasury"
      RwdDepositRefund  -> "refund"
      RwdProposalRefund -> "proposal_refund"

rewardSourceDecoder :: D.Value RewardSource
rewardSourceDecoder = D.refine textToRewardSource D.text
  where
    textToRewardSource = \case
      "leader"          -> Right RwdLeader
      "member"          -> Right RwdMember
      "reserves"        -> Right RwdReserves
      "treasury"        -> Right RwdTreasury
      "refund"          -> Right RwdDepositRefund
      "proposal_refund" -> Right RwdProposalRefund
      other             -> Left $ "unknown RewardSource: " <> other

voteEncoder :: E.Value Vote
voteEncoder = voteToText >$< E.text
  where
    voteToText = \case
      VoteYes     -> "Yes"
      VoteNo      -> "No"
      VoteAbstain -> "Abstain"

voteDecoder :: D.Value Vote
voteDecoder = D.refine textToVote D.text
  where
    textToVote = \case
      "Yes"     -> Right VoteYes
      "No"      -> Right VoteNo
      "Abstain" -> Right VoteAbstain
      other     -> Left $ "unknown Vote: " <> other

voterRoleEncoder :: E.Value VoterRole
voterRoleEncoder = voterRoleToText >$< E.text
  where
    voterRoleToText = \case
      ConstitutionalCommittee -> "ConstitutionalCommittee"
      DRep                    -> "DRep"
      SPO                     -> "SPO"

voterRoleDecoder :: D.Value VoterRole
voterRoleDecoder = D.refine textToVoterRole D.text
  where
    textToVoterRole = \case
      "ConstitutionalCommittee" -> Right ConstitutionalCommittee
      "DRep"                    -> Right DRep
      "SPO"                     -> Right SPO
      other                     -> Left $ "unknown VoterRole: " <> other

govActionTypeEncoder :: E.Value GovActionType
govActionTypeEncoder = govActionTypeToText >$< E.text
  where
    govActionTypeToText = \case
      ParameterChange     -> "ParameterChange"
      HardForkInitiation  -> "HardForkInitiation"
      TreasuryWithdrawals -> "TreasuryWithdrawals"
      NoConfidence        -> "NoConfidence"
      NewCommitteeType    -> "NewCommittee"
      NewConstitution     -> "NewConstitution"
      InfoAction          -> "InfoAction"

govActionTypeDecoder :: D.Value GovActionType
govActionTypeDecoder = D.refine textToGovActionType D.text
  where
    textToGovActionType = \case
      "ParameterChange"     -> Right ParameterChange
      "HardForkInitiation"  -> Right HardForkInitiation
      "TreasuryWithdrawals" -> Right TreasuryWithdrawals
      "NoConfidence"        -> Right NoConfidence
      "NewCommittee"        -> Right NewCommitteeType
      "NewConstitution"     -> Right NewConstitution
      "InfoAction"          -> Right InfoAction
      other                 -> Left $ "unknown GovActionType: " <> other

anchorTypeEncoder :: E.Value AnchorType
anchorTypeEncoder = anchorTypeToText >$< E.text
  where
    anchorTypeToText = \case
      GovActionAnchor      -> "gov_action"
      DrepAnchor           -> "drep"
      OtherAnchor          -> "other"
      VoteAnchor           -> "vote"
      CommitteeDeRegAnchor -> "committee_dereg"
      ConstitutionAnchor   -> "constitution"

anchorTypeDecoder :: D.Value AnchorType
anchorTypeDecoder = D.refine textToAnchorType D.text
  where
    textToAnchorType = \case
      "gov_action"      -> Right GovActionAnchor
      "drep"            -> Right DrepAnchor
      "other"           -> Right OtherAnchor
      "vote"            -> Right VoteAnchor
      "committee_dereg" -> Right CommitteeDeRegAnchor
      "constitution"    -> Right ConstitutionAnchor
      other             -> Left $ "unknown AnchorType: " <> other

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders for newtype wrappers
-- ---------------------------------------------------------------------------

-- | 'VoteUrl' rides a plain @text@ column; the newtype adds no wire
-- ceremony.
voteUrlEncoder :: E.Value VoteUrl
voteUrlEncoder = unVoteUrl >$< E.text

voteUrlDecoder :: D.Value VoteUrl
voteUrlDecoder = VoteUrl <$> D.text
