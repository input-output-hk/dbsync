{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema types for the @off_chain_vote_*@ tables. Seven identity
-- leaves written by the off-chain vote worker: @off_chain_vote_data@
-- as the parent row, five per-document subtables for the parsed CIP
-- fields, and @off_chain_vote_fetch_error@ for failed attempts.
module DbSync.Db.Schema.OffChainVote
  ( -- * Schema types
    OffChainVoteData (..)
  , OffChainVoteGovActionData (..)
  , OffChainVoteDrepData (..)
  , OffChainVoteAuthor (..)
  , OffChainVoteReference (..)
  , OffChainVoteExternalUpdate (..)
  , OffChainVoteFetchError (..)

    -- * Table definitions
  , offChainVoteDataTableDef
  , offChainVoteGovActionDataTableDef
  , offChainVoteDrepDataTableDef
  , offChainVoteAuthorTableDef
  , offChainVoteReferenceTableDef
  , offChainVoteExternalUpdateTableDef
  , offChainVoteFetchErrorTableDef

    -- * COPY encoding
  , encodeOffChainVoteDataCopy
  , encodeOffChainVoteGovActionDataCopy
  , encodeOffChainVoteDrepDataCopy
  , encodeOffChainVoteAuthorCopy
  , encodeOffChainVoteReferenceCopy
  , encodeOffChainVoteExternalUpdateCopy
  , encodeOffChainVoteFetchErrorCopy

    -- * Hasql encoders \/ decoders
  , offChainVoteDataEncoder
  , offChainVoteDataDecoder
  , entityOffChainVoteDataDecoder
  , offChainVoteGovActionDataEncoder
  , offChainVoteGovActionDataDecoder
  , entityOffChainVoteGovActionDataDecoder
  , offChainVoteDrepDataEncoder
  , offChainVoteDrepDataDecoder
  , entityOffChainVoteDrepDataDecoder
  , offChainVoteAuthorEncoder
  , offChainVoteAuthorDecoder
  , entityOffChainVoteAuthorDecoder
  , offChainVoteReferenceEncoder
  , offChainVoteReferenceDecoder
  , entityOffChainVoteReferenceDecoder
  , offChainVoteExternalUpdateEncoder
  , offChainVoteExternalUpdateDecoder
  , entityOffChainVoteExternalUpdateDecoder
  , offChainVoteFetchErrorEncoder
  , offChainVoteFetchErrorDecoder
  , entityOffChainVoteFetchErrorDecoder
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (localTimeToUTC, utc, utcToLocalTime)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
import DbSync.Db.Loader.Encoder (buildCopyRow, bBool, bHex, bInt64, bText, bUTCTime, bWord64)

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key OffChainVoteData           = OffChainVoteDataId
type instance Key OffChainVoteGovActionData  = OffChainVoteGovActionDataId
type instance Key OffChainVoteDrepData       = OffChainVoteDrepDataId
type instance Key OffChainVoteAuthor         = OffChainVoteAuthorId
type instance Key OffChainVoteReference      = OffChainVoteReferenceId
type instance Key OffChainVoteExternalUpdate = OffChainVoteExternalUpdateId
type instance Key OffChainVoteFetchError     = OffChainVoteFetchErrorId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | The @off_chain_vote_data@ table. One row per successful anchor
-- fetch; unique on @(hash, voting_anchor_id)@.
--
-- @is_valid@ encodes the parse outcome:
--   * 'Just' 'True'  — JSON parsed and matched the CIP schema.
--   * 'Just' 'False' — JSON parsed but schema mismatch; subtables empty.
--   * 'Nothing'      — body was not valid JSON.
data OffChainVoteData = OffChainVoteData
  { offChainVoteDataVotingAnchorId :: !VotingAnchorId
  , offChainVoteDataHash           :: !ByteString
  , offChainVoteDataJson           :: !Text
  , offChainVoteDataBytes          :: !ByteString
  , offChainVoteDataWarning        :: !(Maybe Text)
  , offChainVoteDataLanguage       :: !Text
  , offChainVoteDataComment        :: !(Maybe Text)
  , offChainVoteDataIsValid        :: !(Maybe Bool)
  }
  deriving stock (Eq, Show)

-- | The @off_chain_vote_gov_action_data@ table. CIP-108 governance
-- proposal text fields. One row per parent vote-data row.
data OffChainVoteGovActionData = OffChainVoteGovActionData
  { offChainVoteGovActionDataOffChainVoteDataId :: !OffChainVoteDataId
  , offChainVoteGovActionDataTitle              :: !Text
  , offChainVoteGovActionDataAbstract           :: !Text
  , offChainVoteGovActionDataMotivation         :: !Text
  , offChainVoteGovActionDataRationale          :: !Text
  }
  deriving stock (Eq, Show)

-- | The @off_chain_vote_drep_data@ table. CIP-119 DRep metadata
-- attached to a vote-data row.
data OffChainVoteDrepData = OffChainVoteDrepData
  { offChainVoteDrepDataOffChainVoteDataId :: !OffChainVoteDataId
  , offChainVoteDrepDataPaymentAddress     :: !(Maybe Text)
  , offChainVoteDrepDataGivenName          :: !Text
  , offChainVoteDrepDataObjectives         :: !(Maybe Text)
  , offChainVoteDrepDataMotivations        :: !(Maybe Text)
  , offChainVoteDrepDataQualifications     :: !(Maybe Text)
  , offChainVoteDrepDataImageUrl           :: !(Maybe Text)
  , offChainVoteDrepDataImageHash          :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

-- | The @off_chain_vote_author@ table. Author signature attached to a
-- vote-data row.
data OffChainVoteAuthor = OffChainVoteAuthor
  { offChainVoteAuthorOffChainVoteDataId :: !OffChainVoteDataId
  , offChainVoteAuthorName               :: !(Maybe Text)
  , offChainVoteAuthorWitnessAlgorithm   :: !Text
  , offChainVoteAuthorPublicKey          :: !Text
  , offChainVoteAuthorSignature          :: !Text
  , offChainVoteAuthorWarning            :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

-- | The @off_chain_vote_reference@ table. One row per referenced URI.
data OffChainVoteReference = OffChainVoteReference
  { offChainVoteReferenceOffChainVoteDataId :: !OffChainVoteDataId
  , offChainVoteReferenceLabel              :: !Text
  , offChainVoteReferenceUri                :: !Text
  , offChainVoteReferenceHashDigest         :: !(Maybe Text)
  , offChainVoteReferenceHashAlgorithm      :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

-- | The @off_chain_vote_external_update@ table. One row per external
-- update link in the anchor document.
data OffChainVoteExternalUpdate = OffChainVoteExternalUpdate
  { offChainVoteExternalUpdateOffChainVoteDataId :: !OffChainVoteDataId
  , offChainVoteExternalUpdateTitle              :: !Text
  , offChainVoteExternalUpdateUri                :: !Text
  }
  deriving stock (Eq, Show)

-- | The @off_chain_vote_fetch_error@ table. One row per failed
-- attempt; unique on @(voting_anchor_id, retry_count)@.
data OffChainVoteFetchError = OffChainVoteFetchError
  { offChainVoteFetchErrorVotingAnchorId :: !VotingAnchorId
  , offChainVoteFetchErrorFetchError     :: !Text
  , offChainVoteFetchErrorFetchTime      :: !UTCTime
  , offChainVoteFetchErrorRetryCount     :: !Word64
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definitions
-- ---------------------------------------------------------------------------

offChainVoteDataTableDef :: TableDef
offChainVoteDataTableDef = TableDef
  { tdName    = "off_chain_vote_data"
  , tdColumns =
      [ ColumnDef "id"               PgBigInt  False
      , ColumnDef "voting_anchor_id" PgBigInt  False
      , ColumnDef "hash"             PgBytea   False
      , ColumnDef "json"             PgJsonb   False
      , ColumnDef "bytes"            PgBytea   False
      , ColumnDef "warning"          PgText    True
      , ColumnDef "language"         PgText    False
      , ColumnDef "comment"          PgText    True
      , ColumnDef "is_valid"         PgBoolean True
      ]
  , tdMode              = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = ["hash" :| ["voting_anchor_id"]]
  , tdGeneratedColumns  = []
  , tdIdentityColumns   = ["id"]
  , tdForeignKeys       = []
  }

offChainVoteGovActionDataTableDef :: TableDef
offChainVoteGovActionDataTableDef = TableDef
  { tdName    = "off_chain_vote_gov_action_data"
  , tdColumns =
      [ ColumnDef "id"                      PgBigInt False
      , ColumnDef "off_chain_vote_data_id"  PgBigInt False
      , ColumnDef "title"                   PgText   False
      , ColumnDef "abstract"                PgText   False
      , ColumnDef "motivation"              PgText   False
      , ColumnDef "rationale"               PgText   False
      ]
  , tdMode              = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns  = []
  , tdIdentityColumns   = ["id"]
  , tdForeignKeys       = []
  }

offChainVoteDrepDataTableDef :: TableDef
offChainVoteDrepDataTableDef = TableDef
  { tdName    = "off_chain_vote_drep_data"
  , tdColumns =
      [ ColumnDef "id"                      PgBigInt False
      , ColumnDef "off_chain_vote_data_id"  PgBigInt False
      , ColumnDef "payment_address"         PgText   True
      , ColumnDef "given_name"              PgText   False
      , ColumnDef "objectives"              PgText   True
      , ColumnDef "motivations"             PgText   True
      , ColumnDef "qualifications"          PgText   True
      , ColumnDef "image_url"               PgText   True
      , ColumnDef "image_hash"              PgText   True
      ]
  , tdMode              = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns  = []
  , tdIdentityColumns   = ["id"]
  , tdForeignKeys       = []
  }

offChainVoteAuthorTableDef :: TableDef
offChainVoteAuthorTableDef = TableDef
  { tdName    = "off_chain_vote_author"
  , tdColumns =
      [ ColumnDef "id"                      PgBigInt False
      , ColumnDef "off_chain_vote_data_id"  PgBigInt False
      , ColumnDef "name"                    PgText   True
      , ColumnDef "witness_algorithm"       PgText   False
      , ColumnDef "public_key"              PgText   False
      , ColumnDef "signature"               PgText   False
      , ColumnDef "warning"                 PgText   True
      ]
  , tdMode              = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns  = []
  , tdIdentityColumns   = ["id"]
  , tdForeignKeys       = []
  }

offChainVoteReferenceTableDef :: TableDef
offChainVoteReferenceTableDef = TableDef
  { tdName    = "off_chain_vote_reference"
  , tdColumns =
      [ ColumnDef "id"                      PgBigInt False
      , ColumnDef "off_chain_vote_data_id"  PgBigInt False
      , ColumnDef "label"                   PgText   False
      , ColumnDef "uri"                     PgText   False
      , ColumnDef "hash_digest"             PgText   True
      , ColumnDef "hash_algorithm"          PgText   True
      ]
  , tdMode              = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns  = []
  , tdIdentityColumns   = ["id"]
  , tdForeignKeys       = []
  }

offChainVoteExternalUpdateTableDef :: TableDef
offChainVoteExternalUpdateTableDef = TableDef
  { tdName    = "off_chain_vote_external_update"
  , tdColumns =
      [ ColumnDef "id"                      PgBigInt False
      , ColumnDef "off_chain_vote_data_id"  PgBigInt False
      , ColumnDef "title"                   PgText   False
      , ColumnDef "uri"                     PgText   False
      ]
  , tdMode              = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns  = []
  , tdIdentityColumns   = ["id"]
  , tdForeignKeys       = []
  }

offChainVoteFetchErrorTableDef :: TableDef
offChainVoteFetchErrorTableDef = TableDef
  { tdName    = "off_chain_vote_fetch_error"
  , tdColumns =
      [ ColumnDef "id"               PgBigInt    False
      , ColumnDef "voting_anchor_id" PgBigInt    False
      , ColumnDef "fetch_error"      PgText      False
      , ColumnDef "fetch_time"       PgTimestamp False
      , ColumnDef "retry_count"      PgBigInt    False
      ]
  , tdMode              = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = ["voting_anchor_id" :| ["retry_count"]]
  , tdGeneratedColumns  = []
  , tdIdentityColumns   = ["id"]
  , tdForeignKeys       = []
  }

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodeOffChainVoteDataCopy :: OffChainVoteData -> ByteString
encodeOffChainVoteDataCopy d =
  buildCopyRow
    [ Just $ bInt64 (getVotingAnchorId (offChainVoteDataVotingAnchorId d))
    , Just $ bHex (offChainVoteDataHash d)
    , Just $ bText (offChainVoteDataJson d)
    , Just $ bHex (offChainVoteDataBytes d)
    , bText <$> offChainVoteDataWarning d
    , Just $ bText (offChainVoteDataLanguage d)
    , bText <$> offChainVoteDataComment d
    , bBool <$> offChainVoteDataIsValid d
    ]

encodeOffChainVoteGovActionDataCopy :: OffChainVoteGovActionData -> ByteString
encodeOffChainVoteGovActionDataCopy g =
  buildCopyRow
    [ Just $ bInt64 (getOffChainVoteDataId (offChainVoteGovActionDataOffChainVoteDataId g))
    , Just $ bText (offChainVoteGovActionDataTitle g)
    , Just $ bText (offChainVoteGovActionDataAbstract g)
    , Just $ bText (offChainVoteGovActionDataMotivation g)
    , Just $ bText (offChainVoteGovActionDataRationale g)
    ]

encodeOffChainVoteDrepDataCopy :: OffChainVoteDrepData -> ByteString
encodeOffChainVoteDrepDataCopy d =
  buildCopyRow
    [ Just $ bInt64 (getOffChainVoteDataId (offChainVoteDrepDataOffChainVoteDataId d))
    , bText <$> offChainVoteDrepDataPaymentAddress d
    , Just $ bText (offChainVoteDrepDataGivenName d)
    , bText <$> offChainVoteDrepDataObjectives d
    , bText <$> offChainVoteDrepDataMotivations d
    , bText <$> offChainVoteDrepDataQualifications d
    , bText <$> offChainVoteDrepDataImageUrl d
    , bText <$> offChainVoteDrepDataImageHash d
    ]

encodeOffChainVoteAuthorCopy :: OffChainVoteAuthor -> ByteString
encodeOffChainVoteAuthorCopy a =
  buildCopyRow
    [ Just $ bInt64 (getOffChainVoteDataId (offChainVoteAuthorOffChainVoteDataId a))
    , bText <$> offChainVoteAuthorName a
    , Just $ bText (offChainVoteAuthorWitnessAlgorithm a)
    , Just $ bText (offChainVoteAuthorPublicKey a)
    , Just $ bText (offChainVoteAuthorSignature a)
    , bText <$> offChainVoteAuthorWarning a
    ]

encodeOffChainVoteReferenceCopy :: OffChainVoteReference -> ByteString
encodeOffChainVoteReferenceCopy r =
  buildCopyRow
    [ Just $ bInt64 (getOffChainVoteDataId (offChainVoteReferenceOffChainVoteDataId r))
    , Just $ bText (offChainVoteReferenceLabel r)
    , Just $ bText (offChainVoteReferenceUri r)
    , bText <$> offChainVoteReferenceHashDigest r
    , bText <$> offChainVoteReferenceHashAlgorithm r
    ]

encodeOffChainVoteExternalUpdateCopy :: OffChainVoteExternalUpdate -> ByteString
encodeOffChainVoteExternalUpdateCopy u =
  buildCopyRow
    [ Just $ bInt64 (getOffChainVoteDataId (offChainVoteExternalUpdateOffChainVoteDataId u))
    , Just $ bText (offChainVoteExternalUpdateTitle u)
    , Just $ bText (offChainVoteExternalUpdateUri u)
    ]

encodeOffChainVoteFetchErrorCopy :: OffChainVoteFetchError -> ByteString
encodeOffChainVoteFetchErrorCopy e =
  buildCopyRow
    [ Just $ bInt64 (getVotingAnchorId (offChainVoteFetchErrorVotingAnchorId e))
    , Just $ bText (offChainVoteFetchErrorFetchError e)
    , Just $ bUTCTime (offChainVoteFetchErrorFetchTime e)
    , Just $ bWord64 (offChainVoteFetchErrorRetryCount e)
    ]

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------

-- | The @json@ column is @jsonb@; the Haskell-side representation stays
-- 'Text'. PostgreSQL accepts raw JSON bytes through 'E.jsonbBytes'.
jsonbTextEncoder :: E.Value Text
jsonbTextEncoder = TE.encodeUtf8 >$< E.jsonbBytes

jsonbTextDecoder :: D.Value Text
jsonbTextDecoder = D.jsonbBytes $ \bs ->
  case TE.decodeUtf8' bs of
    Right t -> Right t
    Left  e -> Left (show e)

utcTimeAsTimestampEncoder :: E.Value UTCTime
utcTimeAsTimestampEncoder = utcToLocalTime utc >$< E.timestamp

utcTimeAsTimestampDecoder :: D.Value UTCTime
utcTimeAsTimestampDecoder = localTimeToUTC utc <$> D.timestamp

-- off_chain_vote_data --------------------------------------------------------

offChainVoteDataEncoder :: E.Params OffChainVoteData
offChainVoteDataEncoder = mconcat
  [ offChainVoteDataVotingAnchorId >$< idEncoder getVotingAnchorId
  , offChainVoteDataHash           >$< E.param (E.nonNullable E.bytea)
  , offChainVoteDataJson           >$< E.param (E.nonNullable jsonbTextEncoder)
  , offChainVoteDataBytes          >$< E.param (E.nonNullable E.bytea)
  , offChainVoteDataWarning        >$< E.param (E.nullable E.text)
  , offChainVoteDataLanguage       >$< E.param (E.nonNullable E.text)
  , offChainVoteDataComment        >$< E.param (E.nullable E.text)
  , offChainVoteDataIsValid        >$< E.param (E.nullable E.bool)
  ]

offChainVoteDataDecoder :: D.Row OffChainVoteData
offChainVoteDataDecoder = OffChainVoteData
  <$> idDecoder VotingAnchorId
  <*> D.column (D.nonNullable D.bytea)
  <*> D.column (D.nonNullable jsonbTextDecoder)
  <*> D.column (D.nonNullable D.bytea)
  <*> D.column (D.nullable D.text)
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nullable D.text)
  <*> D.column (D.nullable D.bool)

entityOffChainVoteDataDecoder :: D.Row (OffChainVoteDataId, OffChainVoteData)
entityOffChainVoteDataDecoder = (,)
  <$> idDecoder OffChainVoteDataId
  <*> offChainVoteDataDecoder

-- off_chain_vote_gov_action_data --------------------------------------------

offChainVoteGovActionDataEncoder :: E.Params OffChainVoteGovActionData
offChainVoteGovActionDataEncoder = mconcat
  [ offChainVoteGovActionDataOffChainVoteDataId >$< idEncoder getOffChainVoteDataId
  , offChainVoteGovActionDataTitle              >$< E.param (E.nonNullable E.text)
  , offChainVoteGovActionDataAbstract           >$< E.param (E.nonNullable E.text)
  , offChainVoteGovActionDataMotivation         >$< E.param (E.nonNullable E.text)
  , offChainVoteGovActionDataRationale          >$< E.param (E.nonNullable E.text)
  ]

offChainVoteGovActionDataDecoder :: D.Row OffChainVoteGovActionData
offChainVoteGovActionDataDecoder = OffChainVoteGovActionData
  <$> idDecoder OffChainVoteDataId
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable D.text)

entityOffChainVoteGovActionDataDecoder
  :: D.Row (OffChainVoteGovActionDataId, OffChainVoteGovActionData)
entityOffChainVoteGovActionDataDecoder = (,)
  <$> idDecoder OffChainVoteGovActionDataId
  <*> offChainVoteGovActionDataDecoder

-- off_chain_vote_drep_data --------------------------------------------------

offChainVoteDrepDataEncoder :: E.Params OffChainVoteDrepData
offChainVoteDrepDataEncoder = mconcat
  [ offChainVoteDrepDataOffChainVoteDataId >$< idEncoder getOffChainVoteDataId
  , offChainVoteDrepDataPaymentAddress     >$< E.param (E.nullable E.text)
  , offChainVoteDrepDataGivenName          >$< E.param (E.nonNullable E.text)
  , offChainVoteDrepDataObjectives         >$< E.param (E.nullable E.text)
  , offChainVoteDrepDataMotivations        >$< E.param (E.nullable E.text)
  , offChainVoteDrepDataQualifications     >$< E.param (E.nullable E.text)
  , offChainVoteDrepDataImageUrl           >$< E.param (E.nullable E.text)
  , offChainVoteDrepDataImageHash          >$< E.param (E.nullable E.text)
  ]

offChainVoteDrepDataDecoder :: D.Row OffChainVoteDrepData
offChainVoteDrepDataDecoder = OffChainVoteDrepData
  <$> idDecoder OffChainVoteDataId
  <*> D.column (D.nullable D.text)
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nullable D.text)
  <*> D.column (D.nullable D.text)
  <*> D.column (D.nullable D.text)
  <*> D.column (D.nullable D.text)
  <*> D.column (D.nullable D.text)

entityOffChainVoteDrepDataDecoder
  :: D.Row (OffChainVoteDrepDataId, OffChainVoteDrepData)
entityOffChainVoteDrepDataDecoder = (,)
  <$> idDecoder OffChainVoteDrepDataId
  <*> offChainVoteDrepDataDecoder

-- off_chain_vote_author -----------------------------------------------------

offChainVoteAuthorEncoder :: E.Params OffChainVoteAuthor
offChainVoteAuthorEncoder = mconcat
  [ offChainVoteAuthorOffChainVoteDataId >$< idEncoder getOffChainVoteDataId
  , offChainVoteAuthorName               >$< E.param (E.nullable E.text)
  , offChainVoteAuthorWitnessAlgorithm   >$< E.param (E.nonNullable E.text)
  , offChainVoteAuthorPublicKey          >$< E.param (E.nonNullable E.text)
  , offChainVoteAuthorSignature          >$< E.param (E.nonNullable E.text)
  , offChainVoteAuthorWarning            >$< E.param (E.nullable E.text)
  ]

offChainVoteAuthorDecoder :: D.Row OffChainVoteAuthor
offChainVoteAuthorDecoder = OffChainVoteAuthor
  <$> idDecoder OffChainVoteDataId
  <*> D.column (D.nullable D.text)
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nullable D.text)

entityOffChainVoteAuthorDecoder
  :: D.Row (OffChainVoteAuthorId, OffChainVoteAuthor)
entityOffChainVoteAuthorDecoder = (,)
  <$> idDecoder OffChainVoteAuthorId
  <*> offChainVoteAuthorDecoder

-- off_chain_vote_reference --------------------------------------------------

offChainVoteReferenceEncoder :: E.Params OffChainVoteReference
offChainVoteReferenceEncoder = mconcat
  [ offChainVoteReferenceOffChainVoteDataId >$< idEncoder getOffChainVoteDataId
  , offChainVoteReferenceLabel              >$< E.param (E.nonNullable E.text)
  , offChainVoteReferenceUri                >$< E.param (E.nonNullable E.text)
  , offChainVoteReferenceHashDigest         >$< E.param (E.nullable E.text)
  , offChainVoteReferenceHashAlgorithm      >$< E.param (E.nullable E.text)
  ]

offChainVoteReferenceDecoder :: D.Row OffChainVoteReference
offChainVoteReferenceDecoder = OffChainVoteReference
  <$> idDecoder OffChainVoteDataId
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nullable D.text)
  <*> D.column (D.nullable D.text)

entityOffChainVoteReferenceDecoder
  :: D.Row (OffChainVoteReferenceId, OffChainVoteReference)
entityOffChainVoteReferenceDecoder = (,)
  <$> idDecoder OffChainVoteReferenceId
  <*> offChainVoteReferenceDecoder

-- off_chain_vote_external_update --------------------------------------------

offChainVoteExternalUpdateEncoder :: E.Params OffChainVoteExternalUpdate
offChainVoteExternalUpdateEncoder = mconcat
  [ offChainVoteExternalUpdateOffChainVoteDataId >$< idEncoder getOffChainVoteDataId
  , offChainVoteExternalUpdateTitle              >$< E.param (E.nonNullable E.text)
  , offChainVoteExternalUpdateUri                >$< E.param (E.nonNullable E.text)
  ]

offChainVoteExternalUpdateDecoder :: D.Row OffChainVoteExternalUpdate
offChainVoteExternalUpdateDecoder = OffChainVoteExternalUpdate
  <$> idDecoder OffChainVoteDataId
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable D.text)

entityOffChainVoteExternalUpdateDecoder
  :: D.Row (OffChainVoteExternalUpdateId, OffChainVoteExternalUpdate)
entityOffChainVoteExternalUpdateDecoder = (,)
  <$> idDecoder OffChainVoteExternalUpdateId
  <*> offChainVoteExternalUpdateDecoder

-- off_chain_vote_fetch_error ------------------------------------------------

offChainVoteFetchErrorEncoder :: E.Params OffChainVoteFetchError
offChainVoteFetchErrorEncoder = mconcat
  [ offChainVoteFetchErrorVotingAnchorId >$< idEncoder getVotingAnchorId
  , offChainVoteFetchErrorFetchError     >$< E.param (E.nonNullable E.text)
  , offChainVoteFetchErrorFetchTime      >$< E.param (E.nonNullable utcTimeAsTimestampEncoder)
  , offChainVoteFetchErrorRetryCount     >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  ]

offChainVoteFetchErrorDecoder :: D.Row OffChainVoteFetchError
offChainVoteFetchErrorDecoder = OffChainVoteFetchError
  <$> idDecoder VotingAnchorId
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable utcTimeAsTimestampDecoder)
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))

entityOffChainVoteFetchErrorDecoder
  :: D.Row (OffChainVoteFetchErrorId, OffChainVoteFetchError)
entityOffChainVoteFetchErrorDecoder = (,)
  <$> idDecoder OffChainVoteFetchErrorId
  <*> offChainVoteFetchErrorDecoder
