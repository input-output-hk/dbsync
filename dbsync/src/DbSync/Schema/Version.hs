-- | Schema identity of this binary: the global schema version and the
-- fingerprint that ties each released version to an exact schema
-- shape. CI and boot both compare the fingerprint, so drift fails
-- early instead of breaking a query mid-sync.
module DbSync.Schema.Version
  ( -- * Version
    currentSchemaVersion
  , releasedSchemaFingerprints

    -- * Fingerprint
  , Fingerprint (..)
  , schemaFingerprint
  ) where

import Cardano.Crypto.Hash (Blake2b_256, hashToTextAsHex, hashWith)
import Cardano.Prelude
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , ParentRef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )

-- ---------------------------------------------------------------------------
-- * Version
-- ---------------------------------------------------------------------------

-- | Schema version this binary produces. Bump on any change to the
-- declared schema, and add a matching entry to both
-- 'releasedSchemaFingerprints' and the migration ladder.
currentSchemaVersion :: Int
currentSchemaVersion = 1

-- | Pinned fingerprint of every released schema version. CI asserts that
-- the declared schema still hashes to the pin for 'currentSchemaVersion',
-- so editing a table without bumping the version turns the build red.
--
-- Entries for released versions are frozen; only the entry for a version
-- still in development may be refreshed.
releasedSchemaFingerprints :: [(Int, Fingerprint)]
releasedSchemaFingerprints =
  [ (1, Fingerprint "8558e1b8c162e14bd081be2b320e4077d9a75293d18f739243a1c658a5bc7283")
  ]

-- ---------------------------------------------------------------------------
-- * Fingerprint
-- ---------------------------------------------------------------------------

-- | Hex-encoded Blake2b-256 of the canonical schema rendering.
newtype Fingerprint = Fingerprint {unFingerprint :: Text}
  deriving stock (Eq, Show)

-- | Hash every table, sorted by name so input order is irrelevant,
-- plus any raw DDL outside 'TableDef' such as the epoch views. The
-- rendering spells out each 'TableDef' field, so renaming a Haskell
-- field cannot change the hash.
schemaFingerprint :: [TableDef] -> [Text] -> Fingerprint
schemaFingerprint tables extraDdl =
  Fingerprint . hashToTextAsHex . hashWith @Blake2b_256 TE.encodeUtf8 $
    T.intercalate "\n" (map renderTable (L.sortOn tdName tables) <> extraDdl)

renderTable :: TableDef -> Text
renderTable td =
  T.unlines $
    ("table=" <> tdName td <> " mode=" <> renderMode (tdMode td))
      : map renderColumn (tdColumns td)
        <> [ "primary_key=" <> maybe "-" (T.intercalate ",") (tdPrimaryKey td)
           , "checks=" <> T.intercalate ";" (tdChecks td)
           , "column_defaults=" <> renderPairs (tdColumnDefaults td)
           , "uniques=" <> T.intercalate ";" (map (T.intercalate "," . NE.toList) (tdUniqueConstraints td))
           , "generated_columns=" <> renderPairs (tdGeneratedColumns td)
           , "identity_columns=" <> T.intercalate "," (tdIdentityColumns td)
           , "parent_refs=" <> T.intercalate ";" (map renderParentRef (tdParentRefs td))
           ]

renderColumn :: ColumnDef -> Text
renderColumn c =
  "column=" <> cdName c
    <> " type=" <> renderPgType (cdType c)
    <> (if cdNullable c then " null" else " notnull")

renderPgType :: PgType -> Text
renderPgType = \case
  PgBigInt -> "bigint"
  PgInteger -> "integer"
  PgSmallInt -> "smallint"
  PgText -> "text"
  PgBytea -> "bytea"
  PgJsonb -> "jsonb"
  PgBoolean -> "boolean"
  PgNumeric -> "numeric"
  PgTimestamp -> "timestamp"
  PgTimestampTz -> "timestamptz"
  PgTextArray -> "text[]"

renderMode :: TableMode -> Text
renderMode = \case
  TableLogged -> "logged"
  TableUnlogged -> "unlogged"

renderPairs :: [(Text, Text)] -> Text
renderPairs = T.intercalate ";" . map (\(k, v) -> k <> "=" <> v)

renderParentRef :: ParentRef -> Text
renderParentRef pr =
  prColumn pr <> "->" <> prParentTable pr <> "." <> prParentColumn pr
