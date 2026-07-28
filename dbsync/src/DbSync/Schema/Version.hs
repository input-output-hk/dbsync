-- | Schema identity of this binary: the global schema version and the
-- fingerprint that ties each released version to an exact schema shape.
-- Drift between code and a released version is caught in CI (pin test)
-- and at boot (stored fingerprint comparison) instead of surfacing as a
-- broken query mid-sync.
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
  , ForeignKey (..)
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
  [ (1, Fingerprint "12b699697da12cca1f120f45a0201d7c18739eeba6bf3b248c5766e7914dca09")
  ]

-- ---------------------------------------------------------------------------
-- * Fingerprint
-- ---------------------------------------------------------------------------

-- | Hex-encoded Blake2b-256 of the canonical schema rendering.
newtype Fingerprint = Fingerprint {unFingerprint :: Text}
  deriving stock (Eq, Show)

-- | Hash the declared schema: every table (sorted by name, so input order
-- is irrelevant) plus any raw DDL that lives outside 'TableDef' (for
-- example the epoch view definitions). The rendering spells out every
-- 'TableDef' field explicitly so renaming a Haskell field cannot silently
-- change the hash.
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
           , "foreign_keys=" <> T.intercalate ";" (map renderForeignKey (tdForeignKeys td))
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

renderForeignKey :: ForeignKey -> Text
renderForeignKey fk =
  fkColumn fk <> "->" <> fkParentTable fk <> "." <> fkParentColumn fk
