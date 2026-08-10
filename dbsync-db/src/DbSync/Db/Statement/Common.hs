{-# LANGUAGE OverloadedStrings #-}

-- | Shared 'Hasql' helpers used by every @DbSync.Db.Statement.*@
-- module. Each helper is parameterised by a 'TableDef' so the table
-- name lives in one place — the schema module — and never has to be
-- hand-typed in a statement.
module DbSync.Db.Statement.Common
  ( -- * ID allocation
    nextIdStmt

    -- * SQL builders
  , insertRowSql
  , insertReturningIdSql
  , upsertRowSql
  , insertIgnoreRowSql
  , insertableColumns
  , rebuildTableScript

    -- * Lookups
  , LookupColumn (..)
  , queryIdByColumnStmt
  , countRowsStmt

    -- * Reusable codecs
  , int8RowDecoder
  , word64Param

    -- * Array parameter helpers
  , arrayParam
  , nullArrayParam
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import Data.List (lookup)
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (idDecoder)
import DbSync.Db.Schema.Types (ColumnDef (..), PgType (..), TableDef (..))
import DbSync.Db.Sql (quoteIdent)

-- ---------------------------------------------------------------------------
-- * ID allocation
-- ---------------------------------------------------------------------------

nextIdStmt :: TableDef -> (Int64 -> a) -> Stmt.Statement () a
nextIdStmt td ctor =
  Stmt.preparable
    ("SELECT nextval('" <> tdName td <> "_id_seq')")
    E.noParams
    (D.singleRow (idDecoder ctor))

-- ---------------------------------------------------------------------------
-- * SQL builders
-- ---------------------------------------------------------------------------

-- | The columns an @INSERT@ should mention: 'tdColumns' minus the
-- IDENTITY columns (PG fills from the sequence) and generated columns
-- (PG computes them).
insertableColumns :: TableDef -> [ColumnDef]
insertableColumns td =
  [ c | c <- tdColumns td
      , cdName c `notElem` generated
      , cdName c `notElem` identCols
  ]
  where
    generated = map fst (tdGeneratedColumns td)
    identCols = tdIdentityColumns td

-- | @INSERT INTO <table> (col1, …) VALUES ($1, …)@. The caller's
-- encoder must emit values in the same order as 'insertableColumns'.
--
-- 'PgJsonb' columns get a @::jsonb@ cast because PG refuses implicit
-- @text -> jsonb@ coercion on parameterised INSERTs.
insertRowSql :: TableDef -> Text
insertRowSql td =
  T.concat
    [ "INSERT INTO ", tdName td
    , " (", T.intercalate ", " cols, ")"
    , " VALUES (", T.intercalate ", " placeholders, ")"
    ]
  where
    columns      = insertableColumns td
    cols         = map cdName columns
    placeholders =
      [ "$" <> T.pack (show n) <> castFor c
      | (n, c) <- zip [1 :: Int ..] columns
      ]
    castFor c = case cdType c of
      PgJsonb -> "::jsonb"
      _       -> ""

-- | @INSERT INTO <table> (…) VALUES (…) RETURNING id@, with @id@
-- omitted from both the column list and the placeholders. Used when
-- PG allocates the id from the backing sequence.
insertReturningIdSql :: TableDef -> Text
insertReturningIdSql td =
  T.concat
    [ "INSERT INTO ", tdName td
    , " (", T.intercalate ", " (map cdName cols), ")"
    , " VALUES (", T.intercalate ", " placeholders, ")"
    , " RETURNING id"
    ]
  where
    cols = filter ((/= "id") . cdName) (insertableColumns td)
    placeholders =
      [ "$" <> T.pack (show n) <> castFor c
      | (n, c) <- zip [1 :: Int ..] cols
      ]
    castFor c = case cdType c of
      PgJsonb -> "::jsonb"
      _       -> ""

-- | 'insertRowSql' plus @ON CONFLICT … DO UPDATE@ on the table's
-- declared unique constraint, refreshing every other insertable
-- column (except @id@) from @EXCLUDED@. Needs the matching unique
-- index, so Follow-phase only.
upsertRowSql :: TableDef -> Text
upsertRowSql td =
  T.concat
    [ insertRowSql td
    , " ON CONFLICT (", T.intercalate ", " conflictCols, ")"
    , " DO UPDATE SET "
    , T.intercalate ", " [ c <> " = EXCLUDED." <> c | c <- updateCols ]
    ]
  where
    conflictCols = soleUniqueConstraint td
    updateCols =
      [ cdName c
      | c <- insertableColumns td
      , cdName c `notElem` ("id" : conflictCols)
      ]

-- | 'insertRowSql' plus @ON CONFLICT … DO NOTHING@ on the table's
-- declared unique constraint, for rows a rollback replay re-emits
-- byte-identical. Follow-phase only.
insertIgnoreRowSql :: TableDef -> Text
insertIgnoreRowSql td =
  T.concat
    [ insertRowSql td
    , " ON CONFLICT (", T.intercalate ", " (soleUniqueConstraint td), ") DO NOTHING"
    ]

-- | The conflict target: exactly one 'tdUniqueConstraints' entry.
-- Panics otherwise — a builder bug, not a runtime condition.
soleUniqueConstraint :: TableDef -> [Text]
soleUniqueConstraint td = case tdUniqueConstraints td of
  [cols] -> toList cols
  other  -> panic $
    "soleUniqueConstraint: " <> tdName td <> " declares "
      <> show (length other) <> " unique constraints, expected exactly 1"

-- | Replace a table with a fresh heap: @CREATE UNLOGGED TABLE … AS
-- SELECT@ + @DROP@ + @RENAME@, then the constraint DDL that CTAS does
-- not carry over.
--
-- The @id@ @DEFAULT@ is deliberately not re-attached: the flip
-- ('DbSync.Db.Schema.Init.perTableSchemaForFollowTipSql') wires it for
-- counter-managed tables, so a rebuild has to run before the flip.
rebuildTableScript
  :: TableDef
  -> [(Text, Text)]
  -- ^ Column name → its @SELECT@ expression. Unlisted columns pass
  -- through from @src@.
  -> Text
  -- ^ Everything after @FROM@. Must expose the original table as @src@.
  -> Text
rebuildTableScript td overrides fromSql
  | not (null (tdGeneratedColumns td)) =
      panic $
        "rebuildTableScript: " <> tbl <> " declares generated columns, which"
          <> " CTAS materialises as plain ones with no ALTER to re-attach them"
  | otherwise = T.unlines $
      [ "CREATE UNLOGGED TABLE " <> quoteIdent newName <> " AS"
      , "SELECT " <> T.intercalate ", " selExprs
      , "  FROM " <> fromSql <> ";"
      , "DROP TABLE " <> quoteIdent tbl <> ";"
      , "ALTER TABLE " <> quoteIdent newName <> " RENAME TO " <> quoteIdent tbl <> ";"
      ]
        <> notNullDdl <> defaultDdl <> checkDdl <> identityDdl
  where
    tbl     = tdName td
    newName = tbl <> "_new"

    -- Column order follows 'tdColumns', keeping the rebuilt table
    -- column-identical to the declared schema.
    selExprs =
      [ maybe ("src." <> quoted) (\e -> e <> " AS " <> quoted) (lookup name overrides)
      | c <- tdColumns td
      , let name = cdName c
      , let quoted = quoteIdent name
      ]

    alter stmts = "ALTER TABLE " <> quoteIdent tbl <> " " <> stmts <> ";"

    notNullCols = [cdName c | c <- tdColumns td, not (cdNullable c)]
    notNullDdl
      | null notNullCols = []
      | otherwise =
          [ alter $ T.intercalate ", "
              ["ALTER COLUMN " <> quoteIdent c <> " SET NOT NULL" | c <- notNullCols]
          ]
    defaultDdl =
      [ alter ("ALTER COLUMN " <> quoteIdent c <> " SET DEFAULT " <> expr)
      | (c, expr) <- tdColumnDefaults td
      ]
    checkDdl =
      [ alter ("ADD CHECK (" <> expr <> ")") | expr <- tdChecks td ]
    identityDdl =
      [ alter ("ALTER COLUMN " <> quoteIdent c <> " ADD GENERATED BY DEFAULT AS IDENTITY")
      | c <- tdIdentityColumns td
      ]

-- ---------------------------------------------------------------------------
-- * Lookups
-- ---------------------------------------------------------------------------

-- | The closed set of column naming conventions 'queryIdByColumnStmt'
-- accepts.
data LookupColumn
  = ByHash      -- ^ @hash@ (block, tx, slot_leader).
  | ByHashRaw   -- ^ @hash_raw@ (pool_hash, stake_address).
  deriving stock (Eq, Show)

lookupColumnName :: LookupColumn -> Text
lookupColumnName = \case
  ByHash    -> "hash"
  ByHashRaw -> "hash_raw"

queryIdByColumnStmt
  :: TableDef
  -> LookupColumn
  -> (Int64 -> a)
  -> Stmt.Statement ByteString (Maybe a)
queryIdByColumnStmt td col ctor =
  Stmt.preparable
    ("SELECT id FROM " <> tdName td <> " WHERE " <> lookupColumnName col <> " = $1")
    (E.param (E.nonNullable E.bytea))
    (D.rowMaybe (idDecoder ctor))

countRowsStmt :: TableDef -> Stmt.Statement () Int64
countRowsStmt td =
  Stmt.preparable
    ("SELECT COUNT(*) FROM " <> tdName td)
    E.noParams
    int8RowDecoder

-- ---------------------------------------------------------------------------
-- * Reusable codecs
-- ---------------------------------------------------------------------------

-- | Single-column 'Int64' row decoder. Shared by @COUNT(*)@,
-- @MAX(id)@, and similar aggregate-shape statements.
int8RowDecoder :: D.Result Int64
int8RowDecoder = D.singleRow (D.column (D.nonNullable D.int8))

-- | 'Word64' through a PG @int8@ column. Cardano slot numbers and
-- similar are widened to 'Int64' at the boundary; this codifies that
-- decision in one place.
word64Param :: E.Params Word64
word64Param = fromIntegral >$< E.param (E.nonNullable E.int8)

-- ---------------------------------------------------------------------------
-- * Array parameter helpers
-- ---------------------------------------------------------------------------

arrayParam :: E.Value a -> E.Params [a]
arrayParam v = E.param (E.nonNullable (E.foldableArray (E.nonNullable v)))

nullArrayParam :: E.Value a -> E.Params [Maybe a]
nullArrayParam v = E.param (E.nonNullable (E.foldableArray (E.nullable v)))
