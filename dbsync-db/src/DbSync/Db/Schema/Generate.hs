{-# LANGUAGE OverloadedStrings #-}

-- | DDL generation.
--
-- Generates @CREATE TABLE@ statements from 'TableDef' definitions.
-- During 'IngestChainHistory', tables are created as @UNLOGGED@ with no
-- indexes or constraints; during 'PreparingForVolatileTail' they are
-- converted to @LOGGED@ and indexes are added.
module DbSync.Db.Schema.Generate
  ( generateCreateTable
  ) where

import Cardano.Prelude

import Data.List (lookup)
import qualified Data.Text as T

import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )
import DbSync.Db.Sql (quoteIdent)

-- | Generate a @CREATE TABLE@ DDL statement from a 'TableDef'.
--
-- For an extractor data table (UNLOGGED, no constraints) the output
-- is simple:
--
-- @
-- CREATE UNLOGGED TABLE "block" (
--   "id" BIGINT NOT NULL,
--   "hash" BYTEA NOT NULL,
--   "epoch_no" BIGINT
-- );
-- @
--
-- For a metadata table with a primary key, column defaults, and
-- table-level checks (e.g. 'dbsync_sync_state'), the generator emits
-- each in a stable order:
--
-- @
-- CREATE TABLE "dbsync_sync_state" (
--   "id" SMALLINT NOT NULL DEFAULT 1,
--   "block_id_counter" BIGINT NOT NULL DEFAULT 1,
--   …
--   PRIMARY KEY ("id"),
--   CHECK ("id" = 1)
-- );
-- @
--
-- Indexes and foreign keys are never emitted here — they are added
-- during 'PreparingForVolatileTail'.
generateCreateTable :: TableDef -> Text
generateCreateTable td =
  T.unlines $
    [ createLine ]
    ++ bodyLines
    ++ [ ");" ]
  where
    createLine :: Text
    createLine =
      let modeStr = case tdMode td of
            TableUnlogged -> "CREATE UNLOGGED TABLE"
            TableLogged   -> "CREATE TABLE"
      in modeStr <> " " <> quoteIdent (tdName td) <> " ("

    -- Each "body" line represents one comma-separated item of the
    -- CREATE TABLE. We concatenate column lines, a primary-key line
    -- (if any), and zero or more check lines, then append commas to
    -- all but the last.
    bodyLines :: [Text]
    bodyLines =
      let columnLines = map formatColumn (tdColumns td)
          pkLine = case tdPrimaryKey td of
            Nothing   -> []
            Just cols -> [ "PRIMARY KEY (" <> T.intercalate ", " (map quoteIdent cols) <> ")" ]
          checkLines = map (\expr -> "CHECK (" <> expr <> ")") (tdChecks td)
          allItems = columnLines ++ pkLine ++ checkLines
          total = length allItems
      in zipWith (\idx item -> "  " <> item <> commaFor total idx) [1..] allItems

    commaFor :: Int -> Int -> Text
    commaFor total idx
      | idx < total = ","
      | otherwise   = ""

    -- Generated columns are emitted as
    -- @"name" <type> GENERATED ALWAYS AS (expr) STORED@ with no
    -- NOT NULL or DEFAULT (the generation expression takes the place
    -- of any default; PostgreSQL infers nullability from the
    -- expression).
    formatColumn :: ColumnDef -> Text
    formatColumn col =
      let typeSql = pgTypeToSql (cdType col)
          quoted  = quoteIdent (cdName col)
      in case lookup (cdName col) (tdGeneratedColumns td) of
           Just expr ->
             quoted <> " " <> typeSql <> " GENERATED ALWAYS AS (" <> expr <> ") STORED"
           Nothing ->
             let nullability = if cdNullable col then "" else " NOT NULL"
                 defaultClause = case lookup (cdName col) (tdColumnDefaults td) of
                   Nothing   -> ""
                   Just expr -> " DEFAULT " <> expr
             in quoted <> " " <> typeSql <> nullability <> defaultClause

-- | Convert a 'PgType' to its SQL string representation.
pgTypeToSql :: PgType -> Text
pgTypeToSql = \case
  PgBigInt      -> "BIGINT"
  PgInteger     -> "INTEGER"
  PgSmallInt    -> "SMALLINT"
  PgText        -> "TEXT"
  PgBytea       -> "BYTEA"
  PgJsonb       -> "JSONB"
  PgBoolean     -> "BOOLEAN"
  PgNumeric     -> "NUMERIC"
  PgTimestamp   -> "TIMESTAMP WITHOUT TIME ZONE"
  PgTimestampTz -> "TIMESTAMP WITH TIME ZONE"
  PgEnum name   -> name
