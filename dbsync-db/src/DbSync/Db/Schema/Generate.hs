{-# LANGUAGE OverloadedStrings #-}

-- | DDL generation.
--
-- Generates @CREATE TABLE@ statements from 'TableDef' definitions.
-- During 'IngestChainHistory', tables are created as @UNLOGGED@ with no
-- indexes or constraints; during 'PreparingForChainTip' they are
-- converted to @LOGGED@ and indexes are added.
module DbSync.Db.Schema.Generate
  ( generateCreateTable
  ) where

import Cardano.Prelude

import qualified Data.Text as T

import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )

-- | Generate a @CREATE TABLE@ DDL statement from a 'TableDef'.
--
-- Produces SQL like:
--
-- @
-- CREATE UNLOGGED TABLE "block" (
--   "id" BIGINT NOT NULL,
--   "hash" BYTEA NOT NULL,
--   "epoch_no" BIGINT
-- );
-- @
--
-- No indexes, constraints, or foreign keys are included — those are
-- added during 'PreparingForChainTip'.
generateCreateTable :: TableDef -> Text
generateCreateTable td =
  T.unlines $
    [ createLine
    ] ++ columnLines ++ [ ");" ]
  where
    createLine :: Text
    createLine =
      let modeStr = case tdMode td of
            TableUnlogged -> "CREATE UNLOGGED TABLE"
            TableLogged   -> "CREATE TABLE"
      in modeStr <> " " <> quote (tdName td) <> " ("

    columnLines :: [Text]
    columnLines =
      let cols = tdColumns td
          formatted = zipWith (formatColumn (length cols)) [1..] cols
      in formatted

    formatColumn :: Int -> Int -> ColumnDef -> Text
    formatColumn total idx col =
      let comma = if idx < total then "," else ""
          nullability = if cdNullable col then "" else " NOT NULL"
      in "  " <> quote (cdName col) <> " " <> pgTypeToSql (cdType col) <> nullability <> comma

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
  PgGenerated e -> "GENERATED ALWAYS AS (" <> e <> ") STORED"

-- | Quote a SQL identifier with double quotes.
quote :: Text -> Text
quote name = "\"" <> name <> "\""
