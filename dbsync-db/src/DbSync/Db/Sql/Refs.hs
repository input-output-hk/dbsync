{-# LANGUAGE OverloadedStrings #-}

-- | Type-safe construction of SQL identifier references against a
-- 'TableDef'. Hand-built SQL that references columns by raw string
-- breaks silently when a column is renamed; routing every reference
-- through 'col' / 'qcol' surfaces the rename at module-load time
-- via 'panic'.
--
-- The module is deliberately minimal: a SQL builder for the few
-- post-load UPDATEs that need it, not a query DSL. If a callsite
-- needs more than table/column references and aliasing, write the
-- SQL fragment by hand around these helpers.
module DbSync.Db.Sql.Refs
  ( table
  , col
  , qcol
  , columnRef
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.Types (ColumnDef (..), TableDef (..))
import DbSync.Db.Sql (quoteIdent)

-- | Quoted table name. Trusts 'tdName' to be a valid SQL identifier.
table :: TableDef -> Text
table = quoteIdent . tdName

-- | Quoted column name, validated against the table's declared
-- columns. Panics at evaluation time if the column is not declared,
-- catching renames before any SQL hits the wire.
col :: TableDef -> Text -> Text
col td c = quoteIdent (columnRef td c)

-- | Qualified, quoted column reference: @alias."col"@. The alias is
-- a SQL alias (subquery name, CTE name, or table name); the column
-- is validated against the supplied 'TableDef'.
qcol :: Text -> TableDef -> Text -> Text
qcol alias td c = alias <> "." <> col td c

-- | Look up a column by name on a 'TableDef'. Returns the name if
-- the column is declared; panics at evaluation time if not. Lets
-- hand-rolled SQL fragments refer to columns through the schema so
-- a rename surfaces as a load-time error rather than a silent
-- runtime query failure.
columnRef :: TableDef -> Text -> Text
columnRef td name
  | name `elem` map cdName (tdColumns td) = name
  | otherwise = panic $
      "DbSync.Db.Sql.Refs.columnRef: column "
        <> name <> " not declared on table " <> tdName td
