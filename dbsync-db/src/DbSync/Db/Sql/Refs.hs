{-# LANGUAGE OverloadedStrings #-}

-- | Type-safe construction of SQL identifier references. Column
-- references go through 'TableColumn' (from
-- 'DbSync.Db.Schema.Types'), which pairs a column name with its
-- owning 'TableDef'; mismatched references are a type error.
module DbSync.Db.Sql.Refs
  ( table
  , col
  , qcol
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.Types (TableColumn (..), TableDef (..))
import DbSync.Db.Sql (quoteIdent)

table :: TableDef -> Text
table = quoteIdent . tdName

col :: TableColumn -> Text
col = quoteIdent . tcName

-- | Qualified, quoted column reference: @alias."col"@. The alias is
-- a SQL alias (subquery name, CTE name, or table name).
qcol :: Text -> TableColumn -> Text
qcol alias c = alias <> "." <> col c
