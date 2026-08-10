{-# LANGUAGE OverloadedStrings #-}

-- | Render a 'SchemaChange' as the DDL that applies it.
--
-- 'AddTable' and 'AddColumn' reuse the @CREATE TABLE@ formatters, so a
-- migrated column matches the created one. 'AmbiguousChange' renders as a
-- SQL comment, which surfaces it in a draft without being runnable.
module DbSync.Db.Schema.Migration.Render
  ( renderChange
  ) where

import Cardano.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import DbSync.Db.Schema.Generate (formatColumnDdl, generateCreateTable)
import DbSync.Db.Schema.Migration.Diff (SchemaChange (..))
import DbSync.Db.Schema.Types (ParentRef (..))
import DbSync.Db.Sql (quoteIdent)

renderChange :: SchemaChange -> Text
renderChange = \case
  AddTable td -> generateCreateTable td
  DropTable name ->
    "DROP TABLE " <> quoteIdent name <> " CASCADE;"
  AddColumn table col ->
    "ALTER TABLE " <> quoteIdent table <> " ADD COLUMN " <> formatColumnDdl col <> ";"
  DropColumn table col ->
    "ALTER TABLE " <> quoteIdent table <> " DROP COLUMN " <> quoteIdent col <> ";"
  AddCheck table expr ->
    "ALTER TABLE " <> quoteIdent table <> " ADD CHECK (" <> expr <> ");"
  AddUniqueConstraint table cols ->
    "ALTER TABLE " <> quoteIdent table <> " ADD UNIQUE ("
      <> T.intercalate ", " (map quoteIdent (NE.toList cols)) <> ");"
  AddForeignKey table pr ->
    "ALTER TABLE " <> quoteIdent table <> " ADD FOREIGN KEY ("
      <> quoteIdent (prColumn pr) <> ") REFERENCES "
      <> quoteIdent (prParentTable pr) <> " ("
      <> quoteIdent (prParentColumn pr) <> ");"
  AmbiguousChange msg ->
    "-- TODO (manual): " <> msg
