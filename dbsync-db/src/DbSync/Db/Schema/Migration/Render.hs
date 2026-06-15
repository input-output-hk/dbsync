{-# LANGUAGE OverloadedStrings #-}

-- | Render a 'SchemaChange' as the DDL that applies it.
--
-- 'AddTable' reuses 'generateCreateTable' and 'AddColumn' reuses the
-- same column formatter as @CREATE TABLE@, so a migration's column
-- matches the create path. 'AmbiguousChange' renders as a SQL comment
-- (@-- TODO (manual): …@) so a generated draft surfaces it without
-- being runnable as-is.
module DbSync.Db.Schema.Migration.Render
  ( renderChange
  ) where

import Cardano.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import DbSync.Db.Schema.Generate (formatColumnDdl, generateCreateTable)
import DbSync.Db.Schema.Migration.Diff (SchemaChange (..))
import DbSync.Db.Schema.Types (ForeignKey (..))
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
  AddForeignKey table fk ->
    "ALTER TABLE " <> quoteIdent table <> " ADD FOREIGN KEY ("
      <> quoteIdent (fkColumn fk) <> ") REFERENCES "
      <> quoteIdent (fkParentTable fk) <> " ("
      <> quoteIdent (fkParentColumn fk) <> ");"
  AmbiguousChange msg ->
    "-- TODO (manual): " <> msg
