{-# LANGUAGE OverloadedStrings #-}

-- | Mechanical schema diff.
--
-- Compares an @old@ and a @new@ list of 'TableDef' and produces the
-- 'SchemaChange's that turn one into the other. Changes the differ can
-- apply without guessing are concrete (@AddTable@, @AddColumn@, …);
-- anything that could destroy or misread data — a possible rename, a
-- type change, a NOT NULL column with no default — is surfaced as an
-- 'AmbiguousChange' for a human to resolve.
module DbSync.Db.Schema.Migration.Diff
  ( SchemaChange (..)
  , schemaDiff
  ) where

import Cardano.Prelude

import Data.List (lookup)
import qualified Data.Text as T

import DbSync.Db.Schema.Generate (pgTypeToSql)
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , ForeignKey
  , TableDef (..)
  )

-- ---------------------------------------------------------------------------
-- * Changes
-- ---------------------------------------------------------------------------

data SchemaChange
  = AddTable TableDef
  | DropTable Text
      -- ^ Table name.
  | AddColumn Text ColumnDef
      -- ^ Table name and the column to add.
  | DropColumn Text Text
      -- ^ Table name, column name.
  | AddCheck Text Text
      -- ^ Table name, @CHECK@ expression.
  | AddUniqueConstraint Text (NonEmpty Text)
      -- ^ Table name, column list.
  | AddForeignKey Text ForeignKey
      -- ^ Table name, outgoing FK.
  | AmbiguousChange Text
      -- ^ Description of a change the differ refuses to guess.
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Diff
-- ---------------------------------------------------------------------------

-- | Tables are matched by 'tdName', then columns within a matched table
-- by 'cdName'. Output order is deterministic: added then dropped tables
-- in input order, followed by per-matched-table column and constraint
-- changes.
schemaDiff :: [TableDef] -> [TableDef] -> [SchemaChange]
schemaDiff oldTables newTables =
  addedTables ++ droppedTables ++ matchedChanges
  where
    oldNames = map tdName oldTables
    newNames = map tdName newTables
    oldByName = [ (tdName t, t) | t <- oldTables ]

    addedTables   = [ AddTable t           | t <- newTables, tdName t `notElem` oldNames ]
    droppedTables = [ DropTable (tdName t)  | t <- oldTables, tdName t `notElem` newNames ]
    matchedChanges = concat
      [ matchedTableChanges o n
      | n <- newTables
      , Just o <- [lookup (tdName n) oldByName]
      ]

-- | Column and constraint changes between two tables of the same name.
matchedTableChanges :: TableDef -> TableDef -> [SchemaChange]
matchedTableChanges old new =
  columnChanges ++ checkChanges ++ uniqueChanges ++ fkChanges ++ generatedChanges
  where
    tn = tdName new
    oldCols = tdColumns old
    newCols = tdColumns new
    oldColNames = map cdName oldCols
    newColNames = map cdName newCols
    oldColsByName = [ (cdName c, c) | c <- oldCols ]

    newOnly = [ c | c <- newCols, cdName c `notElem` oldColNames ]
    oldOnly = [ c | c <- oldCols, cdName c `notElem` newColNames ]

    -- A table with both new-only and old-only columns is treated as a
    -- possible rename: surface one ambiguity rather than a destructive
    -- drop-and-add pair.
    columnChanges
      | not (null newOnly) && not (null oldOnly) =
          AmbiguousChange renameMsg : commonChanges
      | otherwise =
          map classifyAdded newOnly ++ map classifyDropped oldOnly ++ commonChanges

    renameMsg =
      tn <> ": columns dropped (" <> T.intercalate ", " (map cdName oldOnly)
         <> ") and added (" <> T.intercalate ", " (map cdName newOnly)
         <> ") — possible rename; resolve by hand"

    classifyAdded :: ColumnDef -> SchemaChange
    classifyAdded c
      | cdNullable c || cdName c `elem` defaultedColumns = AddColumn tn c
      | otherwise =
          AmbiguousChange
            (tn <> "." <> cdName c <> ": new NOT NULL column without default needs a backfill")

    classifyDropped :: ColumnDef -> SchemaChange
    classifyDropped c = DropColumn tn (cdName c)

    defaultedColumns = map fst (tdColumnDefaults new)

    -- Columns present in both tables: a type or nullability change is
    -- never applied automatically.
    commonChanges = concat
      [ commonColumnChange o n
      | n <- newCols
      , Just o <- [lookup (cdName n) oldColsByName]
      ]

    commonColumnChange :: ColumnDef -> ColumnDef -> [SchemaChange]
    commonColumnChange o n = typeChange ++ nullChange
      where
        typeChange
          | cdType o /= cdType n =
              [ AmbiguousChange
                  (tn <> "." <> cdName n <> ": type changed "
                     <> pgTypeToSql (cdType o) <> " -> " <> pgTypeToSql (cdType n)
                     <> " — needs an explicit USING cast") ]
          | otherwise = []
        nullChange
          | cdNullable o /= cdNullable n =
              [ AmbiguousChange
                  (tn <> "." <> cdName n
                     <> ": nullability changed; emit ALTER COLUMN SET/DROP NOT NULL by hand") ]
          | otherwise = []

    checkChanges =
      [ AddCheck tn expr | expr <- tdChecks new, expr `notElem` tdChecks old ]
    uniqueChanges =
      [ AddUniqueConstraint tn cols
      | cols <- tdUniqueConstraints new, cols `notElem` tdUniqueConstraints old ]
    fkChanges =
      [ AddForeignKey tn fk | fk <- tdForeignKeys new, fk `notElem` tdForeignKeys old ]
    generatedChanges =
      [ AmbiguousChange (tn <> "." <> col <> ": generated column added; review expression")
      | (col, _) <- tdGeneratedColumns new
      , col `notElem` oldGeneratedColumns ]
    oldGeneratedColumns = map fst (tdGeneratedColumns old)
