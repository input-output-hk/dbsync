{-# LANGUAGE OverloadedStrings #-}

-- | @FOREIGN KEY@ DDL for the ownership edges declared in
-- 'tdParentRefs'. Nothing else is constrained — see 'ParentRef'.
module DbSync.Db.Statement.Constraints
  ( ConstraintStatement (..)
  , parentRefConstraints
  , parentRefConstraintName
  ) where

import Cardano.Prelude

import qualified Data.Text as T

import DbSync.Db.Schema.Types (ParentRef (..), TableDef (..))
import DbSync.Db.Sql (quoteIdent)

-- | One edge, split so the cheap half and the scanning half can be
-- scheduled differently.
data ConstraintStatement = ConstraintStatement
  { csName        :: !Text
  , csTable       :: !Text
    -- ^ Referencing table.
  , csAddSql      :: !Text
    -- ^ @ADD CONSTRAINT … NOT VALID@ — catalog only.
  , csValidateSql :: !Text
    -- ^ @VALIDATE CONSTRAINT@ — scans the table.
  }
  deriving stock (Eq, Show)

-- | PostgreSQL's own default name shape, so a constraint created here
-- and one added by a hand-written migration collide rather than double up.
parentRefConstraintName :: TableDef -> ParentRef -> Text
parentRefConstraintName td pr =
  tdName td <> "_" <> prColumn pr <> "_fkey"

-- | Edges with both ends present. A disabled extractor's tables were
-- never created, so an edge into a missing parent is skipped.
parentRefConstraints :: [TableDef] -> [ConstraintStatement]
parentRefConstraints tables =
  [ ConstraintStatement
      { csName        = name
      , csTable       = tdName td
      , csAddSql      = addSql td pr name
      , csValidateSql = validateSql (tdName td) name
      }
  | td <- tables
  , pr <- tdParentRefs td
  , prParentTable pr `elem` present
  , let name = parentRefConstraintName td pr
  ]
  where
    present = map tdName tables

addSql :: TableDef -> ParentRef -> Text -> Text
addSql td pr name = T.concat
  [ "ALTER TABLE ", quoteIdent (tdName td)
  , " ADD CONSTRAINT ", quoteIdent name
  , " FOREIGN KEY (", quoteIdent (prColumn pr), ")"
  , " REFERENCES ", quoteIdent (prParentTable pr)
  , " (", quoteIdent (prParentColumn pr), ") NOT VALID;"
  ]

validateSql :: Text -> Text -> Text
validateSql tableName name = T.concat
  [ "ALTER TABLE ", quoteIdent tableName
  , " VALIDATE CONSTRAINT ", quoteIdent name, ";"
  ]
