{-# LANGUAGE OverloadedStrings #-}

-- | Small SQL-text helpers shared between the schema generators,
-- the @psql@ driver, and the runtime hasql 'Statement' bindings.
module DbSync.Db.Sql
  ( quoteIdent
  , quoteLiteral
  ) where

import Cardano.Prelude

import qualified Data.Text as T

-- | Wrap a SQL identifier (table or column name) in double quotes.
quoteIdent :: Text -> Text
quoteIdent name = "\"" <> name <> "\""

-- | Wrap a SQL string literal in single quotes, doubling any
-- internal single quotes per the SQL standard.
quoteLiteral :: Text -> Text
quoteLiteral val = "'" <> T.replace "'" "''" val <> "'"
