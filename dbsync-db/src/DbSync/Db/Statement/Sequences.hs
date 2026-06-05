{-# LANGUAGE OverloadedStrings #-}

-- | DDL builders for the sequence-reset pass.
--
-- 'FollowingChainTip' allocates ids via the @id@ column's backing
-- sequence — either an explicit @\<table\>_id_seq@ attached via
-- @ALTER … SET DEFAULT nextval@, or the implicit sequence
-- PostgreSQL creates for an @IDENTITY@ column. Before Follow takes
-- over, each sequence is advanced to @MAX(id) + 1@ so the next
-- allocation does not collide with rows already loaded by Ingest.
--
-- The output is a per-table SQL string of the form
--
-- @
-- SELECT setval(pg_get_serial_sequence('<table>', 'id'),
--               COALESCE((SELECT MAX(id) FROM <table>), 0) + 1,
--               false);
-- @
--
-- The third argument to @setval@ is @is_called@: passing @false@
-- means the next @nextval@ returns exactly the supplied value, so
-- the @+ 1@ is correct for both empty and non-empty tables.
-- @pg_get_serial_sequence@ resolves the sequence name regardless of
-- whether it was created explicitly or by @IDENTITY@.
module DbSync.Db.Statement.Sequences
  ( resetSequenceSql
  , resetSequenceStmt
  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Sql (quoteIdent, quoteLiteral)

-- | Produce the @SELECT setval(...)@ statement for the given
-- table's @id@ sequence. The table must own a sequence on its @id@
-- column, attached either explicitly during the schema-mode flip
-- or implicitly by an @IDENTITY@ declaration.
resetSequenceSql :: Text -> Text
resetSequenceSql tableName =
  T.unwords
    [ "SELECT setval("
    , "pg_get_serial_sequence(" <> quoteLiteral tableName <> ", 'id'),"
    , "COALESCE((SELECT MAX(id) FROM " <> quoteIdent tableName <> "), 0) + 1,"
    , "false)"
    ]

-- | The 'resetSequenceSql' command wrapped as an unprepareable
-- 'Stmt.Statement' that drains the single 'Int64' @setval@ returns.
--
-- Unprepareable because the SQL embeds the table name as a literal
-- rather than a parameter — each table is its own prepared
-- statement under hasql\'s caching, which is fine for the one-shot
-- per-table @setval@ but means there's no benefit to preparing.
resetSequenceStmt :: Text -> Stmt.Statement () Int64
resetSequenceStmt tableName =
  Stmt.unpreparable
    (resetSequenceSql tableName)
    E.noParams
    (D.singleRow (D.column (D.nonNullable D.int8)))
