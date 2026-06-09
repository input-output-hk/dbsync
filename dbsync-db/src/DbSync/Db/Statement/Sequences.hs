{-# LANGUAGE OverloadedStrings #-}

-- | DDL builders for the sequence-reset pass.
--
-- Before 'FollowingChainTip' takes over from Ingest, each table's
-- @id@ sequence is advanced to @MAX(id) + 1@ so the next Follow
-- allocation does not collide with Ingest-loaded rows. The output is
--
-- > SELECT setval(pg_get_serial_sequence('<table>', 'id'),
-- >               COALESCE((SELECT MAX(id) FROM <table>), 0) + 1,
-- >               false);
--
-- @false@ for @is_called@ means the next @nextval@ returns exactly
-- the supplied value, so @+ 1@ is correct for both empty and
-- non-empty tables.
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

resetSequenceSql :: Text -> Text
resetSequenceSql tableName =
  T.unwords
    [ "SELECT setval("
    , "pg_get_serial_sequence(" <> quoteLiteral tableName <> ", 'id'),"
    , "COALESCE((SELECT MAX(id) FROM " <> quoteIdent tableName <> "), 0) + 1,"
    , "false)"
    ]

-- | Unprepareable because the SQL embeds the table name as a literal
-- rather than a parameter; each table would be its own prepared
-- statement, with no caching benefit for a one-shot per-table call.
resetSequenceStmt :: Text -> Stmt.Statement () Int64
resetSequenceStmt tableName =
  Stmt.unpreparable
    (resetSequenceSql tableName)
    E.noParams
    (D.singleRow (D.column (D.nonNullable D.int8)))
