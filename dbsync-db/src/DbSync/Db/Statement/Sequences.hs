{-# LANGUAGE OverloadedStrings #-}

-- | DDL builders for the sequence-reset pass.
--
-- @IngestChainHistory@ assigns IDs from in-process counters (see
-- 'DbSync.Id.Counter'), so the @\<table\>_id_seq@ sequence created
-- during the schema-mode flip is left at its default starting value.
-- 'FollowingChainTip' uses @nextval@ on those sequences directly,
-- so before handing over we set each sequence to
-- @MAX(id) + 1@ — the next ID the in-process counter would have
-- handed out — and let PG take over from there.
--
-- The output is a per-table SQL string of the form
--
-- @
-- SELECT setval('<table>_id_seq',
--               COALESCE((SELECT MAX(id) FROM <table>), 0) + 1,
--               false);
-- @
--
-- The third argument to @setval@ is @is_called@: passing @false@
-- means the next @nextval@ returns exactly the supplied value, so
-- the @+ 1@ is correct for both empty and non-empty tables.
module DbSync.Db.Statement.Sequences
  ( resetSequenceSql
  ) where

import Cardano.Prelude

import qualified Data.Text as T

import DbSync.Db.Sql (quoteIdent, quoteLiteral)

-- | Produce the @SELECT setval(...)@ statement for the given
-- table's @id@ sequence. The table is expected to have an @id@
-- column and an attached @\<table\>_id_seq@ sequence (created
-- during the schema-mode flip).
resetSequenceSql :: Text -> Text
resetSequenceSql tableName =
  T.unwords
    [ "SELECT setval("
    , quoteLiteral seqName <> ","
    , "COALESCE((SELECT MAX(id) FROM " <> quoteIdent tableName <> "), 0) + 1,"
    , "false)"
    ]
  where
    seqName = tableName <> "_id_seq"
