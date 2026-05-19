{-# LANGUAGE OverloadedStrings #-}

-- | SQL builders for the loader-stream COPY path.
--
-- @IngestChainHistory@ streams rows to PostgreSQL via @COPY FROM
-- STDIN@. The driver in @dbsync@ owns the libpq connection and the
-- per-table queue plumbing; this module owns the SQL it executes.
--
-- The @COPY@ command identifies the target table and lists the
-- columns the loader stream encodes for. Generated columns are
-- excluded so PostgreSQL computes them on insert.
module DbSync.Db.Statement.Loader
  ( -- * COPY FROM STDIN builders
    copyFromStdinSql
  , copyableColumnList

    -- * Re-exports
  , ColumnDef (..)
  , TableDef (..)
  ) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE

import DbSync.Db.Schema.Types (ColumnDef (..), TableDef (..))

-- | Build @COPY "table" (col1, col2, …) FROM STDIN@ for the given
-- table and its pre-built column list.
--
-- The column list is supplied as a 'ByteString' rather than rebuilt
-- here because the loader stream caches it once at connection-open
-- time and reuses it for every batch.
copyFromStdinSql
  :: Text         -- ^ Target table name (will be double-quoted).
  -> ByteString   -- ^ Pre-built comma-separated column list.
  -> ByteString
copyFromStdinSql tableName colList =
  "COPY \"" <> TE.encodeUtf8 tableName <> "\" (" <> colList <> ") FROM STDIN"

-- | Comma-separated, double-quoted column list ready to drop into
-- the @COPY@ command — e.g. @"id", "hash", "epoch_no"@.
--
-- Excludes any columns listed in 'tdGeneratedColumns' so PostgreSQL
-- evaluates their @GENERATED ALWAYS AS@ expressions on insert
-- instead of expecting the loader to supply them.
copyableColumnList :: TableDef -> ByteString
copyableColumnList td =
  BS.intercalate ", " $
    map (TE.encodeUtf8 . quote . cdName) ingestable
  where
    generated  = map fst (tdGeneratedColumns td)
    ingestable = filter (\c -> cdName c `notElem` generated) (tdColumns td)
    quote name = "\"" <> name <> "\""
