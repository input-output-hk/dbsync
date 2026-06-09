{-# LANGUAGE OverloadedStrings #-}

-- | SQL builders for the @IngestChainHistory@ loader-stream
-- (@COPY FROM STDIN@) path. The driver in @dbsync@ owns the libpq
-- connection and per-table queue plumbing; this module owns the SQL
-- it executes.
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

-- | Build @COPY "table" (col1, col2, …) FROM STDIN@. The column list
-- is a 'ByteString' because the loader stream caches it once at
-- connection-open time and reuses it for every batch.
copyFromStdinSql
  :: Text         -- ^ Target table name (will be double-quoted).
  -> ByteString   -- ^ Pre-built comma-separated column list.
  -> ByteString
copyFromStdinSql tableName colList =
  "COPY \"" <> TE.encodeUtf8 tableName <> "\" (" <> colList <> ") FROM STDIN"

-- | Comma-separated, double-quoted column list. Excludes generated
-- columns (PG evaluates @GENERATED ALWAYS AS@ expressions on insert)
-- and IDENTITY columns (PG allocates from the sequence). The
-- per-table COPY row encoders must omit the corresponding fields.
copyableColumnList :: TableDef -> ByteString
copyableColumnList td =
  BS.intercalate ", " $
    map (TE.encodeUtf8 . quote . cdName) ingestable
  where
    generated    = map fst (tdGeneratedColumns td)
    identityCols = tdIdentityColumns td
    ingestable   =
      filter
        (\c -> cdName c `notElem` generated && cdName c `notElem` identityCols)
        (tdColumns td)
    quote name = "\"" <> name <> "\""
