{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : DbSync.Writer.Copy
Description : Function-record interface for writing PostgreSQL COPY rows.

Defines the 'CopyWriter' function record used during 'IngestChainHistory'
to stream rows into PostgreSQL via the COPY protocol, and 'CopyConnections',
an opaque wrapper for per-table database connections.
-}
module DbSync.Db.Writer.Copy
  ( -- * Types
    CopyWriter (..)
  , CopyConnections   -- opaque
  , unCopyConnections
  ) where

import Cardano.Prelude

import Data.Map.Strict (Map)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Function record for writing rows via the PostgreSQL COPY protocol.
--
-- Each field is an action on the underlying COPY stream. Constructed
-- by the connection layer; consumed by projection extraction code.
data CopyWriter = CopyWriter
  { cwWriteRow :: !(Text -> ByteString -> IO ())
      -- ^ Write a single COPY row to the named table
  , cwCommit   :: !(IO ())
      -- ^ Commit the current COPY batch across all tables
  , cwReopen   :: !(IO ())
      -- ^ Reopen COPY streams after a commit
  , cwClose    :: !(IO ())
      -- ^ Close all COPY streams and release resources
  }

-- | Opaque wrapper for per-table database connections used by the
-- COPY writer. Keyed by table name.
newtype CopyConnections = CopyConnections
  { unCopyConnections :: Map Text ConnectionHandle }

-- | Placeholder for a database connection handle.
-- Will be replaced with the real type from @postgresql-libpq@ or @hasql@.
type ConnectionHandle = ()
