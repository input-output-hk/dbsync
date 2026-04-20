-- | COPY connection management.
--
-- Manages PostgreSQL connections in COPY mode, including opening and
-- closing COPY streams for each table during bulk ingest.
module DbSync.Db.Writer.Copy.Connection
  ( -- TODO: CopyConnection, openCopy, closeCopy
  ) where

import Cardano.Prelude
