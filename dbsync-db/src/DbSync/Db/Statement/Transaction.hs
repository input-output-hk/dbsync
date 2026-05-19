{-# LANGUAGE OverloadedStrings #-}

-- | Transaction-control SQL.
--
-- Provides the three control commands — @BEGIN@, @COMMIT@,
-- @ROLLBACK@ — used by every PG transaction in the codebase.
-- Each is exposed twice: as 'Text' for hasql\'s 'Sess.script' /
-- 'Stmt.unpreparable' (the control-plane path), and as 'ByteString'
-- for libpq\'s 'PQ.exec' (the loader-stream path). The two forms are
-- the same bytes in different wrappers; both call sites share one
-- source of truth.
module DbSync.Db.Statement.Transaction
  ( -- * Text form (hasql)
    beginSql
  , commitSql
  , rollbackSql

    -- * ByteString form (libpq)
  , beginSqlBs
  , commitSqlBs
  , rollbackSqlBs
  ) where

import Cardano.Prelude

import qualified Data.Text.Encoding as TE

-- | @BEGIN@ — start a transaction.
beginSql :: Text
beginSql = "BEGIN"

-- | @COMMIT@ — commit the current transaction.
commitSql :: Text
commitSql = "COMMIT"

-- | @ROLLBACK@ — abort the current transaction.
rollbackSql :: Text
rollbackSql = "ROLLBACK"

-- | 'beginSql' as 'ByteString' for libpq.
beginSqlBs :: ByteString
beginSqlBs = TE.encodeUtf8 beginSql

-- | 'commitSql' as 'ByteString' for libpq.
commitSqlBs :: ByteString
commitSqlBs = TE.encodeUtf8 commitSql

-- | 'rollbackSql' as 'ByteString' for libpq.
rollbackSqlBs :: ByteString
rollbackSqlBs = TE.encodeUtf8 rollbackSql
