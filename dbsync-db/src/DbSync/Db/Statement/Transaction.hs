{-# LANGUAGE OverloadedStrings #-}

-- | @BEGIN@ / @COMMIT@ / @ROLLBACK@ control commands. Each is
-- exposed twice: as 'Text' for hasql\'s 'Sess.script' /
-- 'Stmt.unpreparable', and as 'ByteString' for libpq\'s 'PQ.exec'
-- (loader-stream path). One source of truth for both call sites.
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

beginSql :: Text
beginSql = "BEGIN"

commitSql :: Text
commitSql = "COMMIT"

rollbackSql :: Text
rollbackSql = "ROLLBACK"

beginSqlBs :: ByteString
beginSqlBs = TE.encodeUtf8 beginSql

commitSqlBs :: ByteString
commitSqlBs = TE.encodeUtf8 commitSql

rollbackSqlBs :: ByteString
rollbackSqlBs = TE.encodeUtf8 rollbackSql
