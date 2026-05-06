{-# LANGUAGE OverloadedStrings #-}

-- | Test-side helpers for hasql 'Statement' values.
--
-- Establishes the pattern used by every @DbSync.Db.Statement.<Name>Spec@
-- module: open a connection to the test DB, run a 'Stmt.Statement'
-- against it, throw 'IOError' on session failure (so 'shouldThrow' /
-- 'try' work without coupling the test to production-side
-- 'AppError' decoding).
--
-- == Why a separate test runner instead of reusing 'DbSync.Checkpoint.SyncState.runStmt'
--
-- The production runner ('runStmt' inside 'DbSync.Checkpoint.SyncState')
-- wraps every 'Hasql.Session.SessionError' into 'AppDatabaseError'.
-- Tests that want to assert on the underlying error shape (e.g.
-- "hasql reported a constraint violation", "the prepared statement
-- failed to encode") would have to peel the 'AppError' wrapper
-- back off — which couples the test to a layer of error
-- translation that isn't the thing under test. Keeping the test
-- runner thin avoids that coupling.
--
-- == Convention for @DbSync.Db.Statement.<Name>Spec@
--
-- > spec :: Spec
-- > spec = describe "DbSync.Db.Statement.<Name>" $
-- >   beforeAll_ (initSchema [] [] testConnStr) $
-- >   afterAll_  (dropSchema [] [] testConnStr) $
-- >   before_    (truncateAllTables ["<table>"]) $
-- >     describe "<statementName>" $
-- >       it "<expectation>" $
-- >         withTestConnection $ \conn -> do
-- >           runStatement conn <inputs> <statement>
-- >           result <- runStatement conn <inputs> <statement>
-- >           result \`shouldBe\` <expected>
--
-- Per-test 'before_' truncation keeps cases independent without
-- paying schema-recreate cost between every @it@. For Specs that
-- assert on side-effects across multiple statements within a single
-- @it@, prefer composing them via 'runSession' rather than chaining
-- 'runStatement' calls — that way the whole sequence runs in one
-- hasql 'Sess.Session' (single PG round-trip's worth of error
-- handling, easier to read).
module DbSync.Test.Hasql
  ( -- * Connection lifecycle
    withTestConnection

    -- * Running statements
  , runStatement
  , runSession
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import System.IO.Error (userError)

import DbSync.Test.Database (testHasqlSettings)

-- | Acquire a hasql connection to the test database, run the
-- action, release on exit. Throws 'IOError' (via 'userError') if
-- @Hasql.Connection.acquire@ fails.
--
-- Settings come from 'DbSync.Test.Database.testHasqlSettings'
-- (same libpq-default rules as every other DB-touching spec in
-- the suite).
withTestConnection :: (Conn.Connection -> IO a) -> IO a
withTestConnection = bracket acquire Conn.release
  where
    acquire = do
      result <- Conn.acquire testHasqlSettings
      case result of
        Left err ->
          throwIO . userError $
            "withTestConnection: failed to acquire test connection: " <> show err
        Right c -> pure c

-- | Run a single 'Stmt.Statement' against the supplied connection.
--
-- Throws 'IOError' on session failure so callers can use
-- 'shouldThrow' / 'try' without unwrapping a production-side
-- 'AppError'. Errors carry the underlying hasql 'SessionError' for
-- debugging.
runStatement
  :: Conn.Connection
  -> a
  -> Stmt.Statement a b
  -> IO b
runStatement conn params stmt =
  runSession conn (Sess.statement params stmt)

-- | Run an arbitrary hasql 'Sess.Session' against the supplied
-- connection. Useful for tests that compose several statements
-- (setup + assertion read) in a single PG transaction's worth of
-- error handling.
runSession
  :: Conn.Connection
  -> Sess.Session a
  -> IO a
runSession conn session = do
  result <- Conn.use conn session
  case result of
    Left err ->
      throwIO . userError $
        "runSession: " <> show err
    Right r -> pure r
