{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pure tests for 'DbSync.Error.Render'.
module DbSync.Error.RenderSpec (spec) where

import Cardano.Prelude

import Control.Concurrent.Async (AsyncCancelled (..))
import qualified Control.Exception as Exception
import Control.Tracer (Tracer (..))
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as T

import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

import DbSync.Error (AppError (..), BlockAnnotation (..))
import DbSync.Error.Render (logThreadExit, renderAppError, renderCrash)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..), SrcInfo (..))

srcInfo :: SrcInfo
srcInfo = SrcInfo
  { siFunction = "useConn"
  , siModule   = "DbSync.Db.Run"
  , siFile     = "dbsync/src/DbSync/Db/Run.hs"
  , siLine     = 30
  }

dbErr :: AppError
dbErr = AppDatabaseError srcInfo "advanceSyncState: connection reset"

spec :: Spec
spec = describe "DbSync.Error.Render" $ do
  describe "renderAppError" $ do
    it "renders kind, location, and message on one line" $
      renderAppError dbErr `shouldBe`
        "database error at dbsync/src/DbSync/Db/Run.hs:30 (useConn): \
        \advanceSyncState: connection reset"

    it "labels each AppError kind" $
      [ T.takeWhile (/= ' ') (renderAppError (ctor srcInfo "m"))
      | ctor <-
          [ AppDatabaseError, AppSyncStateError, AppLedgerError
          , AppBlockError, AppSchemaError, AppNetworkError, AppInternalError
          ]
      ] `shouldBe`
        ["database", "sync-state", "ledger", "block", "schema", "network", "internal"]

  describe "renderCrash" $ do
    it "summarises a thrown AppError" $ do
      e <- captureException (Exception.throwIO dbErr)
      renderCrash e `shouldSatisfy`
        T.isInfixOf "database error at dbsync/src/DbSync/Db/Run.hs:30 (useConn)"

    it "includes block context from an annotateIO scope" $ do
      let hash = BS.pack [0xab, 0xcd, 0xef, 0x01, 0x23, 0x45, 0x67, 0x89, 0xff]
          ann  = BlockAnnotation 42 7 hash
      e <- captureException (Exception.annotateIO ann (Exception.throwIO dbErr))
      renderCrash e `shouldSatisfy`
        T.isInfixOf "while processing block 7 (slot 42, hash abcdef0123456789"

    it "renders a WhileHandling cause under the new error" $ do
      let orig = Exception.toException (AppLedgerError srcInfo "ledger boom")
      e <- captureException
             (Exception.annotateIO (Exception.WhileHandling orig)
               (Exception.throwIO dbErr))
      let out = renderCrash e
      out `shouldSatisfy` T.isInfixOf "database error"
      out `shouldSatisfy` T.isInfixOf "caused while handling: ledger error"

  describe "logThreadExit" $ do
    it "logs AsyncCancelled at Info as an orderly stop" $ do
      (tracer, readLogs) <- capturingTracer
      logThreadExit "W" (Exception.toException AsyncCancelled) tracer
      logs <- readLogs
      map (\m -> (lmSeverity m, lmComponent m, lmMessage m)) logs `shouldBe`
        [(Info, "W", "stopped (cancelled during shutdown)")]

    it "logs any other exception at Error with a crashed prefix" $ do
      (tracer, readLogs) <- capturingTracer
      logThreadExit "W" (Exception.toException dbErr) tracer
      logs <- readLogs
      case logs of
        [m] -> do
          lmSeverity m `shouldBe` Error
          lmMessage m `shouldSatisfy` T.isPrefixOf "crashed: database error"
        _ -> expectationFailure "expected exactly one log line"

-- | Run @act@, expecting it to throw, and return the caught exception.
captureException :: IO a -> IO Exception.SomeException
captureException act = do
  r <- Exception.try act
  case r of
    Left (e :: Exception.SomeException) -> pure e
    Right _ -> panic "captureException: action did not throw"

capturingTracer :: IO (AppTracer, IO [LogMsg])
capturingTracer = do
  ref <- newIORef ([] :: [LogMsg])
  let tracer  = Tracer (\msg -> modifyIORef' ref (msg :))
      readAll = reverse <$> readIORef ref
  pure (tracer, readAll)
