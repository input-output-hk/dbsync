{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for the boot-time network gate: the
-- @network_magic@ recorded on @dbsync_sync_state@ must match the
-- configured genesis before any other resume work happens.
--
-- Requires a running PostgreSQL instance and a @dbsync_test@ database
-- the current user can create tables in.
module DbSync.App.NetworkGateSpec (spec) where

import Cardano.Prelude

import Test.Hspec (Spec, afterAll_, beforeAll_, before_, describe, it, shouldThrow)

import Ouroboros.Network.Magic (NetworkMagic (..))

import DbSync.App.Run (runNetworkGate)
import DbSync.AppM (runAppM)
import DbSync.Db.Schema.Init (dropSchema, initSchema)
import DbSync.Db.Schema.SyncState (syncStateTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Schema.Version (Fingerprint (..))
import DbSync.SyncState.Row
  ( ControlConnection
  , closeControlConnection
  , openControlConnection
  , seedSyncState
  )
import DbSync.Test.AppHarness (quietTracer)
import DbSync.Test.Database (execTestDb, testConnStr, testHasqlSettings)
import DbSync.Trace.Types (AppTracer)

spec :: Spec
spec = describe "runNetworkGate" $
  beforeAll_ (dropSchema [] testConnStr >> initSchema [] testConnStr) $
  afterAll_  (dropSchema [] testConnStr) $
  before_    resetSyncStateTable $ do

    it "passes when the stored magic matches the configured genesis" $
      withGate $ \gate conn -> do
        runAppM conn (seedSyncState 1 testFp False [] 42 "magic-42")
        gate (NetworkMagic 42)

    it "aborts when the stored magic differs" $
      withGate $ \gate conn -> do
        runAppM conn (seedSyncState 1 testFp False [] 2 "preview")
        gate (NetworkMagic 764824073) `shouldThrow` (== ExitFailure 1)

    it "passes quietly while the sync-state row is missing" $
      -- An absent row is 'decideBoot's case ('BootSyncStateMissing'),
      -- not the network gate's.
      withGate $ \gate _conn ->
        gate (NetworkMagic 42)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Hand the test a ready-to-call gate plus the control connection
-- used to stage the sync-state row.
withGate :: ((NetworkMagic -> IO ()) -> ControlConnection -> IO a) -> IO a
withGate k = do
  tracer <- quietTracer
  bracket (openControlConnection testHasqlSettings) closeControlConnection $ \conn ->
    k (gateWith tracer conn) conn
  where
    gateWith :: AppTracer -> ControlConnection -> NetworkMagic -> IO ()
    gateWith tracer conn = runNetworkGate tracer conn

testFp :: Fingerprint
testFp = Fingerprint "test-fp"

resetSyncStateTable :: IO ()
resetSyncStateTable =
  execTestDb $ "TRUNCATE TABLE " <> tdName syncStateTableDef <> ";"
