{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for the boot-time utxo-config gate: the
-- @utxo_consumed_by_tx_id@ and @utxo_strategy@ recorded on
-- @dbsync_sync_state@ must match the current config.
--
-- Requires a running PostgreSQL instance and a @dbsync_test@ database
-- the current user can create tables in.
module DbSync.App.UtxoConfigGateSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T
import Test.Hspec (Spec, afterAll_, beforeAll_, before_, describe, it, shouldBe, shouldThrow)

import DbSync.App.Boot (BootError (..), Configured (..), Stored (..), renderBootError)
import DbSync.App.Config.Types
  ( UtxoOption (..)
  , UtxoStrategy (..)
  , defaultUtxoOption
  )
import DbSync.App.Run (runUtxoConfigGate)
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

spec :: Spec
spec = describe "runUtxoConfigGate" $ do
  describe "gate" $
    beforeAll_ (dropSchema [] testConnStr >> initSchema [] testConnStr) $
    afterAll_  (dropSchema [] testConnStr) $
    before_    resetSyncStateTable $ do

      it "passes when the stored settings match the config" $
        withGate $ \gate conn -> do
          runAppM conn (seedSyncState 1 testFp False [] 42 "magic-42" True "archive")
          gate defaultUtxoOption { uoConsumedByTxId = True }

      it "aborts when consumed_by_tx_id differs" $
        withGate $ \gate conn -> do
          runAppM conn (seedSyncState 1 testFp False [] 42 "magic-42" True "archive")
          gate defaultUtxoOption { uoConsumedByTxId = False }
            `shouldThrow` (== ExitFailure 1)

      it "aborts when the strategy differs" $
        withGate $ \gate conn -> do
          runAppM conn (seedSyncState 1 testFp False [] 42 "magic-42" True "prune")
          gate defaultUtxoOption { uoStrategy = StrategyArchive }
            `shouldThrow` (== ExitFailure 1)

      it "passes quietly while the sync-state row is missing" $
        -- An absent row is 'decideBoot's case ('BootSyncStateMissing'),
        -- not this gate's.
        withGate $ \gate _conn ->
          gate defaultUtxoOption

  describe "renderBootError" $ do
    -- Lock stored/configured to their lines, so a swapped construction
    -- site cannot render a misleading message.
    it "puts the stored consumed_by_tx_id on the sync-state line" $ do
      let msg = renderBootError $
            BootConsumedByTxIdMismatch (Stored True) (Configured False)
      lineWith "dbsync_sync_state" msg `shouldBe` Just "True"
      lineWith "current config" msg `shouldBe` Just "False"

    it "puts the stored strategy on the sync-state line" $ do
      let msg = renderBootError $
            BootUtxoStrategyMismatch (Stored "prune") (Configured "archive")
      lineWith "dbsync_sync_state" msg `shouldBe` Just "\"prune\""
      lineWith "current config" msg `shouldBe` Just "\"archive\""

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

withGate :: ((UtxoOption -> IO ()) -> ControlConnection -> IO a) -> IO a
withGate k = do
  tracer <- quietTracer
  bracket (openControlConnection testHasqlSettings) closeControlConnection $ \conn ->
    k (runUtxoConfigGate tracer conn) conn

-- | The value after @=@ on the first line containing the needle.
lineWith :: Text -> Text -> Maybe Text
lineWith needle msg = do
  line <- find (needle `T.isInfixOf`) (T.lines msg)
  case T.splitOn "=" line of
    [_, v] -> Just (T.strip v)
    _      -> Nothing

testFp :: Fingerprint
testFp = Fingerprint "test-fp"

resetSyncStateTable :: IO ()
resetSyncStateTable =
  execTestDb $ "TRUNCATE TABLE " <> tdName syncStateTableDef <> ";"
