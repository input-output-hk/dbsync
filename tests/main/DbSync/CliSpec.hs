-- | Tests for CLI argument parsing.
module DbSync.CliSpec
  ( spec
  ) where

import Cardano.Prelude

import DbSync.Cli (CliArgs (..), cliArgsParser)
import Options.Applicative
  ( ParserResult (..)
  , defaultPrefs
  , execParserPure
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "DbSync.Cli" $ do
  describe "cliArgsParser" $ do
    it "parses all four required arguments" $ do
      let result = parseArgs
            [ "--db-sync-config", "/path/to/db-sync-config.json"
            , "--socket-path", "/path/to/node.socket"
            , "--ledger-state-dir", "/data/dbsync"
            , "--profile", "/path/to/dbsync-profile.json"
            ]
      result `shouldBe` Right CliArgs
        { caDbSyncConfig      = "/path/to/db-sync-config.json"
        , caSocketPath        = "/path/to/node.socket"
        , caLedgerStateDir    = "/data/dbsync"
        , caProfile           = "/path/to/dbsync-profile.json"
        , caResyncFromGenesis = False
        }

    it "accepts arguments in any order" $ do
      let result = parseArgs
            [ "--profile", "dbsync-profile.json"
            , "--ledger-state-dir", "/tmp/state"
            , "--socket-path", "/run/node.socket"
            , "--db-sync-config", "db-sync-config.json"
            ]
      result `shouldBe` Right CliArgs
        { caDbSyncConfig      = "db-sync-config.json"
        , caSocketPath        = "/run/node.socket"
        , caLedgerStateDir    = "/tmp/state"
        , caProfile           = "dbsync-profile.json"
        , caResyncFromGenesis = False
        }

    it "defaults --resync-from-genesis to False when omitted" $ do
      let result = parseArgs
            [ "--db-sync-config",   "x"
            , "--socket-path",      "y"
            , "--ledger-state-dir", "z"
            , "--profile",          "w"
            ]
      fmap caResyncFromGenesis result `shouldBe` Right False

    it "sets --resync-from-genesis to True when supplied" $ do
      let result = parseArgs
            [ "--db-sync-config",   "x"
            , "--socket-path",      "y"
            , "--ledger-state-dir", "z"
            , "--profile",          "w"
            , "--resync-from-genesis"
            ]
      fmap caResyncFromGenesis result `shouldBe` Right True

    it "fails when --db-sync-config is missing" $ do
      let result = parseArgs
            [ "--socket-path", "/path/to/node.socket"
            , "--ledger-state-dir", "/data"
            , "--profile", "dbsync-profile.json"
            ]
      result `shouldSatisfy` isLeft

    it "fails when --socket-path is missing" $ do
      let result = parseArgs
            [ "--db-sync-config", "db-sync-config.json"
            , "--ledger-state-dir", "/data"
            , "--profile", "dbsync-profile.json"
            ]
      result `shouldSatisfy` isLeft

    it "fails when --ledger-state-dir is missing" $ do
      let result = parseArgs
            [ "--db-sync-config", "db-sync-config.json"
            , "--socket-path", "/path/to/node.socket"
            , "--profile", "dbsync-profile.json"
            ]
      result `shouldSatisfy` isLeft

    it "fails when --profile is missing" $ do
      let result = parseArgs
            [ "--db-sync-config", "db-sync-config.json"
            , "--socket-path", "/path/to/node.socket"
            , "--ledger-state-dir", "/data"
            ]
      result `shouldSatisfy` isLeft

    it "fails with no arguments" $ do
      let result = parseArgs []
      result `shouldSatisfy` isLeft

-- * Helpers

-- | Parse a list of arguments using the CLI parser, returning Left on failure.
parseArgs :: [Text] -> Either Text CliArgs
parseArgs args =
  case execParserPure defaultPrefs cliArgsParser (map toS args) of
    Success a           -> Right a
    Failure _           -> Left "parse failure"
    CompletionInvoked _ -> Left "completion"
