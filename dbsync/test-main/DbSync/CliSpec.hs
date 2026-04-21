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
            [ "--node-config", "/path/to/db-sync-config.json"
            , "--socket-path", "/path/to/node.socket"
            , "--state-dir", "/data/dbsync"
            , "--profile", "/path/to/profile.json"
            ]
      result `shouldBe` Right CliArgs
        { caNodeConfig = "/path/to/db-sync-config.json"
        , caSocketPath = "/path/to/node.socket"
        , caStateDir   = "/data/dbsync"
        , caProfile    = "/path/to/profile.json"
        }

    it "accepts arguments in any order" $ do
      let result = parseArgs
            [ "--profile", "profile.json"
            , "--state-dir", "/tmp/state"
            , "--socket-path", "/run/node.socket"
            , "--node-config", "db-sync-config.json"
            ]
      result `shouldBe` Right CliArgs
        { caNodeConfig = "db-sync-config.json"
        , caSocketPath = "/run/node.socket"
        , caStateDir   = "/tmp/state"
        , caProfile    = "profile.json"
        }

    it "fails when --node-config is missing" $ do
      let result = parseArgs
            [ "--socket-path", "/path/to/node.socket"
            , "--state-dir", "/data"
            , "--profile", "profile.json"
            ]
      result `shouldSatisfy` isLeft

    it "fails when --socket-path is missing" $ do
      let result = parseArgs
            [ "--node-config", "db-sync-config.json"
            , "--state-dir", "/data"
            , "--profile", "profile.json"
            ]
      result `shouldSatisfy` isLeft

    it "fails when --state-dir is missing" $ do
      let result = parseArgs
            [ "--node-config", "db-sync-config.json"
            , "--socket-path", "/path/to/node.socket"
            , "--profile", "profile.json"
            ]
      result `shouldSatisfy` isLeft

    it "fails when --profile is missing" $ do
      let result = parseArgs
            [ "--node-config", "db-sync-config.json"
            , "--socket-path", "/path/to/node.socket"
            , "--state-dir", "/data"
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
