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
    it "parses all three required arguments" $ do
      let result = parseArgs
            [ "--node-config", "/path/to/node-config.json"
            , "--node-socket", "/path/to/node.socket"
            , "--config", "/path/to/db-sync.yaml"
            ]
      result `shouldBe` Right CliArgs
        { caNodeConfig = "/path/to/node-config.json"
        , caNodeSocket = "/path/to/node.socket"
        , caConfig     = "/path/to/db-sync.yaml"
        }

    it "accepts arguments in any order" $ do
      let result = parseArgs
            [ "--config", "db-sync.yaml"
            , "--node-socket", "/run/node.socket"
            , "--node-config", "mainnet-config.json"
            ]
      result `shouldBe` Right CliArgs
        { caNodeConfig = "mainnet-config.json"
        , caNodeSocket = "/run/node.socket"
        , caConfig     = "db-sync.yaml"
        }

    it "fails when --node-config is missing" $ do
      let result = parseArgs
            [ "--node-socket", "/path/to/node.socket"
            , "--config", "db-sync.yaml"
            ]
      result `shouldSatisfy` isLeft

    it "fails when --node-socket is missing" $ do
      let result = parseArgs
            [ "--node-config", "config.json"
            , "--config", "db-sync.yaml"
            ]
      result `shouldSatisfy` isLeft

    it "fails when --config is missing" $ do
      let result = parseArgs
            [ "--node-config", "config.json"
            , "--node-socket", "/path/to/node.socket"
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
    Success a    -> Right a
    Failure _    -> Left "parse failure"
    CompletionInvoked _ -> Left "completion"
