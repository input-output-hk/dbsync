-- | Tests for CLI argument parsing.
module DbSync.App.CliSpec
  ( spec
  ) where

import Cardano.Prelude

import DbSync.App.Cli (CliArgs (..), cliArgsParser)
import Options.Applicative
  ( ParserResult (..)
  , defaultPrefs
  , execParserPure
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "DbSync.App.Cli" $ do
  describe "cliArgsParser" $ do
    it "parses all five required arguments" $ do
      let result = parseArgs
            [ "--config", "/path/to/config.json"
            , "--pg-config", "/path/to/pg-config.json"
            , "--node-config", "/path/to/node-config.json"
            , "--socket-path", "/path/to/node.socket"
            , "--ledger-state-dir", "/data/dbsync"
            ]
      result `shouldBe` Right CliArgs
        { caConfig            = "/path/to/config.json"
        , caPgConfig          = "/path/to/pg-config.json"
        , caNodeConfig        = "/path/to/node-config.json"
        , caSocketPath        = "/path/to/node.socket"
        , caLedgerStateDir    = "/data/dbsync"
        , caResyncFromGenesis = False
        , caRollbackToSlot    = Nothing
        }

    it "accepts arguments in any order" $ do
      let result = parseArgs
            [ "--ledger-state-dir", "/tmp/state"
            , "--node-config", "node-config.json"
            , "--socket-path", "/run/node.socket"
            , "--pg-config", "pg-config.json"
            , "--config", "config.json"
            ]
      result `shouldBe` Right CliArgs
        { caConfig            = "config.json"
        , caPgConfig          = "pg-config.json"
        , caNodeConfig        = "node-config.json"
        , caSocketPath        = "/run/node.socket"
        , caLedgerStateDir    = "/tmp/state"
        , caResyncFromGenesis = False
        , caRollbackToSlot    = Nothing
        }

    it "defaults --resync-from-genesis to False when omitted" $
      fmap caResyncFromGenesis (parseArgs requiredArgs) `shouldBe` Right False

    it "sets --resync-from-genesis to True when supplied" $
      fmap caResyncFromGenesis (parseArgs (requiredArgs <> ["--resync-from-genesis"]))
        `shouldBe` Right True

    it "defaults --rollback-to-slot to Nothing when omitted" $
      fmap caRollbackToSlot (parseArgs requiredArgs) `shouldBe` Right Nothing

    it "parses --rollback-to-slot SLOTNO into a Just" $
      fmap caRollbackToSlot (parseArgs (requiredArgs <> ["--rollback-to-slot", "12345"]))
        `shouldBe` Right (Just 12345)

    it "fails when --config is missing" $
      parseArgs (without "--config") `shouldSatisfy` isLeft

    it "fails when --pg-config is missing" $
      parseArgs (without "--pg-config") `shouldSatisfy` isLeft

    it "fails when --node-config is missing" $
      parseArgs (without "--node-config") `shouldSatisfy` isLeft

    it "fails when --socket-path is missing" $
      parseArgs (without "--socket-path") `shouldSatisfy` isLeft

    it "fails when --ledger-state-dir is missing" $
      parseArgs (without "--ledger-state-dir") `shouldSatisfy` isLeft

    it "fails with no arguments" $
      parseArgs [] `shouldSatisfy` isLeft

-- * Helpers

requiredArgs :: [Text]
requiredArgs =
  [ "--config", "config.json"
  , "--pg-config", "pg-config.json"
  , "--node-config", "node-config.json"
  , "--socket-path", "node.socket"
  , "--ledger-state-dir", "/data"
  ]

-- | 'requiredArgs' with one flag/value pair removed.
without :: Text -> [Text]
without flag = go requiredArgs
  where
    go (f : v : rest)
      | f == flag = rest
      | otherwise = f : v : go rest
    go xs = xs

-- | Parse a list of arguments using the CLI parser, returning Left on failure.
parseArgs :: [Text] -> Either Text CliArgs
parseArgs args =
  case execParserPure defaultPrefs cliArgsParser (map toS args) of
    Success a           -> Right a
    Failure _           -> Left "parse failure"
    CompletionInvoked _ -> Left "completion"
