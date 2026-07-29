-- | Every example config shipped in @config-examples/@ must parse
-- and validate. Catches drift between the examples and the config
-- schema (e.g. a renamed option or a shorthand the parser stops
-- accepting).
module DbSync.App.Config.ExamplesSpec
  ( spec
  ) where

import Cardano.Prelude

import System.Directory (listDirectory)

import DbSync.App.Config.Database (parseDatabaseConfig)
import DbSync.App.Config.Types (parseConfig)
import DbSync.App.Config.Validation (validateConfig)
import Test.Hspec (Spec, describe, it, runIO, shouldSatisfy)

examplesDir :: FilePath
examplesDir = "../config-examples"

spec :: Spec
spec = describe "config-examples/" $ do
  names <- runIO $ sort . filter (".json" `isSuffixOf`) <$> listDirectory examplesDir

  -- Guards against the directory moving and the loop below silently
  -- testing nothing.
  it "ships at least one example" $
    names `shouldSatisfy` (not . null)

  for_ names $ \name ->
    if "pg-config" `isPrefixOf` name
      then it (name <> " parses as a pg-config") $ do
        result <- parseDatabaseConfig (examplesDir <> "/" <> name)
        case result of
          Left err -> panic $ "parse failed: " <> show err
          Right _  -> pure ()
      else it (name <> " parses and validates") $ do
        result <- parseConfig (examplesDir <> "/" <> name)
        case result of
          Left err -> panic $ "parse failed: " <> show err
          Right cfg -> case validateConfig cfg of
            Left errs -> panic $ "validation failed: " <> show errs
            Right _   -> pure ()
