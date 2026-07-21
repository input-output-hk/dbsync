-- | Every preset profile shipped in @profiles/@ must parse and
-- validate. Catches drift between the presets and the config schema
-- (e.g. a renamed option or a shorthand the parser stops accepting).
module DbSync.App.Config.ProfilesSpec
  ( spec
  ) where

import Cardano.Prelude

import System.Directory (listDirectory)

import DbSync.App.Config.Types (parseConfig)
import DbSync.App.Config.Validation (validateConfig)
import Test.Hspec (Spec, describe, it, runIO, shouldSatisfy)

profilesDir :: FilePath
profilesDir = "../profiles"

spec :: Spec
spec = describe "profiles/" $ do
  names <- runIO $ sort . filter (".json" `isSuffixOf`) <$> listDirectory profilesDir

  -- Guards against the directory moving and the loop below silently
  -- testing nothing.
  it "ships at least one preset" $
    names `shouldSatisfy` (not . null)

  for_ names $ \name ->
    it (name <> " parses and validates") $ do
      result <- parseConfig (profilesDir <> "/" <> name)
      case result of
        Left err -> panic $ "parse failed: " <> show err
        Right cfg -> case validateConfig cfg of
          Left errs -> panic $ "validation failed: " <> show errs
          Right _   -> pure ()
