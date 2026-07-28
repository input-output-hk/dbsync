-- | Tests for pg-config parsing and @password_file@ resolution.
module DbSync.App.Config.DatabaseSpec
  ( spec
  ) where

import Cardano.Prelude

import DbSync.App.Config.Database (DatabaseConfig (..), parseDatabaseConfig)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "DbSync.App.Config.Database" $ do
  describe "parseDatabaseConfig" $ do
    it "parses all fields and resolves a relative password_file" $ do
      -- fixtures/pg-config-full.json points at "pg-password.txt",
      -- resolved against the fixture's own directory; the trailing
      -- newline in the secret file is stripped.
      result <- parseDatabaseConfig "fixtures/pg-config-full.json"
      result `shouldBe` Right DatabaseConfig
        { dcHost     = "db.internal"
        , dcPort     = 6432
        , dcName     = "cexplorer"
        , dcUser     = "dbsync"
        , dcPassword = "sekrit"
        }

    it "defaults port, user, and password when omitted" $ do
      result <- parseDatabaseConfig "fixtures/pg-config-minimal.json"
      result `shouldBe` Right DatabaseConfig
        { dcHost     = "localhost"
        , dcPort     = 5432
        , dcName     = "cexplorer"
        , dcUser     = "postgres"
        , dcPassword = ""
        }

    it "fails when the password_file does not exist" $ do
      result <- parseDatabaseConfig "fixtures/pg-config-missing-file.json"
      result `shouldSatisfy` isLeft

    it "fails when required fields are missing" $ do
      result <- parseDatabaseConfig "fixtures/pg-config-no-host.json"
      result `shouldSatisfy` isLeft
