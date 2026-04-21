module Main
  ( main
  ) where

import Cardano.Prelude

import qualified DbSync.AppSpec as AppSpec
import qualified DbSync.CliSpec as CliSpec
import qualified DbSync.Config.GenesisSpec as ConfigGenesisSpec
import qualified DbSync.Config.NodeSpec as ConfigNodeSpec
import qualified DbSync.Config.TypesSpec as ConfigTypesSpec
import qualified DbSync.Config.ValidationSpec as ConfigValidationSpec
import qualified DbSync.Extractor.CoreSpec as ExtractorCoreSpec
import qualified DbSync.Schema.CoreSpec as SchemaCoreSpec
import qualified DbSync.Schema.GenerateSpec as SchemaGenerateSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  AppSpec.spec
  CliSpec.spec
  ConfigTypesSpec.spec
  ConfigValidationSpec.spec
  ConfigNodeSpec.spec
  ConfigGenesisSpec.spec
  SchemaCoreSpec.spec
  SchemaGenerateSpec.spec
  ExtractorCoreSpec.spec
