module Main (main) where

import Cardano.Prelude
import DbSync.Compare.Check (CheckItem (..), runChecks)
import DbSync.Compare.Connect (DbRole (..), mkSettings, withDb)
import DbSync.Compare.Introspect (computeBlockCeiling, computeCeiling, gatherFacts)
import DbSync.Compare.Options (Config (..), parseConfig)
import DbSync.Compare.RowCounts (rowCountsChecks)
import DbSync.Compare.SchemaCoverage
  ( renderSchemaCoverage
  , runSchemaCoverage
  , schemaCoverageComparable
  , schemaCoverageHasRegressions
  )
import DbSync.Compare.SpotCheck (SpotCheckConfig (..), spotCheckChecks)

main :: IO ()
main = do
  cfg <- parseConfig
  let settingsFor name = mkSettings name (cfgHost cfg) (cfgPort cfg) (cfgUser cfg) (cfgPassword cfg)
      openDb name role = withDb (settingsFor name) role name (cfgVerbose cfg) (cfgStatementTimeout cfg)
  openDb (cfgOldDb cfg) OldDb $ \oldConn ->
    openDb (cfgNewDb cfg) NewDb $ \newConn -> do
      oldFacts <- gatherFacts oldConn
      newFacts <- gatherFacts newConn
      let epochCeiling = computeCeiling (cfgMaxEpoch cfg) oldFacts newFacts
          blockCeiling = computeBlockCeiling (cfgMaxBlock cfg) oldFacts newFacts

      coverage <- runSchemaCoverage oldConn newConn oldFacts newFacts epochCeiling
      renderSchemaCoverage coverage

      rowItems <-
        if cfgRowCounts cfg
          then do
            checks <- rowCountsChecks (schemaCoverageComparable coverage) blockCeiling
            pure (Section "row counts" : map Item checks)
          else pure []
      spotItems <-
        if cfgSpotCheck cfg
          then
            spotCheckChecks
              SpotCheckConfig {scSeed = cfgSeed cfg, scSamples = cfgSamples cfg, scEraFilter = cfgEras cfg}
              epochCeiling
          else pure []

      checksBad <- runChecks oldConn newConn (rowItems <> spotItems)
      if schemaCoverageHasRegressions coverage || checksBad
        then exitFailure
        else exitSuccess
