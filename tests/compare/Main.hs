module Main (main) where

import Cardano.Prelude
import DbSync.Compare.Connect (DbRole (..), mkSettings, withDb)
import DbSync.Compare.Introspect (computeCeiling, gatherFacts)
import DbSync.Compare.Options (Config (..), parseConfig)
import DbSync.Compare.RowCounts (renderRowCounts, rowCountsHaveMismatch, runRowCounts)
import DbSync.Compare.SchemaCoverage
  ( renderSchemaCoverage
  , runSchemaCoverage
  , schemaCoverageComparable
  , schemaCoverageHasRegressions
  )
import DbSync.Compare.SpotCheck
  ( SpotCheckConfig (..)
  , renderSpotCheck
  , runSpotCheck
  , spotCheckHasMismatch
  )

main :: IO ()
main = do
  cfg <- parseConfig
  let settingsFor name = mkSettings name (cfgHost cfg) (cfgPort cfg) (cfgUser cfg) (cfgPassword cfg)
  withDb (settingsFor (cfgOldDb cfg)) OldDb (cfgOldDb cfg) $ \oldConn ->
    withDb (settingsFor (cfgNewDb cfg)) NewDb (cfgNewDb cfg) $ \newConn -> do
      oldFacts <- gatherFacts oldConn
      newFacts <- gatherFacts newConn
      let epochCeiling = computeCeiling (cfgMaxEpoch cfg) oldFacts newFacts

      coverage <- runSchemaCoverage oldConn newConn oldFacts newFacts epochCeiling
      renderSchemaCoverage coverage

      rowCountsBad <-
        if cfgRowCounts cfg
          then do
            counts <- runRowCounts oldConn newConn (schemaCoverageComparable coverage) epochCeiling
            renderRowCounts counts
            pure (rowCountsHaveMismatch counts)
          else pure False

      spotBad <-
        if cfgSpotCheck cfg
          then do
            let spotCfg =
                  SpotCheckConfig
                    { scSeed = cfgSeed cfg
                    , scSamples = cfgSamples cfg
                    , scEraFilter = cfgEras cfg
                    }
            spot <- runSpotCheck oldConn newConn spotCfg epochCeiling
            renderSpotCheck spot
            pure (spotCheckHasMismatch spot)
          else pure False

      if schemaCoverageHasRegressions coverage || rowCountsBad || spotBad
        then exitFailure
        else exitSuccess
