module DbSync.Compare.SchemaCoverage
  ( Verdict (..)
  , TableResult (..)
  , SchemaCoverageResult (..)
  , runSchemaCoverage
  , renderSchemaCoverage
  , schemaCoverageHasRegressions
  , schemaCoverageComparable
  ) where

import Cardano.Prelude
import Data.List (lookup)
import DbSync.Compare.Connect (DbConn, roleLabel)
import DbSync.Compare.Introspect (DbFacts (..), tableNonEmpty)
import DbSync.Compare.Report
import DbSync.Compare.Schema

-- ---------------------------------------------------------------------------
-- * Results
-- ---------------------------------------------------------------------------

data Verdict
  = Comparable (Maybe Text) -- ^ Populated on both sides; optional note (e.g. address mapping).
  | Skip Text -- ^ Not compared, with reason.
  deriving stock (Eq, Show)

data TableResult = TableResult
  { trTable :: !Text
  , trExtractor :: !Text
  , trVerdict :: !Verdict
  }
  deriving stock (Eq, Show)

data SchemaCoverageResult = SchemaCoverageResult
  { covOldFacts :: !DbFacts
  , covNewFacts :: !DbFacts
  , covCeiling :: !Int64
  , covTables :: ![TableResult]
  , covUnclassifiedOld :: ![Text] -- ^ Old tables with no new counterpart and no known reason.
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Classification
-- ---------------------------------------------------------------------------

runSchemaCoverage :: DbConn -> DbConn -> DbFacts -> DbFacts -> Int64 -> IO SchemaCoverageResult
runSchemaCoverage oldConn newConn oldFacts newFacts epochCeiling = do
  tables <- for newTableNames $ \table ->
    TableResult table (extractorOf table) <$> classify table
  pure
    SchemaCoverageResult
      { covOldFacts = oldFacts
      , covNewFacts = newFacts
      , covCeiling = epochCeiling
      , covTables = tables
      , covUnclassifiedOld = unclassifiedOld
      }
  where
    extractorOf table = fromMaybe "-" (lookup table tableToExtractor)

    classify table = case staticClassify table of
      StaticSkip reason -> pure (Skip reason)
      StaticCandidate note -> classifyCandidate table (renamedOldName table) note

    classifyCandidate table oldName note
      | table `notElem` dfPresentTables newFacts = pure (Skip "absent in new schema")
      | oldName `notElem` dfPresentTables oldFacts = pure (Skip ("absent in old schema (" <> oldName <> ")"))
      | otherwise = do
          neOld <- tableNonEmpty oldConn oldName
          neNew <- tableNonEmpty newConn table
          pure (populated note neOld neNew)

    populated note neOld neNew = case (neOld, neNew) of
      (False, False) -> Skip "empty on both sides"
      (True, False) -> Skip "empty in new, populated in old"
      (False, True) -> Skip "empty in old, populated in new"
      (True, True) -> Comparable note

    accountedOld = map renamedOldName newTableNames <> map fst oldOnlyTables
    unclassifiedOld = filter (`notElem` accountedOld) (dfPresentTables oldFacts)

-- A regression is data the new database has lost relative to the old one, or
-- an old table the new schema neither maps nor knowingly drops.
schemaCoverageHasRegressions :: SchemaCoverageResult -> Bool
schemaCoverageHasRegressions result =
  not (null (covUnclassifiedOld result)) || any (regressed . trVerdict) (covTables result)
  where
    regressed (Skip "absent in new schema") = True
    regressed (Skip "empty in new, populated in old") = True
    regressed _ = False

schemaCoverageComparable :: SchemaCoverageResult -> [Text]
schemaCoverageComparable result =
  [trTable r | r <- covTables result, isComparable (trVerdict r)]
  where
    isComparable (Comparable _) = True
    isComparable _ = False

-- ---------------------------------------------------------------------------
-- * Rendering
-- ---------------------------------------------------------------------------

renderSchemaCoverage :: SchemaCoverageResult -> IO ()
renderSchemaCoverage result = do
  header "dbsync-compare"
  putLine (factsLine (covOldFacts result))
  putLine (factsLine (covNewFacts result))
  putLine ("ceiling : epoch <= " <> show (covCeiling result))

  header "Schema coverage"
  for_ (covTables result) renderRow

  putLine ""
  putLine
    ( green ("comparable: " <> show (length comparable))
        <> "   "
        <> yellow ("skipped: " <> show (length skipped))
    )

  unless (null (covUnclassifiedOld result)) $ do
    putLine ""
    putLine (red "old tables not classified (possible schema drift):")
    for_ (covUnclassifiedOld result) (\t -> putLine (red ("  " <> t)))
  where
    comparable = [t | TableResult t _ (Comparable _) <- covTables result]
    skipped = [t | TableResult t _ (Skip _) <- covTables result]

factsLine :: DbFacts -> Text
factsLine f =
  padRight 4 (roleLabel (dfRole f))
    <> padRight 16 (dfDbName f)
    <> "pg " <> dfServerVersion f
    <> ", " <> dfDbSize f
    <> ", max epoch " <> maybe "n/a" show (dfMaxEpoch f)

renderRow :: TableResult -> IO ()
renderRow (TableResult table ext verdict) =
  putLine (renderMark verdict <> "  " <> padRight 26 table <> padRight 22 ext <> detail)
  where
    detail = case verdict of
      Comparable Nothing -> dim "comparable"
      Comparable (Just note) -> dim ("comparable - " <> note)
      Skip reason -> dim reason

renderMark :: Verdict -> Text
renderMark = \case
  Comparable _ -> green (padRight 5 "OK")
  Skip _ -> yellow (padRight 5 "SKIP")
