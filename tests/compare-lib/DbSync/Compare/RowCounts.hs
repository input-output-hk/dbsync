module DbSync.Compare.RowCounts
  ( RowCountRow (..)
  , RowCountsResult (..)
  , runRowCounts
  , renderRowCounts
  , rowCountsHaveMismatch
  ) where

import Cardano.Prelude
import DbSync.Compare.Connect (DbConn, queryMaybeInt, queryScalarInt)
import DbSync.Compare.FkGraph (Bound (..), boundFor)
import DbSync.Compare.Introspect (quoteIdent)
import DbSync.Compare.Report
import DbSync.Compare.Schema (renamedOldName)

-- ---------------------------------------------------------------------------
-- * Epoch-bounded row counts
-- ---------------------------------------------------------------------------

data RowCountRow = RowCountRow
  { rcTable :: !Text
  , rcOld :: !Int64
  , rcNew :: !Int64
  , rcBounded :: !Bool -- ^ False means counted unbounded; the delta is informational.
  , rcMatch :: !Bool
  }
  deriving stock (Eq, Show)

newtype RowCountsResult = RowCountsResult
  { rcRows :: [RowCountRow]
  }
  deriving stock (Eq, Show)

runRowCounts :: DbConn -> DbConn -> [Text] -> Int64 -> IO RowCountsResult
runRowCounts oldConn newConn tables epochCeiling = do
  oldAnchors <- computeAnchors oldConn epochCeiling
  newAnchors <- computeAnchors newConn epochCeiling
  rows <- for tables $ \table -> do
    let bound = boundFor table
    old <- queryScalarInt oldConn (countSql (renamedOldName table) bound oldAnchors epochCeiling)
    new <- queryScalarInt newConn (countSql table bound newAnchors epochCeiling)
    pure (RowCountRow table old new (isBounded bound) (old == new))
  pure (RowCountsResult rows)
  where
    isBounded Unbounded = False
    isBounded _ = True

rowCountsHaveMismatch :: RowCountsResult -> Bool
rowCountsHaveMismatch = any (\r -> rcBounded r && not (rcMatch r)) . rcRows

-- ---------------------------------------------------------------------------
-- * Anchor max ids
-- ---------------------------------------------------------------------------

data Anchors = Anchors
  { anBlock :: !Int64
  , anTx :: !Int64
  , anTxOut :: !Int64
  , anPoolUpdate :: !Int64
  , anGovAction :: !Int64
  }

computeAnchors :: DbConn -> Int64 -> IO Anchors
computeAnchors conn epochCeiling = do
  b <- maxId "block" ("epoch_no <= " <> show epochCeiling)
  t <- maxId "tx" ("block_id <= " <> show b)
  o <- maxId "tx_out" ("tx_id <= " <> show t)
  pu <- maxId "pool_update" ("registered_tx_id <= " <> show t)
  g <- maxId "gov_action_proposal" ("tx_id <= " <> show t)
  pure (Anchors b t o pu g)
  where
    maxId table cond =
      fromMaybe 0 <$> queryMaybeInt conn ("SELECT max(id) FROM " <> table <> " WHERE " <> cond)

anchorMax :: Anchors -> Text -> Int64
anchorMax anchors = \case
  "tx" -> anTx anchors
  "tx_out" -> anTxOut anchors
  "pool_update" -> anPoolUpdate anchors
  "gov_action_proposal" -> anGovAction anchors
  _ -> anBlock anchors

countSql :: Text -> Bound -> Anchors -> Int64 -> Text
countSql root bound anchors epochCeiling =
  "SELECT COUNT(*)::bigint FROM " <> quoteIdent root <> clause
  where
    clause = case bound of
      BoundEpoch -> " WHERE epoch_no <= " <> show epochCeiling
      BoundAnchor col parent -> " WHERE " <> quoteIdent col <> " <= " <> show (anchorMax anchors parent)
      Unbounded -> ""

-- ---------------------------------------------------------------------------
-- * Rendering
-- ---------------------------------------------------------------------------

renderRowCounts :: RowCountsResult -> IO ()
renderRowCounts result = do
  header "Row counts (epoch-bounded)"
  for_ (rcRows result) renderRow
  putLine ""
  putLine
    ( (if mismatches == 0 then green "all bounded counts match" else red (show mismatches <> " bounded mismatch(es)"))
        <> "   "
        <> dim (show unbounded <> " unbounded (info)")
    )
  where
    mismatches = length (filter (\r -> rcBounded r && not (rcMatch r)) (rcRows result))
    unbounded = length (filter (not . rcBounded) (rcRows result))

renderRow :: RowCountRow -> IO ()
renderRow r =
  putLine (mark <> "  " <> padRight 26 (rcTable r) <> counts)
  where
    mark
      | not (rcBounded r) = dim (padRight 5 "INFO")
      | rcMatch r = green (padRight 5 "OK")
      | otherwise = red (padRight 5 "FAIL")
    counts =
      padRight 18 ("old=" <> show (rcOld r))
        <> padRight 18 ("new=" <> show (rcNew r))
        <> if rcOld r == rcNew r then "" else dim ("diff=" <> show (rcNew r - rcOld r))
