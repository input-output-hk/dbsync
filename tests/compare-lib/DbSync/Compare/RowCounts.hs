module DbSync.Compare.RowCounts
  ( RowCountRow (..)
  , Anchors
  , DedupKeySet (..)
  , rowCountsChecks
  , computeAnchors
  , countTable
  , dedupTables
  , dedupContentMatch
  ) where

import Cardano.Prelude
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Set as Set
import qualified Data.Text as T
import DbSync.Compare.Check (Check (..), CheckOutcome (..))
import DbSync.Compare.Connect (DbConn, queryMaybeInt, queryScalarInt, queryTextList)
import DbSync.Compare.FkGraph (Bound (..), DedupReferrer (..), boundFor)
import DbSync.Compare.Introspect (quoteIdent)
import DbSync.Compare.Schema (renamedOldName)

-- ---------------------------------------------------------------------------
-- * Block-bounded row counts
-- ---------------------------------------------------------------------------

data RowCountRow = RowCountRow
  { rcTable :: !Text
  , rcOld :: !Int64
  , rcNew :: !Int64
  , rcBounded :: !Bool -- ^ False means counted unbounded; the delta is informational.
  , rcMatch :: !Bool
  }
  deriving stock (Eq, Show)

-- Build a count check per comparable table plus a key-set check per dedup
-- table. Anchors are shared across the whole batch through a lazily-filled memo
-- so the per-side anchor queries run once rather than once per table.
rowCountsChecks :: [Text] -> Int64 -> IO [Check]
rowCountsChecks tables blockCeiling = do
  anchorRef <- newIORef Nothing
  let withAnchors oldConn newConn = do
        readIORef anchorRef >>= \case
          Just as -> pure as
          Nothing -> do
            as <- (,) <$> computeAnchors oldConn blockCeiling <*> computeAnchors newConn blockCeiling
            writeIORef anchorRef (Just as)
            pure as
  pure $
    [countCheck withAnchors table | table <- tables]
      <> [dedupCheck withAnchors table | table <- dedupTables]

countCheck :: (DbConn -> DbConn -> IO (Anchors, Anchors)) -> Text -> Check
countCheck withAnchors table =
  Check
    { chkLabel = "count " <> table
    , chkRun = \oldConn newConn -> do
        (oldAnchors, newAnchors) <- withAnchors oldConn newConn
        row <- countTable oldConn newConn oldAnchors newAnchors table
        pure (countOutcome row)
    }

countOutcome :: RowCountRow -> CheckOutcome
countOutcome row
  | not (rcBounded row) = Info ("old=" <> show (rcOld row) <> " new=" <> show (rcNew row) <> " (unbounded)")
  | rcMatch row = Ok ("old=new=" <> show (rcOld row))
  | otherwise =
      Diff ["old=" <> show (rcOld row) <> " new=" <> show (rcNew row) <> " diff=" <> show (rcNew row - rcOld row)]

dedupCheck :: (DbConn -> DbConn -> IO (Anchors, Anchors)) -> Text -> Check
dedupCheck withAnchors table =
  Check
    { chkLabel = "keys " <> table
    , chkRun = \oldConn newConn -> do
        (oldAnchors, newAnchors) <- withAnchors oldConn newConn
        (DedupKeySet olds, DedupKeySet news) <- dedupContentMatch oldConn newConn oldAnchors newAnchors table
        let missingNew = Set.difference olds news
            missingOld = Set.difference news olds
        pure $
          if Set.null missingNew && Set.null missingOld
            then Ok (show (Set.size olds) <> " keys")
            else
              Diff $
                ("missing_new=" <> show (Set.size missingNew) <> " missing_old=" <> show (Set.size missingOld))
                  : exampleLines "missing in new" missingNew
                  <> exampleLines "missing in old" missingOld
    }
  where
    exampleLines label keys
      | Set.null keys = []
      | otherwise = [label <> ": " <> T.intercalate ", " (take 5 (Set.toList keys))]

-- Count one table on both sides, bounded to the shared block ceiling.
countTable :: DbConn -> DbConn -> Anchors -> Anchors -> Text -> IO RowCountRow
countTable oldConn newConn oldAnchors newAnchors table = do
  let bound = boundFor table
  old <- countOne oldConn (renamedOldName table) bound oldAnchors
  new <- countOne newConn table bound newAnchors
  pure (RowCountRow table old new (isBounded bound) (old == new))
  where
    isBounded Unbounded = False
    isBounded _ = True

-- A dedup table is bounded by the highest id any anchor-bounded referrer reaches.
countOne :: DbConn -> Text -> Bound -> Anchors -> IO Int64
countOne conn root bound anchors = case bound of
  BoundDedup referrers -> do
    cutoff <- dedupCutoffId conn anchors referrers
    queryScalarInt conn ("SELECT COUNT(*)::bigint FROM " <> quoteIdent root <> " WHERE id <= " <> show cutoff)
  _ -> queryScalarInt conn (countSql root bound anchors)

-- ---------------------------------------------------------------------------
-- * Dedup content match
-- ---------------------------------------------------------------------------

-- The natural keys of a dedup table within the bounded id range, on one side.
newtype DedupKeySet = DedupKeySet
  { unDedupKeySet :: Set Text
  }
  deriving stock (Eq, Show)

-- Dedup tables whose natural-key contents are compared over the bounded id range.
dedupTables :: [Text]
dedupTables = ["stake_address", "pool_hash", "multi_asset", "slot_leader"]

-- Pull each side's bounded key set so the spec can diff them.
dedupContentMatch :: DbConn -> DbConn -> Anchors -> Anchors -> Text -> IO (DedupKeySet, DedupKeySet)
dedupContentMatch oldConn newConn oldAnchors newAnchors table =
  case boundFor table of
    BoundDedup referrers -> do
      oldCut <- dedupCutoffId oldConn oldAnchors referrers
      newCut <- dedupCutoffId newConn newAnchors referrers
      olds <- dedupKeySet oldConn table oldCut
      news <- dedupKeySet newConn table newCut
      pure (olds, news)
    _ -> pure (DedupKeySet mempty, DedupKeySet mempty)

-- The highest dedup id reachable from any anchor-bounded referrer.
dedupCutoffId :: DbConn -> Anchors -> [DedupReferrer] -> IO Int64
dedupCutoffId conn anchors referrers = do
  maxes <- for referrers referrerMax
  pure (maximum (0 : maxes))
  where
    referrerMax (DedupReferrer table col) =
      case boundFor table of
        BoundAnchor anchorCol parent ->
          fromMaybe 0
            <$> queryMaybeInt
              conn
              ( "SELECT max(" <> quoteIdent col <> ")::bigint FROM " <> quoteIdent table
                  <> " WHERE " <> quoteIdent anchorCol <> " <= " <> show (anchorMax anchors parent)
              )
        _ -> pure 0

-- The set of natural keys present in id range [1..cutoff].
dedupKeySet :: DbConn -> Text -> Int64 -> IO DedupKeySet
dedupKeySet conn table cutoff = do
  keys <- queryTextList conn keySql
  pure (DedupKeySet (Set.fromList keys))
  where
    cols = dedupKeyColumns table
    selected = T.intercalate " || '|' || " (map keyExpr cols)
    keySql =
      "SELECT " <> selected <> " FROM " <> quoteIdent table
        <> " WHERE id <= " <> show cutoff

-- Natural-key columns per dedup table; encode bytea columns to hex text.
dedupKeyColumns :: Text -> [Text]
dedupKeyColumns = \case
  "stake_address" -> ["hash_raw"]
  "pool_hash" -> ["hash_raw"]
  "multi_asset" -> ["policy", "name"]
  "slot_leader" -> ["hash"]
  _ -> []

keyExpr :: Text -> Text
keyExpr col = "encode(" <> quoteIdent col <> ", 'hex')"

-- ---------------------------------------------------------------------------
-- * Anchor max ids
-- ---------------------------------------------------------------------------

data Anchors = Anchors
  { anBlock :: !Int64
  , anBlockNo :: !Int64 -- ^ The shared block_no ceiling itself.
  , anTx :: !Int64
  , anTxOut :: !Int64
  , anPoolUpdate :: !Int64
  , anGovAction :: !Int64
  , anEpoch :: !Int64 -- ^ Epoch of the cutoff block, for epoch-scoped tables.
  }

-- The block id anchor is the lowest id at the cutoff block_no, so the cut lands
-- at the same chain tip on both databases regardless of how each encodes the
-- Byron epoch-boundary blocks that share or null that block_no.
computeAnchors :: DbConn -> Int64 -> IO Anchors
computeAnchors conn blockCeiling = do
  b <- fromMaybe 0 <$> queryMaybeInt conn ("SELECT min(id)::bigint FROM block WHERE block_no = " <> show blockCeiling)
  t <- maxId "tx" ("block_id <= " <> show b)
  o <- maxId "tx_out" ("tx_id <= " <> show t)
  pu <- maxId "pool_update" ("registered_tx_id <= " <> show t)
  g <- maxId "gov_action_proposal" ("tx_id <= " <> show t)
  e <- fromMaybe 0 <$> queryMaybeInt conn ("SELECT max(epoch_no)::bigint FROM block WHERE block_no <= " <> show blockCeiling)
  pure (Anchors b blockCeiling t o pu g e)
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

countSql :: Text -> Bound -> Anchors -> Text
countSql root bound anchors =
  "SELECT COUNT(*)::bigint FROM " <> quoteIdent root <> clause
  where
    clause = case bound of
      BoundBlock -> " WHERE block_no <= " <> show (anBlockNo anchors)
      BoundEpoch -> " WHERE epoch_no <= " <> show (anEpoch anchors)
      BoundAnchor col parent -> " WHERE " <> quoteIdent col <> " <= " <> show (anchorMax anchors parent)
      BoundDedup _ -> ""
      Unbounded -> ""
