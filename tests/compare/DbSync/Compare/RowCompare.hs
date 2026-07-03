module DbSync.Compare.RowCompare
  ( KeyedRow (..)
  , RowDiff (..)
  , toKeyedRow
  , compareRowSets
  , diffKey
  ) where

import Cardano.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- ---------------------------------------------------------------------------
-- * Keyed rows
-- ---------------------------------------------------------------------------

-- A row reduced to its natural key plus the fields worth comparing. Surrogate
-- ids, FK columns, and always-null columns are projected away before this
-- point, so two rows with equal keys and equal fields are equal in content.
data KeyedRow = KeyedRow
  { krKey :: ![Text] -- ^ Natural-key values, rendered as text.
  , krFields :: ![(Text, Maybe Text)] -- ^ (label, value) in a fixed field order.
  }
  deriving stock (Eq, Show)

-- Split a raw projected row into key columns (the first @keyCount@) and the
-- labelled value columns that follow. A null key column is unexpected and
-- folds to a sentinel so it still sorts and compares deterministically.
toKeyedRow :: Int -> [Text] -> [Maybe Text] -> KeyedRow
toKeyedRow keyCount labels raw =
  KeyedRow
    { krKey = map (fromMaybe "<null>") (take keyCount raw)
    , krFields = zip labels (drop keyCount raw)
    }

-- ---------------------------------------------------------------------------
-- * Diffing
-- ---------------------------------------------------------------------------

data RowDiff
  = MissingInNew ![Text] -- ^ Key present in old, absent in new.
  | MissingInOld ![Text] -- ^ Key present in new, absent in old.
  | FieldMismatch ![Text] !Text !(Maybe Text) !(Maybe Text) -- ^ key, field, old, new.
  deriving stock (Eq, Show)

compareRowSets :: [KeyedRow] -> [KeyedRow] -> [RowDiff]
compareRowSets olds news =
  concatMap (diffKey oldMap newMap) (Set.toAscList allKeys)
  where
    oldMap = Map.fromList [(krKey r, r) | r <- olds]
    newMap = Map.fromList [(krKey r, r) | r <- news]
    allKeys = Map.keysSet oldMap <> Map.keysSet newMap

diffKey :: Map.Map [Text] KeyedRow -> Map.Map [Text] KeyedRow -> [Text] -> [RowDiff]
diffKey oldMap newMap key =
  case (Map.lookup key oldMap, Map.lookup key newMap) of
    (Just o, Just n) -> diffFields key (krFields o) (krFields n)
    (Just _, Nothing) -> [MissingInNew key]
    (Nothing, Just _) -> [MissingInOld key]
    (Nothing, Nothing) -> []

diffFields :: [Text] -> [(Text, Maybe Text)] -> [(Text, Maybe Text)] -> [RowDiff]
diffFields key olds news =
  [ FieldMismatch key label ov nv
  | ((label, ov), (_, nv)) <- zip olds news
  , ov /= nv
  ]
