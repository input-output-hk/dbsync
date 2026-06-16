module DbSync.Compare.FkGraph
  ( Bound (..)
  , boundFor
  ) where

import Cardano.Prelude
import Data.List (lookup)
import DbSync.Db.Schema.Types (ColumnDef (..), ForeignKey (..), TableDef (..))
import DbSync.Extractor.Registry (allDeclaredTables)

-- ---------------------------------------------------------------------------
-- * Epoch bounding
-- ---------------------------------------------------------------------------

data Bound
  = BoundEpoch
  | BoundAnchor Text Text -- ^ FK column and the anchor table it references.
  | Unbounded
  deriving stock (Eq, Show)

boundFor :: Text -> Bound
boundFor name
  | name == "epoch_finalized" = Unbounded
  | "epoch_no" `elem` columnsOf name = BoundEpoch
  | Just (col, parent) <- anchorEdge name = BoundAnchor col parent
  | otherwise = Unbounded

-- Anchor surrogate ids increase monotonically with chain order, so a bounded
-- max id on each gives an epoch cut without a join.
anchorTables :: [Text]
anchorTables = ["block", "tx", "tx_out", "pool_update", "gov_action_proposal"]

anchorEdge :: Text -> Maybe (Text, Text)
anchorEdge name =
  listToMaybe [(col, parent) | (col, parent) <- edgesOf name, parent `elem` anchorTables]

-- ---------------------------------------------------------------------------
-- * Foreign-key edges
-- ---------------------------------------------------------------------------

tableDefs :: [(Text, TableDef)]
tableDefs = [(tdName t, t) | t <- allDeclaredTables]

columnsOf :: Text -> [Text]
columnsOf name = maybe [] (map cdName . tdColumns) (lookup name tableDefs)

edgesOf :: Text -> [(Text, Text)]
edgesOf name = realEdges <> syntheticEdges
  where
    cols = columnsOf name
    realEdges =
      [ (fkColumn fk, fkParentTable fk)
      | Just td <- [lookup name tableDefs]
      , fk <- tdForeignKeys td
      ]
    realCols = map fst realEdges
    syntheticEdges =
      [ (col, parent)
      | (col, parent) <- syntheticAnchors
      , col `elem` cols
      , col `notElem` realCols
      ]

-- Anchor columns the schema does not always record as foreign keys.
syntheticAnchors :: [(Text, Text)]
syntheticAnchors =
  [ ("tx_id", "tx")
  , ("registered_tx_id", "tx")
  , ("announced_tx_id", "tx")
  , ("tx_out_id", "tx_out")
  , ("pool_update_id", "pool_update")
  , ("gov_action_proposal_id", "gov_action_proposal")
  ]