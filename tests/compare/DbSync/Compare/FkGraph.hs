module DbSync.Compare.FkGraph
  ( Bound (..)
  , DedupReferrer (..)
  , boundFor
  ) where

import Cardano.Prelude
import Data.List (lookup)
import DbSync.Db.Schema.Types (ColumnDef (..), ParentRef (..), TableDef (..))
import DbSync.Extractor.Registry (allDeclaredTables)

-- ---------------------------------------------------------------------------
-- * Epoch bounding
-- ---------------------------------------------------------------------------

data Bound
  = BoundBlock -- ^ The block table itself, cut at the shared block_no.
  | BoundEpoch
  | BoundAnchor Text Text -- ^ FK column and the anchor table it references.
  | BoundDedup [DedupReferrer] -- ^ Dedup table bounded by the ids its referrers reach.
  | Unbounded
  deriving stock (Eq, Show)

-- A column on an anchor-bounded table that points back at a dedup table.
data DedupReferrer = DedupReferrer
  { drTable :: !Text
  , drColumn :: !Text
  }
  deriving stock (Eq, Show)

-- An anchor edge beats an epoch bound: it cuts at the exact shared
-- chain point, while an epoch bound necessarily includes the
-- in-progress epoch's rows on whichever side is further ahead.
boundFor :: Text -> Bound
boundFor name
  | name == "epoch_finalized" = Unbounded
  | name == "block" = BoundBlock
  | Just referrers <- lookup name dedupReferrers = BoundDedup referrers
  | Just (col, parent) <- anchorEdge name = BoundAnchor col parent
  | "epoch_no" `elem` columnsOf name = BoundEpoch
  | otherwise = Unbounded

-- ---------------------------------------------------------------------------
-- * Dedup tables
-- ---------------------------------------------------------------------------

-- Dedup tables carry no chain position: a row is written the first time its
-- key is seen and shared forever after. Their ids are assigned in chain order,
-- so bounding by the greatest id any anchor-bounded referrer reaches gives the
-- same cut on both databases. Each referrer must itself be anchor-bounded; an
-- unbounded referrer would drag the cut back to the tip and defeat the bound.
dedupReferrers :: [(Text, [DedupReferrer])]
dedupReferrers =
  [ ("stake_address", referrers stakeAddressReferrers)
  , ("pool_hash", referrers poolHashReferrers)
  , ("multi_asset", referrers multiAssetReferrers)
  , ("slot_leader", referrers [("block", "slot_leader_id")])
  ]
  where
    referrers = mapMaybe keepAnchorBounded
    keepAnchorBounded (table, col)
      | isAnchorBounded table = Just (DedupReferrer table col)
      | otherwise = Nothing

stakeAddressReferrers :: [(Text, Text)]
stakeAddressReferrers =
  [ ("tx_out", "stake_address_id")
  , ("collateral_tx_out", "stake_address_id")
  , ("address", "stake_address_id")
  , ("stake_registration", "addr_id")
  , ("stake_deregistration", "addr_id")
  , ("delegation", "addr_id")
  , ("withdrawal", "addr_id")
  , ("reward", "addr_id")
  ]

poolHashReferrers :: [(Text, Text)]
poolHashReferrers =
  [ ("pool_update", "hash_id")
  , ("pool_retire", "hash_id")
  , ("delegation", "pool_hash_id")
  ]

multiAssetReferrers :: [(Text, Text)]
multiAssetReferrers =
  [ ("ma_tx_out", "ident")
  , ("ma_tx_mint", "ident")
  ]

isAnchorBounded :: Text -> Bool
isAnchorBounded name = case anchorEdge name of
  Just _ -> True
  Nothing -> False

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
      [ (prColumn fk, prParentTable fk)
      | Just td <- [lookup name tableDefs]
      , fk <- tdParentRefs td
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