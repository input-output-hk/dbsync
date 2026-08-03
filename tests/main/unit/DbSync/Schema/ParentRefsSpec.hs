{-# LANGUAGE OverloadedStrings #-}

-- | One assertion per declared table: every column that names an
-- owning row carries a matching 'ParentRef'. A table that omits one
-- is never reached by the resume trim or the rollback cascade, so its
-- rows outlive the parent they belong to.
module DbSync.Schema.ParentRefsSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Types (ColumnDef (..), ParentRef (..), TableDef (..))
import DbSync.Extractor.Registry (allDeclaredTables)

spec :: Spec
spec = describe "Ownership edges are declared" $
  forM_ allDeclaredTables $ \td ->
    it (T.unpack (tdName td)) $
      undeclaredOwners td `shouldBe` []

-- | Column names that identify an owning row, with the table each
-- points at.
ownerColumns :: [(Text, Text)]
ownerColumns =
  [ ("block_id",               "block")
  , ("tx_id",                  "tx")
  , ("tx_out_id",              "tx_out")
  , ("gov_action_proposal_id", "gov_action_proposal")
  ]

-- | @(table, column)@ pairs whose column matches 'ownerColumns' by
-- name without being an ownership edge.
exempt :: [(Text, Text)]
exempt =
  -- These point backwards at the output being consumed, which belongs
  -- to an earlier tx. Their owner is the consuming tx, reached through
  -- tx_in_id.
  [ ("tx_in",             "tx_out_id")
  , ("collateral_tx_in",  "tx_out_id")
  , ("reference_tx_in",   "tx_out_id")
    -- A vote references the proposal it is cast on; the vote itself
    -- belongs to the tx that carried it.
  , ("voting_procedure",  "gov_action_proposal_id")
  ]

undeclaredOwners :: TableDef -> [Text]
undeclaredOwners td =
  [ name
  | (name, parent) <- ownerColumns
  , name `elem` map cdName (tdColumns td)
  , (tdName td, name) `notElem` exempt
  , not (any (matches name parent) (tdParentRefs td))
  ]
  where
    matches name parent pr =
      prColumn pr == name && prParentTable pr == parent
