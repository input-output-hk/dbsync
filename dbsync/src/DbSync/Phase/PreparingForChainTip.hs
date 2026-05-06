{-# LANGUAGE OverloadedStrings #-}

-- | PreparingForChainTip phase. Currently only the schema-flip step;
-- index creation, ANALYZE, and sequence-reset are TODO.
module DbSync.Phase.PreparingForChainTip
  ( run
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.Init (prepareSchemaForFollowTip)
import DbSync.Db.Schema.Types (TableDef)

run :: [TableDef] -> Text -> IO ()
run = prepareSchemaForFollowTip
