{-# LANGUAGE OverloadedStrings #-}

-- | Block processing pipeline for the IngestChainHistory phase.
--
-- Composes multiple 'ExtractorDef' extractors into a single
-- processing function that transforms a 'GenericBlock' into
-- merged 'RowBatches' ready for COPY streaming.
--
-- All functions are __pure__ — no IO, no database access.
-- The caller is responsible for feeding blocks and writing rows.
module DbSync.Ingest.Pipeline
  ( -- * Processing
    processBlock
  ) where

import Cardano.Prelude

import qualified Data.Map.Strict as Map

import DbSync.Block.Types (GenericBlock)
import DbSync.Extractor
  ( ExtractFn
  , ExtractState (..)
  , ExtractorDef (..)
  , RowBatches (..)
  )

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

-- | Process a single 'GenericBlock' through all enabled extractors.
--
-- Runs each extractor's 'pdExtract' function sequentially, threading
-- the 'ExtractState' through and merging the resulting 'RowBatches'
-- via their 'Semigroup' instance (which merges the inner 'Map' by
-- concatenating row lists per table).
--
-- __Pure function__ — suitable for testing without IO.
--
-- ==== Example
--
-- @
-- let extractors = [coreExtractor]
--     (batches, state') = processBlock extractors genBlock initState
-- -- batches contains rows for "block", "tx", "slot_leader"
-- @
processBlock
  :: [ExtractorDef]
  -> GenericBlock
  -> ExtractState
  -> (RowBatches, ExtractState)
processBlock extractors block st0 =
  foldl' step (mempty, st0) extractors
  where
    step :: (RowBatches, ExtractState) -> ExtractorDef -> (RowBatches, ExtractState)
    step (accBatches, st) extractor =
      let (newBatches, st') = pdExtract extractor block st
      in (mergeBatches accBatches newBatches, st')

-- | Merge two 'RowBatches' by concatenating row lists for each table.
--
-- Uses 'Map.unionWith' to combine entries: if both batches have rows
-- for the same table, the rows are concatenated (left then right).
mergeBatches :: RowBatches -> RowBatches -> RowBatches
mergeBatches (RowBatches a) (RowBatches b) =
  RowBatches $ Map.unionWith (<>) a b
