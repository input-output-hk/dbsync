{-# LANGUAGE OverloadedStrings #-}

-- | Block processing pipeline for the unified extraction architecture.
--
-- Runs all enabled extractors on a single 'GenericBlock', using the
-- provided 'IdResolver' and 'Writer'. Works identically in both
-- 'IngestChainHistory' and 'FollowingChainTip' — only the resolver
-- and writer implementations differ.
module DbSync.Ingest.Pipeline
  ( -- * Processing
    processBlock
  ) where

import Cardano.Prelude

import DbSync.Block.Types (GenericBlock)
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Resolver (IdResolver)
import DbSync.Writer (Writer)

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

-- | Process a single 'GenericBlock' through all enabled extractors.
--
-- Each extractor's 'pdProcess' function is called sequentially with the
-- shared 'IdResolver' and 'Writer'. The resolver maintains state across
-- extractors (e.g. the block ID assigned by core is visible to UTxO).
--
-- ==== Example
--
-- @
-- resolver <- mkIngestResolver stateRef
-- writer   <- mkCopyWriter connections
-- processBlock resolver writer [coreExtractor] genBlock
-- @
processBlock
  :: IdResolver IO
  -> Writer IO
  -> [ExtractorDef]
  -> GenericBlock
  -> IO ()
processBlock resolver writer extractors block =
  forM_ extractors $ \ext ->
    pdProcess ext resolver writer block
