{-# LANGUAGE DataKinds #-}

-- | Block parsing: HFC era dispatch.
--
-- Takes a 'CardanoBlock' from the ChainSync protocol and converts it
-- into an era-independent 'GenericBlock' suitable for extraction.
--
-- The dispatch pattern-matches on the Hard Fork Combinator era tags
-- ('BlockByron', 'BlockShelley', etc.) and delegates to era-specific
-- converters in "DbSync.Block.Parser.Byron" and "DbSync.Block.Parser.Shelley".
module DbSync.Block.Parser
  ( -- * Parsing
    parseBlock

    -- * Re-exports
  , EpochSlotInfo (..)
  , stubEpochSlotInfo
  ) where

import Cardano.Prelude

import Ouroboros.Consensus.Cardano.Block
  ( CardanoBlock
  , HardForkBlock
      ( BlockAllegra
      , BlockAlonzo
      , BlockBabbage
      , BlockByron
      , BlockConway
      , BlockDijkstra
      , BlockMary
      , BlockShelley
      )
  , StandardCrypto
  )

import DbSync.Block.Parser.Block
  ( fromAllegraBlock
  , fromAlonzoBlock
  , fromBabbageBlock
  , fromConwayBlock
  , fromDijkstraBlock
  , fromMaryBlock
  , fromShelleyBlock
  )
import DbSync.Block.Parser.Byron (fromByronBlock)
import DbSync.Block.Parser.Types (EpochSlotInfo (..), stubEpochSlotInfo)
import DbSync.Block.Types (GenericBlock)

-- ---------------------------------------------------------------------------
-- * HFC era dispatch
-- ---------------------------------------------------------------------------

-- | Convert a 'CardanoBlock' from the node into an era-independent 'GenericBlock'.
--
-- This is the main entry point for block parsing. It dispatches on the
-- HFC era tag and delegates to era-specific converters.
--
-- __Deferred features (first pass):__
--
--   * Redeemers, scripts, and governance fields are set to empty\/default.
--   * These will be populated when their respective extractors are implemented.
parseBlock :: EpochSlotInfo -> CardanoBlock StandardCrypto -> GenericBlock
parseBlock esi = \case
  -- Byron era (pre-Shelley, includes Epoch Boundary Blocks)
  BlockByron byronBlk      -> fromByronBlock esi byronBlk
  -- Shelley+ eras — all wired to real converters
  BlockShelley shelleyBlk  -> fromShelleyBlock esi shelleyBlk
  BlockAllegra allegraBlk  -> fromAllegraBlock esi allegraBlk
  BlockMary maryBlk        -> fromMaryBlock esi maryBlk
  BlockAlonzo alonzoBlk    -> fromAlonzoBlock esi alonzoBlk
  BlockBabbage babbageBlk  -> fromBabbageBlock esi babbageBlk
  BlockConway conwayBlk    -> fromConwayBlock esi conwayBlk
  BlockDijkstra dijkBlk    -> fromDijkstraBlock esi dijkBlk
