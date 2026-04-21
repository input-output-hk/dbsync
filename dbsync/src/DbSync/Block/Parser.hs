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
  BlockByron _byronBlk     -> panic "TODO: Byron block conversion (Step 4)"
  -- Shelley era (TPraos consensus)
  BlockShelley _shelleyBlk -> panic "TODO: Shelley block conversion (Step 3)"
  -- Allegra era (TPraos, adds validity intervals)
  BlockAllegra _allegraBlk -> panic "TODO: Allegra block conversion (Step 3)"
  -- Mary era (TPraos, adds multi-asset values)
  BlockMary _maryBlk       -> panic "TODO: Mary block conversion (Step 3)"
  -- Alonzo era (TPraos, adds Plutus smart contracts)
  BlockAlonzo _alonzoBlk   -> panic "TODO: Alonzo block conversion (Step 3)"
  -- Babbage era (Praos consensus, adds inline datums, reference scripts)
  BlockBabbage _babbageBlk -> panic "TODO: Babbage block conversion (Step 3)"
  -- Conway era (Praos, adds on-chain governance)
  BlockConway _conwayBlk   -> panic "TODO: Conway block conversion (Step 3)"
  -- Dijkstra era (Praos, post-Conway)
  BlockDijkstra _dijkBlk   -> panic "TODO: Dijkstra block conversion (Step 3)"
