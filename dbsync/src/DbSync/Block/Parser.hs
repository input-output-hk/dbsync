{-# LANGUAGE DataKinds #-}

-- | Block parsing: HFC era dispatch.
--
-- Takes a 'CardanoBlock' from the ChainSync protocol and converts it
-- into an era-independent 'GenericBlock' suitable for extraction.
--
-- The dispatch pattern-matches on the Hard Fork Combinator era tags
-- ('BlockByron', 'BlockShelley', etc.) and delegates to era-specific
-- converters in "DbSync.Block.Parser.Byron" and "DbSync.Block.Parser.Block".
module DbSync.Block.Parser
  ( -- * Parsing
    parseBlock
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
import DbSync.Block.Types (GenericBlock)
import DbSync.StateQuery (SlotDetails)

-- ---------------------------------------------------------------------------
-- * HFC era dispatch
-- ---------------------------------------------------------------------------

-- | Convert a 'CardanoBlock' from the node into an era-independent 'GenericBlock'.
--
-- Takes 'SlotDetails' (computed from the HardFork Interpreter) which provides
-- the correct epoch number, slot-within-epoch, and time for any slot across
-- all era transitions.
parseBlock :: SlotDetails -> CardanoBlock StandardCrypto -> GenericBlock
parseBlock sd = \case
  -- Byron era (pre-Shelley, includes Epoch Boundary Blocks)
  BlockByron byronBlk      -> fromByronBlock sd byronBlk
  -- Shelley+ eras — all wired to real converters
  BlockShelley shelleyBlk  -> fromShelleyBlock sd shelleyBlk
  BlockAllegra allegraBlk  -> fromAllegraBlock sd allegraBlk
  BlockMary maryBlk        -> fromMaryBlock sd maryBlk
  BlockAlonzo alonzoBlk    -> fromAlonzoBlock sd alonzoBlk
  BlockBabbage babbageBlk  -> fromBabbageBlock sd babbageBlk
  BlockConway conwayBlk    -> fromConwayBlock sd conwayBlk
  BlockDijkstra dijkBlk    -> fromDijkstraBlock sd dijkBlk
