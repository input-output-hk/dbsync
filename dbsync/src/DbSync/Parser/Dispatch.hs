{-# LANGUAGE DataKinds #-}

-- | Turns a 'CardanoBlock' from ChainSync into an era-independent
-- 'GenericBlock'. The dispatch matches on the Hard Fork Combinator era
-- tag and calls the converter in "DbSync.Parser.Byron" or
-- "DbSync.Parser.Block".
module DbSync.Parser.Dispatch
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

import DbSync.Parser.Block
  ( fromAllegraBlock
  , fromAlonzoBlock
  , fromBabbageBlock
  , fromConwayBlock
  , fromDijkstraBlock
  , fromMaryBlock
  , fromShelleyBlock
  )
import DbSync.Parser.Byron (fromByronBlock)
import DbSync.Parser.Types (GenericBlock (..), GenericTx (..))
import DbSync.StateQuery (SlotDetails)

-- ---------------------------------------------------------------------------
-- * HFC era dispatch
-- ---------------------------------------------------------------------------

-- | The 'SlotDetails', which come from the HardFork Interpreter, give
-- the correct epoch number, slot-within-epoch and time for any slot
-- across every era transition.
--
-- The 'Bool' says whether the @cbor@ extractor is on. When it is off,
-- 'gateTxCbor' clears the per-tx raw-CBOR field, so its thunks do not
-- pin the decoded ledger transactions for the life of the block.
parseBlock :: Bool -> SlotDetails -> CardanoBlock StandardCrypto -> GenericBlock
parseBlock cborEnabled sd = gateTxCbor cborEnabled . \case
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

-- | Drops each transaction's raw CBOR when the @cbor@ extractor is
-- off. The parser builds that field speculatively, and its thunk pins
-- the whole decoded ledger transaction until something forces it.
-- Clearing it lets the ledger txs go as soon as the parse ends.
gateTxCbor :: Bool -> GenericBlock -> GenericBlock
gateTxCbor True  gb = gb
gateTxCbor False gb = gb {blkTxs = map clearTxCbor (blkTxs gb)}
  where
    clearTxCbor gtx = gtx {txCborRaw = Nothing}
