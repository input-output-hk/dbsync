-- | Messages flowing from the chainsync receiver to its consumers.
--
-- Carried on a single 'TBQueue' so rollback markers stay in FIFO order
-- with the forward blocks they invalidate.
module DbSync.Node.ChainSyncMsg
  ( ChainSyncMsg (..)
  ) where

import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)

import DbSync.Block.Types (CardanoPoint)

data ChainSyncMsg
  = MsgForward  !(CardanoBlock StandardCrypto)
  | MsgRollback !CardanoPoint
