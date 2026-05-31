-- | LocalStateQuery mini-protocol handler.
--
-- Reads request\/response pairs from 'sqvRequestVar' and drives the
-- LSQ state machine on the n2c socket: Acquire → Query → Release,
-- writing each response back into the per-request reply TMVar that
-- the caller in 'DbSync.StateQuery' is blocked on.
module DbSync.StateQuery.Handler
  ( localStateQueryHandler
  ) where

import Cardano.Prelude hiding (atomically)

import Control.Concurrent.STM (atomically, putTMVar, takeTMVar)

import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Ledger.Query (Query)
import Ouroboros.Network.Block (Point)
import Ouroboros.Network.Protocol.LocalStateQuery.Client
  ( ClientStAcquired (..)
  , ClientStAcquiring (..)
  , ClientStIdle (..)
  , ClientStQuerying (..)
  , LocalStateQueryClient (..)
  )
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

import DbSync.StateQuery.Types (StateQueryVar (..))

-- | LocalStateQuery protocol client that handles interpreter requests.
--
-- Loops forever, reading requests from the 'StateQueryVar' TMVar,
-- sending them to the node via Acquire → Query → Release, and
-- writing responses back to the response TMVar.
localStateQueryHandler
  :: StateQueryVar
  -> LocalStateQueryClient
       (CardanoBlock StandardCrypto)
       (Point (CardanoBlock StandardCrypto))
       (Query (CardanoBlock StandardCrypto))
       IO
       a
localStateQueryHandler sqv =
  LocalStateQueryClient idleState
  where
    idleState :: IO (ClientStIdle (CardanoBlock StandardCrypto) (Point (CardanoBlock StandardCrypto)) (Query (CardanoBlock StandardCrypto)) IO a)
    idleState = do
      (query, respVar) <- atomically $ takeTMVar (sqvRequestVar sqv)
      pure
        . SendMsgAcquire VolatileTip
        $ ClientStAcquiring
          { recvMsgAcquired =
              pure . SendMsgQuery query $
                ClientStQuerying
                  { recvMsgResult = \result -> do
                      atomically $ putTMVar respVar (Right result)
                      pure $ SendMsgRelease idleState
                  }
          , recvMsgFailure = \failure -> do
              atomically $ putTMVar respVar (Left failure)
              idleState
          }
