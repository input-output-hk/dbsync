{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}

-- | Higher-level Conway-era forging scenarios.
--
-- Each scenario forges a single block whose txs prepare governance
-- state (DRep distribution, committee hot-key authorization) that
-- later proposals and votes depend on.
module Cardano.Mock.Forging.Tx.Conway.Scenarios
  ( registerDRepsAndDelegateVotes
  , registerCommitteeCreds
  ) where

import Cardano.Ledger.Address (Withdrawals (..))
import Cardano.Ledger.Conway.TxCert (Delegatee (..))
import Cardano.Ledger.Core (TopTx, Tx ())
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Mock.Forging.Interpreter
  ( Interpreter
  , forgeNextFindLeader
  , withConwayLedgerState
  )
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Tx.Generic
  ( bootstrapCommitteeCreds
  , resolveStakeCreds
  , unregisteredDRepIds
  )
import Cardano.Mock.Forging.Types
  ( CardanoBlock
  , ForgingError
  , StakeIndex (..)
  , TxEra (..)
  , UTxOIndex (..)
  )
import Cardano.Prelude
import Ouroboros.Consensus.Shelley.Eras (ConwayEra)
import qualified Prelude

-- | Build a block that funds a fresh stake address, registers a
-- DRep, and delegates the stake's vote to that DRep. Prerequisite
-- for any gov-action proposal that needs DRep voters to ratify.
registerDRepsAndDelegateVotes :: Interpreter -> IO CardanoBlock
registerDRepsAndDelegateVotes interpreter = do
  drepId <- case unregisteredDRepIds of
    d : _ -> pure d
    []    -> Prelude.error "registerDRepsAndDelegateVotes: unregisteredDRepIds is empty"
  blockTxs <-
    withConwayLedgerState interpreter $
      registerDRepAndDelegateVotes' drepId (StakeIndex 4)
  forgeNextFindLeader interpreter (map TxConway blockTxs)

registerDRepAndDelegateVotes'
  :: Credential DRepRole
  -> StakeIndex
  -> Conway.ConwayLedgerState mk
  -> Either ForgingError [Tx TopTx ConwayEra]
registerDRepAndDelegateVotes' drepId stakeIx ledger = do
  stakeCred <- resolveStakeCreds stakeIx ledger
  let utxoStake = UTxOAddressNewWithStake 0 stakeIx
      regDelegCert =
        Conway.mkDelegTxCert (DelegVote (DRepCredential drepId)) stakeCred
  paymentTx <- Conway.mkPaymentTx (UTxOIndex 0) utxoStake 10_000 500 0 ledger
  regTx     <- Conway.mkRegisterDRepTx drepId
  regDelegTx <- Conway.mkDCertTx [regDelegCert] (Withdrawals mempty) Nothing
  pure [paymentTx, regTx, regDelegTx]

-- | Build a block whose txs authorize a hot key for every
-- bootstrap-committee cold key seeded in @genesis.conway.json@.
-- Prerequisite for committee voters to ratify gov actions.
registerCommitteeCreds :: Interpreter -> IO CardanoBlock
registerCommitteeCreds interpreter = do
  blockTxs <- withConwayLedgerState interpreter $ \_ ->
    mapM (uncurry Conway.mkCommitteeAuthTx) bootstrapCommitteeCreds
  forgeNextFindLeader interpreter (map TxConway blockTxs)
