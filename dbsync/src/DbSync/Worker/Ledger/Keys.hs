{-# LANGUAGE DataKinds #-}

-- | Shared aliases for ledger key / credential types, collected so
-- callers don't import @Cardano.Ledger.Keys@ /
-- @Cardano.Ledger.Credential@ / @Cardano.Ledger.Hashes@ piecemeal.
module DbSync.Worker.Ledger.Keys
  ( StakeCred
  , PoolKeyHash
  , DataHash
  ) where

import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))

-- | Credential used to identify a stake key.
type StakeCred = Ledger.Credential Staking

-- | Hash identifying a stake pool.
type PoolKeyHash = KeyHash StakePool

-- | Hash identifying a Plutus datum.
type DataHash = Ledger.DataHash
