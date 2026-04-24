{-# LANGUAGE DataKinds #-}

{- |
Module      : DbSync.Ledger.Keys
Description : Shared aliases for ledger key / credential types.

Three tiny aliases used throughout the ledger code and most
extractors. Collected here so callers don't have to import
@Cardano.Ledger.Keys@ / @Cardano.Ledger.Credential@ /
@Cardano.Ledger.Hashes@ piecemeal, and so a future rename (say,
@PoolKeyHash@ → @StakePoolHash@) is a one-line change.
-}
module DbSync.Ledger.Keys
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
