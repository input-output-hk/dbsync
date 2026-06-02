-- | Ingest 'IdResolver' fragments for the @stake_delegation@ extractor.
--
-- Includes the three pot-rebalancing ID assigners (treasury,
-- reserve, pot_transfer) because the stake-delegation extractor is
-- the one that emits them.
module DbSync.Phase.Ingest.Resolver.StakeDelegation
  ( resolveStakeAddressIngest
  , assignStakeRegistrationIdIngest
  , assignStakeDeregistrationIdIngest
  , assignDelegationIdIngest
  , assignWithdrawalIdIngest
  , assignPotTransferIdIngest
  , assignTreasuryIdIngest
  , assignReserveIdIngest
  ) where

import Cardano.Prelude

import qualified Data.ByteString.Short as SBS
import Data.IORef (IORef)

import DbSync.Db.Schema.Ids
  ( DelegationId (..)
  , PotTransferId (..)
  , ReserveId (..)
  , StakeAddressId (..)
  , StakeDeregistrationId (..)
  , StakeRegistrationId (..)
  , TreasuryId (..)
  , WithdrawalId (..)
  )
import DbSync.Db.Schema.StakeDelegation (StakeAddress)
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounters (..))
import DbSync.Phase.Ingest.DedupStore (DedupStores (..), lookupOrInsert)
import DbSync.Phase.Ingest.Resolver.Internal (allocateNextId)

-- | Dedup lookup against the LSM-backed stake_address table.
resolveStakeAddressIngest
  :: DedupStores -> ByteString -> StakeAddress -> IO (StakeAddressId, Bool)
resolveStakeAddressIngest dedupStores hash _sa = do
  let !key = SBS.toShort hash
  (saId, isNew) <- lookupOrInsert key (dstStakeAddress dedupStores)
  pure (StakeAddressId saId, isNew)

assignStakeRegistrationIdIngest :: IORef ExtractState -> IO StakeRegistrationId
assignStakeRegistrationIdIngest extractStateRef =
  allocateNextId extractStateRef icStakeRegistrationId (\cs c -> cs { icStakeRegistrationId = c }) StakeRegistrationId

assignStakeDeregistrationIdIngest :: IORef ExtractState -> IO StakeDeregistrationId
assignStakeDeregistrationIdIngest extractStateRef =
  allocateNextId extractStateRef icStakeDeregistrationId (\cs c -> cs { icStakeDeregistrationId = c }) StakeDeregistrationId

assignDelegationIdIngest :: IORef ExtractState -> IO DelegationId
assignDelegationIdIngest extractStateRef =
  allocateNextId extractStateRef icDelegationId (\cs c -> cs { icDelegationId = c }) DelegationId

assignWithdrawalIdIngest :: IORef ExtractState -> IO WithdrawalId
assignWithdrawalIdIngest extractStateRef =
  allocateNextId extractStateRef icWithdrawalId (\cs c -> cs { icWithdrawalId = c }) WithdrawalId

assignPotTransferIdIngest :: IORef ExtractState -> IO PotTransferId
assignPotTransferIdIngest extractStateRef =
  allocateNextId extractStateRef icPotTransferId (\cs c -> cs { icPotTransferId = c }) PotTransferId

assignTreasuryIdIngest :: IORef ExtractState -> IO TreasuryId
assignTreasuryIdIngest extractStateRef =
  allocateNextId extractStateRef icTreasuryId (\cs c -> cs { icTreasuryId = c }) TreasuryId

assignReserveIdIngest :: IORef ExtractState -> IO ReserveId
assignReserveIdIngest extractStateRef =
  allocateNextId extractStateRef icReserveId (\cs c -> cs { icReserveId = c }) ReserveId
