-- | COPY writers for tables owned by the @stake_delegation@ extractor.
--
-- Includes the three pot-rebalancing tables (@pot_transfer@,
-- @treasury@, @reserve@): although their wire shape lines up with the
-- epoch-boundary tables, the stake-delegation extractor is the one
-- that actually emits them (from MIR certificates and treasury
-- withdrawals).
module DbSync.Phase.Ingest.Writer.StakeDelegation
  ( writeStakeAddressCopy
  , writeStakeRegistrationCopy
  , writeStakeDeregistrationCopy
  , writeDelegationCopy
  , writeWithdrawalCopy
  , writePotTransferCopy
  , writeTreasuryCopy
  , writeReserveCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.EpochBoundary
  ( PotTransfer
  , Reserve
  , Treasury
  , encodePotTransferCopy
  , encodeReserveCopy
  , encodeTreasuryCopy
  , potTransferTableDef
  , reserveTableDef
  , treasuryTableDef
  )
import DbSync.Db.Schema.Ids
  ( DelegationId
  , PotTransferId
  , ReserveId
  , StakeAddressId
  , StakeDeregistrationId
  , StakeRegistrationId
  , TreasuryId
  , WithdrawalId
  )
import DbSync.Db.Schema.StakeDelegation
  ( Delegation
  , StakeAddress
  , StakeDeregistration
  , StakeRegistration
  , Withdrawal
  , delegationTableDef
  , encodeDelegationCopy
  , encodeStakeAddressCopy
  , encodeStakeDeregistrationCopy
  , encodeStakeRegistrationCopy
  , encodeWithdrawalCopy
  , stakeAddressTableDef
  , stakeDeregistrationTableDef
  , stakeRegistrationTableDef
  , withdrawalTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))

writeStakeAddressCopy :: LoaderStream -> StakeAddressId -> StakeAddress -> IO ()
writeStakeAddressCopy ls sid sa = lsWriteRow ls (tdName stakeAddressTableDef) (encodeStakeAddressCopy sid sa)

writeStakeRegistrationCopy :: LoaderStream -> StakeRegistrationId -> StakeRegistration -> IO ()
writeStakeRegistrationCopy ls sid sr = lsWriteRow ls (tdName stakeRegistrationTableDef) (encodeStakeRegistrationCopy sid sr)

writeStakeDeregistrationCopy :: LoaderStream -> StakeDeregistrationId -> StakeDeregistration -> IO ()
writeStakeDeregistrationCopy ls sid sd = lsWriteRow ls (tdName stakeDeregistrationTableDef) (encodeStakeDeregistrationCopy sid sd)

writeDelegationCopy :: LoaderStream -> DelegationId -> Delegation -> IO ()
writeDelegationCopy ls did d = lsWriteRow ls (tdName delegationTableDef) (encodeDelegationCopy did d)

writeWithdrawalCopy :: LoaderStream -> WithdrawalId -> Withdrawal -> IO ()
writeWithdrawalCopy ls wid w = lsWriteRow ls (tdName withdrawalTableDef) (encodeWithdrawalCopy wid w)

writePotTransferCopy :: LoaderStream -> PotTransferId -> PotTransfer -> IO ()
writePotTransferCopy ls ptid pt = lsWriteRow ls (tdName potTransferTableDef) (encodePotTransferCopy ptid pt)

writeTreasuryCopy :: LoaderStream -> TreasuryId -> Treasury -> IO ()
writeTreasuryCopy ls tid t = lsWriteRow ls (tdName treasuryTableDef) (encodeTreasuryCopy tid t)

writeReserveCopy :: LoaderStream -> ReserveId -> Reserve -> IO ()
writeReserveCopy ls rid r = lsWriteRow ls (tdName reserveTableDef) (encodeReserveCopy rid r)
