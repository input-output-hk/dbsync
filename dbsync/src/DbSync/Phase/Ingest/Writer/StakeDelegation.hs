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
import DbSync.Db.Schema.Ids (StakeAddressId)
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

writeStakeRegistrationCopy :: LoaderStream -> StakeRegistration -> IO ()
writeStakeRegistrationCopy ls sr = lsWriteRow ls (tdName stakeRegistrationTableDef) (encodeStakeRegistrationCopy sr)

writeStakeDeregistrationCopy :: LoaderStream -> StakeDeregistration -> IO ()
writeStakeDeregistrationCopy ls sd = lsWriteRow ls (tdName stakeDeregistrationTableDef) (encodeStakeDeregistrationCopy sd)

writeDelegationCopy :: LoaderStream -> Delegation -> IO ()
writeDelegationCopy ls d = lsWriteRow ls (tdName delegationTableDef) (encodeDelegationCopy d)

writeWithdrawalCopy :: LoaderStream -> Withdrawal -> IO ()
writeWithdrawalCopy ls w = lsWriteRow ls (tdName withdrawalTableDef) (encodeWithdrawalCopy w)

writePotTransferCopy :: LoaderStream -> PotTransfer -> IO ()
writePotTransferCopy ls pt = lsWriteRow ls (tdName potTransferTableDef) (encodePotTransferCopy pt)

writeTreasuryCopy :: LoaderStream -> Treasury -> IO ()
writeTreasuryCopy ls t = lsWriteRow ls (tdName treasuryTableDef) (encodeTreasuryCopy t)

writeReserveCopy :: LoaderStream -> Reserve -> IO ()
writeReserveCopy ls r = lsWriteRow ls (tdName reserveTableDef) (encodeReserveCopy r)
