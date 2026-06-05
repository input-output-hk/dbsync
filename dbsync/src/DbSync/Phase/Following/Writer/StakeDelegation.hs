-- | hasql writers for tables owned by the @stake_delegation@ extractor.
--
-- The three pot-rebalancing tables (@pot_transfer@, @treasury@,
-- @reserve@) only receive rows from MIR certificates, which are
-- Shelley→Babbage only. Follow runs at chain tip where the active
-- era is Conway+, so these writers are unreachable in production.
-- They are wired symmetrically with the Ingest path anyway, so a
-- future deeper-rollforward scenario remains correct.
module DbSync.Phase.Following.Writer.StakeDelegation
  ( writeStakeAddressConn
  , writeStakeAddressBuf
  , writeStakeRegistrationConn
  , writeStakeRegistrationBuf
  , writeStakeDeregistrationConn
  , writeStakeDeregistrationBuf
  , writeDelegationConn
  , writeDelegationBuf
  , writeWithdrawalConn
  , writeWithdrawalBuf
  , writePotTransferConn
  , writePotTransferBuf
  , writeTreasuryConn
  , writeTreasuryBuf
  , writeReserveConn
  , writeReserveBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.EpochBoundary (PotTransfer, Reserve, Treasury)
import DbSync.Db.Schema.Ids (StakeAddressId)
import DbSync.Db.Schema.StakeDelegation
  ( Delegation
  , StakeAddress
  , StakeDeregistration
  , StakeRegistration
  , Withdrawal
  )
import DbSync.Db.Statement.Delegation (insertDelegationRowStmt)
import DbSync.Db.Statement.PotTransfer (insertPotTransferRowStmt)
import DbSync.Db.Statement.Reserve (insertReserveRowStmt)
import DbSync.Db.Statement.StakeAddress (insertStakeAddressRowStmt)
import DbSync.Db.Statement.StakeDeregistration (insertStakeDeregistrationRowStmt)
import DbSync.Db.Statement.StakeRegistration (insertStakeRegistrationRowStmt)
import DbSync.Db.Statement.Treasury (insertTreasuryRowStmt)
import DbSync.Db.Statement.Withdrawal (insertWithdrawalRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

writeStakeAddressConn :: Conn.Connection -> StakeAddressId -> StakeAddress -> IO ()
writeStakeAddressConn conn sid sa = runConn conn (sid, sa) insertStakeAddressRowStmt

writeStakeAddressBuf :: WriteBuffer -> StakeAddressId -> StakeAddress -> IO ()
writeStakeAddressBuf buf sid sa = queueBuf buf (sid, sa) insertStakeAddressRowStmt

writeStakeRegistrationConn :: Conn.Connection -> StakeRegistration -> IO ()
writeStakeRegistrationConn conn sr = runConn conn sr insertStakeRegistrationRowStmt

writeStakeRegistrationBuf :: WriteBuffer -> StakeRegistration -> IO ()
writeStakeRegistrationBuf buf sr = queueBuf buf sr insertStakeRegistrationRowStmt

writeStakeDeregistrationConn :: Conn.Connection -> StakeDeregistration -> IO ()
writeStakeDeregistrationConn conn sd = runConn conn sd insertStakeDeregistrationRowStmt

writeStakeDeregistrationBuf :: WriteBuffer -> StakeDeregistration -> IO ()
writeStakeDeregistrationBuf buf sd = queueBuf buf sd insertStakeDeregistrationRowStmt

writeDelegationConn :: Conn.Connection -> Delegation -> IO ()
writeDelegationConn conn d = runConn conn d insertDelegationRowStmt

writeDelegationBuf :: WriteBuffer -> Delegation -> IO ()
writeDelegationBuf buf d = queueBuf buf d insertDelegationRowStmt

writeWithdrawalConn :: Conn.Connection -> Withdrawal -> IO ()
writeWithdrawalConn conn w = runConn conn w insertWithdrawalRowStmt

writeWithdrawalBuf :: WriteBuffer -> Withdrawal -> IO ()
writeWithdrawalBuf buf w = queueBuf buf w insertWithdrawalRowStmt

writePotTransferConn :: Conn.Connection -> PotTransfer -> IO ()
writePotTransferConn conn pt = runConn conn pt insertPotTransferRowStmt

writePotTransferBuf :: WriteBuffer -> PotTransfer -> IO ()
writePotTransferBuf buf pt = queueBuf buf pt insertPotTransferRowStmt

writeTreasuryConn :: Conn.Connection -> Treasury -> IO ()
writeTreasuryConn conn t = runConn conn t insertTreasuryRowStmt

writeTreasuryBuf :: WriteBuffer -> Treasury -> IO ()
writeTreasuryBuf buf t = queueBuf buf t insertTreasuryRowStmt

writeReserveConn :: Conn.Connection -> Reserve -> IO ()
writeReserveConn conn r = runConn conn r insertReserveRowStmt

writeReserveBuf :: WriteBuffer -> Reserve -> IO ()
writeReserveBuf buf r = queueBuf buf r insertReserveRowStmt
