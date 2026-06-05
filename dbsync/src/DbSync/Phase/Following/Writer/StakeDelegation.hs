-- | hasql writers for tables owned by the @stake_delegation@ extractor.
--
-- The three pot-rebalancing tables (@pot_transfer@, @treasury@,
-- @reserve@) have no Follow-phase insert wiring yet; their writers
-- panic via 'todoWriteLeaf' if invoked.
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
import DbSync.Db.Statement.StakeAddress (insertStakeAddressRowStmt)
import DbSync.Db.Statement.StakeDeregistration (insertStakeDeregistrationRowStmt)
import DbSync.Db.Statement.StakeRegistration (insertStakeRegistrationRowStmt)
import DbSync.Db.Statement.Withdrawal (insertWithdrawalRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn, todoWriteLeaf)

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

-- Follow-phase insert plumbing for the pot-rebalancing tables is
-- not yet wired; the writers panic if invoked.

writePotTransferConn :: Conn.Connection -> PotTransfer -> IO ()
writePotTransferConn _ = todoWriteLeaf "writePotTransfer"

writePotTransferBuf :: WriteBuffer -> PotTransfer -> IO ()
writePotTransferBuf _ = todoWriteLeaf "writePotTransfer"

writeTreasuryConn :: Conn.Connection -> Treasury -> IO ()
writeTreasuryConn _ = todoWriteLeaf "writeTreasury"

writeTreasuryBuf :: WriteBuffer -> Treasury -> IO ()
writeTreasuryBuf _ = todoWriteLeaf "writeTreasury"

writeReserveConn :: Conn.Connection -> Reserve -> IO ()
writeReserveConn _ = todoWriteLeaf "writeReserve"

writeReserveBuf :: WriteBuffer -> Reserve -> IO ()
writeReserveBuf _ = todoWriteLeaf "writeReserve"
