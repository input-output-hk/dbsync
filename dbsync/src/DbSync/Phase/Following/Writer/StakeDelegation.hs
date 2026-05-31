-- | hasql writers for tables owned by the @stake_delegation@ extractor.
--
-- The three pot-rebalancing tables (@pot_transfer@, @treasury@,
-- @reserve@) are emitted by the stake-delegation extractor; the
-- Follow insert plumbing for them is not landed yet, hence the
-- 'todoWrite' stubs.
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
  )
import DbSync.Db.Statement.Delegation (insertDelegationRowStmt)
import DbSync.Db.Statement.StakeAddress (insertStakeAddressRowStmt)
import DbSync.Db.Statement.StakeDeregistration (insertStakeDeregistrationRowStmt)
import DbSync.Db.Statement.StakeRegistration (insertStakeRegistrationRowStmt)
import DbSync.Db.Statement.Withdrawal (insertWithdrawalRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn, todoWrite)

writeStakeAddressConn :: Conn.Connection -> StakeAddressId -> StakeAddress -> IO ()
writeStakeAddressConn conn sid sa = runConn conn (sid, sa) insertStakeAddressRowStmt

writeStakeAddressBuf :: WriteBuffer -> StakeAddressId -> StakeAddress -> IO ()
writeStakeAddressBuf buf sid sa = queueBuf buf (sid, sa) insertStakeAddressRowStmt

writeStakeRegistrationConn :: Conn.Connection -> StakeRegistrationId -> StakeRegistration -> IO ()
writeStakeRegistrationConn conn sid sr = runConn conn (sid, sr) insertStakeRegistrationRowStmt

writeStakeRegistrationBuf :: WriteBuffer -> StakeRegistrationId -> StakeRegistration -> IO ()
writeStakeRegistrationBuf buf sid sr = queueBuf buf (sid, sr) insertStakeRegistrationRowStmt

writeStakeDeregistrationConn :: Conn.Connection -> StakeDeregistrationId -> StakeDeregistration -> IO ()
writeStakeDeregistrationConn conn sid sd = runConn conn (sid, sd) insertStakeDeregistrationRowStmt

writeStakeDeregistrationBuf :: WriteBuffer -> StakeDeregistrationId -> StakeDeregistration -> IO ()
writeStakeDeregistrationBuf buf sid sd = queueBuf buf (sid, sd) insertStakeDeregistrationRowStmt

writeDelegationConn :: Conn.Connection -> DelegationId -> Delegation -> IO ()
writeDelegationConn conn did d = runConn conn (did, d) insertDelegationRowStmt

writeDelegationBuf :: WriteBuffer -> DelegationId -> Delegation -> IO ()
writeDelegationBuf buf did d = queueBuf buf (did, d) insertDelegationRowStmt

writeWithdrawalConn :: Conn.Connection -> WithdrawalId -> Withdrawal -> IO ()
writeWithdrawalConn conn wid w = runConn conn (wid, w) insertWithdrawalRowStmt

writeWithdrawalBuf :: WriteBuffer -> WithdrawalId -> Withdrawal -> IO ()
writeWithdrawalBuf buf wid w = queueBuf buf (wid, w) insertWithdrawalRowStmt

-- TODO: insert plumbing for the pot tables; matches the original
-- monolithic writer where these were stubbed out.

writePotTransferConn :: Conn.Connection -> PotTransferId -> PotTransfer -> IO ()
writePotTransferConn _ = todoWrite "writePotTransfer"

writePotTransferBuf :: WriteBuffer -> PotTransferId -> PotTransfer -> IO ()
writePotTransferBuf _ = todoWrite "writePotTransfer"

writeTreasuryConn :: Conn.Connection -> TreasuryId -> Treasury -> IO ()
writeTreasuryConn _ = todoWrite "writeTreasury"

writeTreasuryBuf :: WriteBuffer -> TreasuryId -> Treasury -> IO ()
writeTreasuryBuf _ = todoWrite "writeTreasury"

writeReserveConn :: Conn.Connection -> ReserveId -> Reserve -> IO ()
writeReserveConn _ = todoWrite "writeReserve"

writeReserveBuf :: WriteBuffer -> ReserveId -> Reserve -> IO ()
writeReserveBuf _ = todoWrite "writeReserve"
