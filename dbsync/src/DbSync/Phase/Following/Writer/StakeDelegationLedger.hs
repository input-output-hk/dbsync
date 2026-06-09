-- | hasql writers for the four ledger-derived tables owned by the
-- @stake_delegation_ledger@ extractor.
module DbSync.Phase.Following.Writer.StakeDelegationLedger
  ( writeRewardConn
  , writeRewardBuf
  , writePotRewardConn
  , writePotRewardBuf
  , writeEpochStakeConn
  , writeEpochStakeBuf
  , writeEpochStakeProgressConn
  , writeEpochStakeProgressBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.StakeDelegation
  ( EpochStake
  , EpochStakeProgress
  , PotReward
  , Reward
  )
import DbSync.Db.Statement.StakeDelegationLedger (insertEpochStakeRowStmt)
import DbSync.Db.Statement.StakeDelegationLedger (insertEpochStakeProgressRowStmt)
import DbSync.Db.Statement.StakeDelegationLedger (insertPotRewardRowStmt)
import DbSync.Db.Statement.StakeDelegationLedger (insertRewardRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

writeRewardConn :: Conn.Connection -> Reward -> IO ()
writeRewardConn conn r = runConn conn r insertRewardRowStmt

writeRewardBuf :: WriteBuffer -> Reward -> IO ()
writeRewardBuf buf r = queueBuf buf r insertRewardRowStmt

writePotRewardConn :: Conn.Connection -> PotReward -> IO ()
writePotRewardConn conn pr = runConn conn pr insertPotRewardRowStmt

writePotRewardBuf :: WriteBuffer -> PotReward -> IO ()
writePotRewardBuf buf pr = queueBuf buf pr insertPotRewardRowStmt

writeEpochStakeConn :: Conn.Connection -> EpochStake -> IO ()
writeEpochStakeConn conn es = runConn conn es insertEpochStakeRowStmt

writeEpochStakeBuf :: WriteBuffer -> EpochStake -> IO ()
writeEpochStakeBuf buf es = queueBuf buf es insertEpochStakeRowStmt

writeEpochStakeProgressConn :: Conn.Connection -> EpochStakeProgress -> IO ()
writeEpochStakeProgressConn conn esp = runConn conn esp insertEpochStakeProgressRowStmt

writeEpochStakeProgressBuf :: WriteBuffer -> EpochStakeProgress -> IO ()
writeEpochStakeProgressBuf buf esp = queueBuf buf esp insertEpochStakeProgressRowStmt
