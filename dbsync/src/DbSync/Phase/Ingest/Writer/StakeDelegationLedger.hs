-- | COPY writers for the four ledger-derived tables owned by the
-- @stake_delegation_ledger@ extractor.
module DbSync.Phase.Ingest.Writer.StakeDelegationLedger
  ( writeRewardCopy
  , writePotRewardCopy
  , writeEpochStakeCopy
  , writeEpochStakeProgressCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.StakeDelegation
  ( EpochStake
  , EpochStakeProgress
  , PotReward
  , Reward
  , encodeEpochStakeCopy
  , encodeEpochStakeProgressCopy
  , encodePotRewardCopy
  , encodeRewardCopy
  , epochStakeProgressTableDef
  , epochStakeTableDef
  , potRewardTableDef
  , rewardTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))

writeRewardCopy :: LoaderStream -> Reward -> IO ()
writeRewardCopy ls r = lsWriteRow ls (tdName rewardTableDef) (encodeRewardCopy r)

writePotRewardCopy :: LoaderStream -> PotReward -> IO ()
writePotRewardCopy ls pr = lsWriteRow ls (tdName potRewardTableDef) (encodePotRewardCopy pr)

writeEpochStakeCopy :: LoaderStream -> EpochStake -> IO ()
writeEpochStakeCopy ls es = lsWriteRow ls (tdName epochStakeTableDef) (encodeEpochStakeCopy es)

writeEpochStakeProgressCopy :: LoaderStream -> EpochStakeProgress -> IO ()
writeEpochStakeProgressCopy ls esp =
  lsWriteRow ls (tdName epochStakeProgressTableDef) (encodeEpochStakeProgressCopy esp)
