-- | COPY writers for tables owned by the @epoch_boundary@ extractor.
--
-- The three pot-rebalancing tables that share a wire shape with the
-- epoch-boundary tables (@pot_transfer@, @treasury@, @reserve@) live
-- in "DbSync.Phase.Ingest.Writer.StakeDelegation" because the
-- stake-delegation extractor is the one that emits them.
module DbSync.Phase.Ingest.Writer.EpochBoundary
  ( writeAdaPotsCopy
  , writeEpochParamCopy
  , writeEpochStateCopy
  , writeCostModelCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.AdaPots (AdaPots, adaPotsTableDef, encodeAdaPotsCopy)
import DbSync.Db.Schema.EpochBoundary
  ( CostModel
  , EpochParam
  , EpochState
  , costModelTableDef
  , encodeCostModelCopy
  , encodeEpochParamCopy
  , encodeEpochStateCopy
  , epochParamTableDef
  , epochStateTableDef
  )
import DbSync.Db.Schema.Ids (CostModelId)
import DbSync.Db.Schema.Types (TableDef (..))

writeAdaPotsCopy :: LoaderStream -> AdaPots -> IO ()
writeAdaPotsCopy ls pots = lsWriteRow ls (tdName adaPotsTableDef) (encodeAdaPotsCopy pots)

writeEpochParamCopy :: LoaderStream -> EpochParam -> IO ()
writeEpochParamCopy ls ep = lsWriteRow ls (tdName epochParamTableDef) (encodeEpochParamCopy ep)

writeEpochStateCopy :: LoaderStream -> EpochState -> IO ()
writeEpochStateCopy ls es = lsWriteRow ls (tdName epochStateTableDef) (encodeEpochStateCopy es)

writeCostModelCopy :: LoaderStream -> CostModelId -> CostModel -> IO ()
writeCostModelCopy ls cmid cm = lsWriteRow ls (tdName costModelTableDef) (encodeCostModelCopy cmid cm)
