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
import DbSync.Db.Schema.Ids (AdaPotsId, CostModelId, EpochParamId, EpochStateId)
import DbSync.Db.Schema.Types (TableDef (..))

writeAdaPotsCopy :: LoaderStream -> AdaPotsId -> AdaPots -> IO ()
writeAdaPotsCopy ls apid pots = lsWriteRow ls (tdName adaPotsTableDef) (encodeAdaPotsCopy apid pots)

writeEpochParamCopy :: LoaderStream -> EpochParamId -> EpochParam -> IO ()
writeEpochParamCopy ls epid ep = lsWriteRow ls (tdName epochParamTableDef) (encodeEpochParamCopy epid ep)

writeEpochStateCopy :: LoaderStream -> EpochStateId -> EpochState -> IO ()
writeEpochStateCopy ls esid es = lsWriteRow ls (tdName epochStateTableDef) (encodeEpochStateCopy esid es)

writeCostModelCopy :: LoaderStream -> CostModelId -> CostModel -> IO ()
writeCostModelCopy ls cmid cm = lsWriteRow ls (tdName costModelTableDef) (encodeCostModelCopy cmid cm)
