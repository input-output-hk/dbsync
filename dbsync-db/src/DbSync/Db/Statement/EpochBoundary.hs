-- | Hasql 'Statement' bindings for the @epoch_boundary@ extractor
-- tables: @ada_pots@, @epoch_param@, @epoch_state@, @cost_model@.
--
-- @cost_model@ is the only dedup-keyed table here (keyed on its
-- Blake2b CBOR @hash@); the other three upsert on @epoch_no@ so a
-- rollback that re-crosses the boundary refreshes the epoch's row.
module DbSync.Db.Statement.EpochBoundary
  ( -- * ada_pots
    insertAdaPotsRowStmt

    -- * epoch_param
  , insertEpochParamRowStmt

    -- * epoch_state
  , insertEpochStateRowStmt

    -- * cost_model
  , insertCostModelRowStmt
  , nextCostModelIdStmt
  , queryCostModelIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.AdaPots (AdaPots, adaPotsEncoder, adaPotsTableDef)
import DbSync.Db.Schema.EpochBoundary
  ( CostModel
  , EpochParam
  , EpochState
  , costModelEncoder
  , costModelTableDef
  , epochParamEncoder
  , epochParamTableDef
  , epochStateEncoder
  , epochStateTableDef
  )
import DbSync.Db.Schema.Ids (CostModelId (..), idEncoder)
import DbSync.Db.Statement.Common
  ( LookupColumn (..)
  , insertRowSql
  , nextIdStmt
  , queryIdByColumnStmt
  , upsertRowSql
  )

-- ---------------------------------------------------------------------------
-- * ada_pots
-- ---------------------------------------------------------------------------

insertAdaPotsRowStmt :: Stmt.Statement AdaPots ()
insertAdaPotsRowStmt =
  Stmt.preparable (upsertRowSql adaPotsTableDef) adaPotsEncoder D.noResult

-- ---------------------------------------------------------------------------
-- * epoch_param
-- ---------------------------------------------------------------------------

insertEpochParamRowStmt :: Stmt.Statement EpochParam ()
insertEpochParamRowStmt =
  Stmt.preparable (upsertRowSql epochParamTableDef) epochParamEncoder D.noResult

-- ---------------------------------------------------------------------------
-- * epoch_state
-- ---------------------------------------------------------------------------

insertEpochStateRowStmt :: Stmt.Statement EpochState ()
insertEpochStateRowStmt =
  Stmt.preparable (upsertRowSql epochStateTableDef) epochStateEncoder D.noResult

-- ---------------------------------------------------------------------------
-- * cost_model
-- ---------------------------------------------------------------------------

insertCostModelRowStmt :: Stmt.Statement (CostModelId, CostModel) ()
insertCostModelRowStmt =
  Stmt.preparable (insertRowSql costModelTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getCostModelId)
           <> (snd >$< costModelEncoder)

nextCostModelIdStmt :: Stmt.Statement () CostModelId
nextCostModelIdStmt = nextIdStmt costModelTableDef CostModelId

queryCostModelIdStmt :: Stmt.Statement ByteString (Maybe CostModelId)
queryCostModelIdStmt = queryIdByColumnStmt costModelTableDef ByHash CostModelId
