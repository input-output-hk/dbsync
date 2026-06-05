-- | Hasql 'Statement' bindings for the @cost_model@ dedup table.
--
-- 'FollowingChainTip' resolves a cost model by its @hash@ via
-- 'queryCostModelIdStmt'. On a miss it allocates a new id from
-- 'nextCostModelIdStmt' and the writer inserts via
-- 'insertCostModelRowStmt'.
module DbSync.Db.Statement.CostModel
  ( -- * Inserts
    insertCostModelRowStmt

    -- * ID allocation
  , nextCostModelIdStmt

    -- * Lookups
  , queryCostModelIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.EpochBoundary
  ( CostModel
  , costModelEncoder
  , costModelTableDef
  )
import DbSync.Db.Schema.Ids (CostModelId (..), idEncoder)
import DbSync.Db.Statement.Common
  ( LookupColumn (..)
  , insertRowSql
  , nextIdStmt
  , queryIdByColumnStmt
  )

insertCostModelRowStmt :: Stmt.Statement (CostModelId, CostModel) ()
insertCostModelRowStmt =
  Stmt.preparable (insertRowSql costModelTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getCostModelId)
           <> (snd >$< costModelEncoder)

nextCostModelIdStmt :: Stmt.Statement () CostModelId
nextCostModelIdStmt = nextIdStmt costModelTableDef CostModelId

-- | Look up an existing 'CostModelId' by Blake2b CBOR hash.
queryCostModelIdStmt :: Stmt.Statement ByteString (Maybe CostModelId)
queryCostModelIdStmt = queryIdByColumnStmt costModelTableDef ByHash CostModelId
