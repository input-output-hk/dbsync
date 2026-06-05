-- | Follow 'IdResolver' fragments for the @epoch_boundary@ extractor.
--
-- Only the dedup-style 'CostModel' lookup keeps an explicit resolver
-- field — the rest of the boundary tables allocate ids via PostgreSQL
-- @IDENTITY@ columns so they need no Follow-side assigner.
module DbSync.Phase.Following.Resolver.EpochBoundary
  ( -- * Direct flavour
    resolveCostModelConn

    -- * Buffered flavour
  , resolveCostModelBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.EpochBoundary (CostModel)
import DbSync.Db.Schema.Ids (CostModelId)
import DbSync.Db.Statement.CostModel (nextCostModelIdStmt, queryCostModelIdStmt)
import DbSync.Phase.Following.Resolver.Internal
  ( BlockDedupCache (..)
  , resolveDedupSimple
  , runStmt
  )

resolveCostModelConn :: Conn.Connection -> ByteString -> CostModel -> IO (CostModelId, Bool)
resolveCostModelConn conn hash _cm = do
  mId <- runStmt conn hash queryCostModelIdStmt
  case mId of
    Just cmId -> pure (cmId, False)
    Nothing   -> do
      cmId <- runStmt conn () nextCostModelIdStmt
      pure (cmId, True)

resolveCostModelBuf
  :: Conn.Connection -> BlockDedupCache -> ByteString -> CostModel -> IO (CostModelId, Bool)
resolveCostModelBuf conn cache hash _cm =
  resolveDedupSimple
    conn
    hash
    (bdcCostModel cache)
    queryCostModelIdStmt
    nextCostModelIdStmt
