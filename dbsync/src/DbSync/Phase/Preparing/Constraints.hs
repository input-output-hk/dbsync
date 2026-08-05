{-# LANGUAGE OverloadedStrings #-}

-- | Create the ownership-edge foreign keys once the bulk load is done.
-- Holding them during Ingest is not an option: the loader writes children
-- before the resolve pass fills their parent pointers.
module DbSync.Phase.Preparing.Constraints
  ( addConstraints
  , validateConstraints
  ) where

import Cardano.Prelude

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.List (sortOn)
import qualified Hasql.Session as Sess

import DbSync.Db.Pool (PoolM, forPooled_, usePool)
import DbSync.Db.Run (useConn)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.Constraints
  ( ConstraintStatement (..)
  , parentRefConstraints
  )
import DbSync.Db.Transaction (HasHasqlConnection (..))
import DbSync.Phase.Preparing.Indexes (tableSizeRank)
import DbSync.Phase.Preparing.Step (StepKind (..), step)
import DbSync.Trace (HasTracer (..))

-- | Serial, because @ADD CONSTRAINT@ takes @SHARE ROW EXCLUSIVE@ on the
-- parent and most edges share @tx@.
addConstraints
  :: (HasTracer env, HasHasqlConnection env, MonadReader env m, MonadUnliftIO m)
  => [TableDef] -> m ()
addConstraints tables =
  for_ (parentRefConstraints tables) $ \c ->
    step ConstraintStep (csName c) $ do
      conn <- asks getHasqlConnection
      useConn "Phase.Preparing.Constraints" conn (Sess.script (csAddSql c))

-- | Poolable, unlike 'addConstraints': @VALIDATE CONSTRAINT@ takes only
-- @ROW SHARE@ on the parent. Biggest child first so the long scans
-- overlap the rest.
validateConstraints :: Int -> [TableDef] -> PoolM ()
validateConstraints poolSize tables =
  forPooled_ poolSize prioritised $ \c ->
    step ConstraintStep ("validate " <> csName c) $
      usePool ("validate " <> csTable c) (Sess.script (csValidateSql c))
  where
    prioritised = sortOn (tableSizeRank . csTable) (parentRefConstraints tables)
