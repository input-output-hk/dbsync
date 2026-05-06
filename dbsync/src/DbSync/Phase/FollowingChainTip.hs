{-# LANGUAGE FlexibleContexts #-}

-- | FollowingChainTip phase: per-block INSERT against PG.
module DbSync.Phase.FollowingChainTip
  ( run
  , processBlocks
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.AppM (FollowM)
import DbSync.Block.Types (GenericBlock)
import DbSync.Extractor (ExtractorDef, HasExtractors (..))
import DbSync.Ingest.Pipeline (processBlock)
import DbSync.Resolver (HasResolver (..), IdResolver)
import DbSync.Resolver.Follow (mkFollowResolver)
import DbSync.Writer (HasWriter (..), Writer)
import DbSync.Writer.InsertAdapter (mkInsertWriter)

-- | Production entry point. Not yet wired into Main.
run :: FollowM ()
run = panic "TODO: not implemented"

-- | Drive a fixed list of blocks through the FollowingChainTip
-- pipeline against the given connection. Used by tests; production
-- 'run' will pull from the receiver queue instead.
processBlocks :: Conn.Connection -> [ExtractorDef] -> [GenericBlock] -> IO ()
processBlocks conn extractors blocks = do
  resolver <- mkFollowResolver conn
  let writer = mkInsertWriter conn
      env    = Env { envResolver = resolver, envWriter = writer, envExtractors = extractors }
  for_ blocks $ \blk -> runReaderT (processBlock blk) env

-- | Minimal env satisfying 'processBlock''s constraints.
data Env = Env
  { envResolver   :: !(IdResolver IO)
  , envWriter     :: !(Writer IO)
  , envExtractors :: ![ExtractorDef]
  }

instance HasResolver Env where getResolver = envResolver
instance HasWriter Env where getWriter = envWriter
instance HasExtractors Env where getExtractors = envExtractors
