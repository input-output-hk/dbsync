-- | Shared 'processBlock' environment for unit and property tests.
--
-- 'processBlock' is polymorphic over an env that supplies a resolver,
-- writer, extractor list, network, ledger-data fetch, and sync
-- phase. Specs across the suite all need the same plumbing — this
-- module is the one place that wiring lives.
module DbSync.Test.PipelineEnv
  ( -- * Test env
    TestPipelineEnv (..)
  , mkTestPipelineEnv
  , mkTestPipelineEnvOn
  , mkTestPipelineEnvWith
  ) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (Network (..))

import DbSync.Block.Types (GenericBlock)
import DbSync.Env (HasNetwork (..))
import DbSync.Extractor
  ( BlockLedgerData
  , ExtractorDef
  , HasExtractors (..)
  , HasLedgerData (..)
  , emptyBlockLedgerData
  )
import DbSync.Db.Phase (SyncPhase (..))
import DbSync.Phase.Ref (HasSyncPhase (..))
import DbSync.Resolver (HasResolver (..), IdResolver)
import DbSync.Writer (HasWriter (..), Writer)

-- | The env shape every spec passes to 'processBlock'.
--
-- 'tpeLedgerData' lets a test inject worker output for blocks that
-- exercise ledger-ON paths; defaults to 'emptyBlockLedgerData'.
-- 'tpeSyncPhase' chooses between Ingest (post-load fallback) and
-- Follow (inline value resolution); defaults to 'IngestChainHistory'.
data TestPipelineEnv = TestPipelineEnv
  { tpeResolver   :: !(IdResolver IO)
  , tpeWriter     :: !(Writer IO)
  , tpeExtractors :: ![ExtractorDef]
  , tpeNetwork    :: !Network
  , tpeLedgerData :: !(GenericBlock -> IO BlockLedgerData)
  , tpeSyncPhase  :: !SyncPhase
  }

instance HasResolver TestPipelineEnv where
  getResolver = tpeResolver

instance HasWriter TestPipelineEnv where
  getWriter = tpeWriter

instance HasExtractors TestPipelineEnv where
  getExtractors = tpeExtractors

instance HasNetwork TestPipelineEnv where
  getNetwork = tpeNetwork

instance HasLedgerData TestPipelineEnv where
  getLedgerData env = tpeLedgerData env

instance HasSyncPhase TestPipelineEnv where
  getSyncPhase = pure . tpeSyncPhase

-- | Build an env on mainnet with empty ledger data and Ingest phase.
mkTestPipelineEnv
  :: IdResolver IO -> Writer IO -> [ExtractorDef] -> TestPipelineEnv
mkTestPipelineEnv = mkTestPipelineEnvOn Mainnet

-- | Build an env with an explicit network; ledger data empty,
-- Ingest phase.
mkTestPipelineEnvOn
  :: Network -> IdResolver IO -> Writer IO -> [ExtractorDef] -> TestPipelineEnv
mkTestPipelineEnvOn n r w exs =
  mkTestPipelineEnvWith n r w exs (\_ -> pure emptyBlockLedgerData) IngestChainHistory

-- | Full constructor: caller supplies the ledger-data fetcher and
-- sync phase. Used by specs that exercise ledger-ON dispatch or the
-- Follow path.
mkTestPipelineEnvWith
  :: Network
  -> IdResolver IO
  -> Writer IO
  -> [ExtractorDef]
  -> (GenericBlock -> IO BlockLedgerData)
  -> SyncPhase
  -> TestPipelineEnv
mkTestPipelineEnvWith n r w exs ld phase = TestPipelineEnv
  { tpeResolver   = r
  , tpeWriter     = w
  , tpeExtractors = exs
  , tpeNetwork    = n
  , tpeLedgerData = ld
  , tpeSyncPhase  = phase
  }
