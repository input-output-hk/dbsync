-- | Shared 'processBlock' environment for unit and property tests.
--
-- 'processBlock' is polymorphic over an env that supplies a resolver,
-- writer, extractor list, and network. Specs across the suite all
-- need the same plumbing — this module is the one place that wiring
-- lives.
module DbSync.Test.PipelineEnv
  ( -- * Test env
    TestPipelineEnv (..)
  , mkTestPipelineEnv
  , mkTestPipelineEnvOn
  ) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (Network (..))

import DbSync.Env (HasNetwork (..))
import DbSync.Extractor (ExtractorDef, HasExtractors (..))
import DbSync.Resolver (HasResolver (..), IdResolver)
import DbSync.Writer (HasWriter (..), Writer)

-- | The minimal env shape every spec passes to 'processBlock'.
data TestPipelineEnv = TestPipelineEnv
  { tpeResolver   :: !(IdResolver IO)
  , tpeWriter     :: !(Writer IO)
  , tpeExtractors :: ![ExtractorDef]
  , tpeNetwork    :: !Network
  }

instance HasResolver TestPipelineEnv where
  getResolver = tpeResolver

instance HasWriter TestPipelineEnv where
  getWriter = tpeWriter

instance HasExtractors TestPipelineEnv where
  getExtractors = tpeExtractors

instance HasNetwork TestPipelineEnv where
  getNetwork = tpeNetwork

-- | Build an env on the mainnet network.  Most unit tests don't care
-- about the network bit; those that do should use
-- 'mkTestPipelineEnvOn'.
mkTestPipelineEnv
  :: IdResolver IO -> Writer IO -> [ExtractorDef] -> TestPipelineEnv
mkTestPipelineEnv = mkTestPipelineEnvOn Mainnet

-- | Build an env with an explicit network.
mkTestPipelineEnvOn
  :: Network -> IdResolver IO -> Writer IO -> [ExtractorDef] -> TestPipelineEnv
mkTestPipelineEnvOn n r w exs = TestPipelineEnv
  { tpeResolver   = r
  , tpeWriter     = w
  , tpeExtractors = exs
  , tpeNetwork    = n
  }
