-- | Re-exports "DbSync.Trace.Types" and defines the 'HasTracer'
-- accessor class. Call sites use 'traceWith' on the 'AppTracer'
-- with a constructed 'LogMsg'.
module DbSync.Trace
  ( -- * Re-exports
    module DbSync.Trace.Types

    -- * Accessor class
  , HasTracer (..)
  ) where

import DbSync.Trace.Types

class HasTracer env where
  getTracer :: env -> AppTracer

-- | Self-instance so boot and test code can run
-- 'HasTracer'-polymorphic helpers via @runAppM tracer ...@ without a
-- phase env.
instance HasTracer AppTracer where
  getTracer t = t
