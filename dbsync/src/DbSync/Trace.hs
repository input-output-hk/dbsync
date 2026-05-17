-- | Re-exports the structured-logging types and defines the
-- 'HasTracer' accessor class.
--
-- The actual logging call sites use 'traceWith' on the 'AppTracer'
-- directly with a constructed 'LogMsg' — see "DbSync.Trace.Types".
module DbSync.Trace
  ( -- * Re-exports
    module DbSync.Trace.Types

    -- * Accessor class
  , HasTracer (..)
  ) where

import DbSync.Trace.Types

-- | Access the tracer from any environment. Implemented per-env.
class HasTracer env where
  getTracer :: env -> AppTracer

-- | Self-instance so boot-time / test code can drive
-- 'HasTracer'-polymorphic helpers via @runAppM tracer ...@ without
-- building a phase env.
instance HasTracer AppTracer where
  getTracer t = t
