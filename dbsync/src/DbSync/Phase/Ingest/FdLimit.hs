{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Raise the process's open-file soft limit at startup. Platform
-- defaults sit far below what the ingest LSM session needs once
-- merges accumulate: macOS inherits 256, and many Linux
-- distributions ship 1024. The cap surfaces as
-- @FsTooManyOpenFiles@ mid-sync.
module DbSync.Phase.Ingest.FdLimit
  ( raiseFdLimit
  , fdSoftLimitTarget
  ) where

import Cardano.Prelude

import Control.Tracer (traceWith)
import qualified System.Posix.Resource as Posix

import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- | Upper bound on the soft limit this module requests. A lower OS
-- hard limit caps it further. 1,048,576 matches Linux's typical
-- @/proc/sys/fs/nr_open@ ceiling and sits above macOS's
-- @kern.maxfilesperproc@ default of 245,760.
fdSoftLimitTarget :: Integer
fdSoftLimitTarget = 1_048_576

-- | Read @RLIMIT_NOFILE@ and raise the soft limit to
-- @min(hard, 'fdSoftLimitTarget')@. It logs at 'Info' on success and
-- at 'Warning' when the OS rejects the request.
raiseFdLimit :: AppTracer -> IO ()
raiseFdLimit tracer = do
  result <- try @SomeException $ do
    rl <- Posix.getResourceLimit Posix.ResourceOpenFiles
    let !target = pickTarget (Posix.hardLimit rl)
    when (Posix.softLimit rl /= target) $
      Posix.setResourceLimit Posix.ResourceOpenFiles
        rl { Posix.softLimit = target }
    final <- Posix.getResourceLimit Posix.ResourceOpenFiles
    pure (Posix.softLimit rl, Posix.softLimit final)
  case result of
    Right (before, after)
      | before == after ->
          traceInfo $
            "FD soft limit already at " <> renderLimit after <> "; not raised"
      | otherwise ->
          traceInfo $
            "Raised FD soft limit from " <> renderLimit before
              <> " to " <> renderLimit after
    Left e ->
      traceWith tracer $ LogMsg Warning "FdLimit"
        ( "Could not raise open-file soft limit ("
            <> show e
            <> "); the ingest LSM session may exhaust file descriptors "
            <> "under sustained load. Operators on locked-down environments "
            <> "(containers, systemd units) should raise RLIMIT_NOFILE "
            <> "(--ulimit nofile=... / LimitNOFILE=...)."
        )
  where
    traceInfo msg = traceWith tracer $ LogMsg Info "FdLimit" msg

-- | Honour the hard limit. 'ResourceLimitInfinity' carries no
-- integer value, so that case falls back to 'fdSoftLimitTarget'.
pickTarget :: Posix.ResourceLimit -> Posix.ResourceLimit
pickTarget = \case
  Posix.ResourceLimitInfinity -> Posix.ResourceLimit fdSoftLimitTarget
  Posix.ResourceLimitUnknown  -> Posix.ResourceLimit fdSoftLimitTarget
  Posix.ResourceLimit n       -> Posix.ResourceLimit (min n fdSoftLimitTarget)

renderLimit :: Posix.ResourceLimit -> Text
renderLimit = \case
  Posix.ResourceLimitInfinity -> "unlimited"
  Posix.ResourceLimitUnknown  -> "unknown"
  Posix.ResourceLimit n       -> show n
