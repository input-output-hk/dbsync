{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Raise the process's open-file soft limit at startup.
--
-- Defaults on common platforms leave the soft limit far below what
-- the ingest LSM session needs once merges accumulate (macOS
-- launchd-spawned processes inherit 256; many Linux distributions
-- ship 1024). Hitting the cap manifests as @FsTooManyOpenFiles@
-- mid-sync, often during background merge work.
--
-- 'raiseFdLimit' lifts the soft limit to the hard limit (capped at
-- 'fdSoftLimitTarget') so the running process gets all the headroom
-- the OS will grant it, and reports the outcome through the
-- application tracer.
module DbSync.Phase.Ingest.FdLimit
  ( raiseFdLimit
  , fdSoftLimitTarget
  ) where

import Cardano.Prelude

import Control.Tracer (traceWith)
import qualified System.Posix.Resource as Posix

import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- | Upper bound on what we attempt to set the soft limit to. The OS
-- hard limit caps this further if it is lower.
--
-- 1,048,576 matches Linux's typical @/proc/sys/fs/nr_open@ ceiling
-- and is well above macOS's @kern.maxfilesperproc@ default
-- (245,760). Any lower hard cap is respected automatically.
fdSoftLimitTarget :: Integer
fdSoftLimitTarget = 1_048_576

-- | Read @RLIMIT_NOFILE@ and raise the soft limit to
-- @min(hard, 'fdSoftLimitTarget')@. Logged at 'Info' on success,
-- 'Warning' if the OS rejects the request.
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
        ) Nothing
  where
    traceInfo msg = traceWith tracer $ LogMsg Info "FdLimit" msg Nothing

-- | Honour the hard limit. 'ResourceLimitInfinity' has no integer
-- value, so we fall back to 'fdSoftLimitTarget' in that case.
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
