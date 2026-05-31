-- | Top-level umbrella re-export for the @App@ sub-namespace.
--
-- Convenience so callers can write @import DbSync.App (runApp, AppArgs (..))@
-- without spelling out the submodule path. Implementation lives in:
--
-- * "DbSync.App.Args"   — AppArgs record
-- * "DbSync.App.Boot"   — boot decision
-- * "DbSync.App.Cli"    — CLI parser
-- * "DbSync.App.Config" — YAML profile parsing
-- * "DbSync.App.Env"    — environment records
-- * "DbSync.App.Run"    — runApp orchestration
-- * "DbSync.App.Setup"  — buildCoreEnv / buildExtractors / runStartup
module DbSync.App
  ( module DbSync.App.Args
  , module DbSync.App.Run
  , module DbSync.App.Setup
  ) where

import DbSync.App.Args
import DbSync.App.Run
import DbSync.App.Setup
