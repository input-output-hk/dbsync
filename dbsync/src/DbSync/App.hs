-- | Application entry point.
--
-- Orchestrates the full db-sync lifecycle: config parsing, environment
-- setup, phase transitions (Ingest → Preparing → Following), and
-- graceful shutdown.
module DbSync.App
  ( run
  ) where

import Cardano.Prelude

-- | Run db-sync with the given core environment.
--
-- This is the top-level entry point called from @main@.
-- TODO: Accept CoreEnv once DbSync.Env is defined.
run :: IO ()
run = panic "TODO: not implemented"
