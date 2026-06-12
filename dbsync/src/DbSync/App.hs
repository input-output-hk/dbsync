-- | Re-exports the public surface of the @App@ sub-namespace
-- (arguments, runner, setup) so callers can @import DbSync.App@.
module DbSync.App
  ( module DbSync.App.Args
  , module DbSync.App.Run
  , module DbSync.App.Setup
  ) where

import DbSync.App.Args
import DbSync.App.Run
import DbSync.App.Setup
