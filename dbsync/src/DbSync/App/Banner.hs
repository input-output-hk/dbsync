-- | Startup banner for the @dbsync@ executable.
module DbSync.App.Banner
  ( printBanner
  ) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO (hFlush)

-- | Writes to stderr, the stream the log tracer also uses.
--
-- Encodes to UTF-8 here instead of through the handle's own encoder,
-- so the block characters print under a non-UTF-8 locale rather than
-- throwing.
printBanner :: IO ()
printBanner = do
  BS.hPutStr stderr (TE.encodeUtf8 banner)
  hFlush stderr

banner :: Text
banner = T.unlines
  [ "/██████████///███████████///█████████//█████/█████/██████///█████///█████████/"
  , "░░███░░░░███/░░███░░░░░███/███░░░░░███░░███/░░███/░░██████/░░███///███░░░░░███"
  , "/░███///░░███/░███////░███░███////░░░//░░███/███///░███░███/░███//███/////░░░/"
  , "/░███////░███/░██████████/░░█████████///░░█████////░███░░███░███/░███/////////"
  , "/░███////░███/░███░░░░░███/░░░░░░░░███///░░███/////░███/░░██████/░███/////////"
  , "/░███////███//░███////░███/███////░███////░███/////░███//░░█████/░░███/////███"
  , "/██████████///███████████/░░█████████/////█████////█████//░░█████/░░█████████/"
  , "░░░░░░░░░░///░░░░░░░░░░░///░░░░░░░░░/////░░░░░////░░░░░////░░░░░///░░░░░░░░░//"
  , ""
  ]
