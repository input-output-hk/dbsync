module DbSync.Compare.Report
  ( header
  , putLine
  , green
  , red
  , yellow
  , dim
  , padRight
  ) where

import Cardano.Prelude
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- ---------------------------------------------------------------------------
-- * Coloured output
-- ---------------------------------------------------------------------------

green, red, yellow, cyan, dim :: Text -> Text
green t = "\ESC[32m" <> t <> reset
red t = "\ESC[31m" <> t <> reset
yellow t = "\ESC[33m" <> t <> reset
cyan t = "\ESC[36m" <> t <> reset
dim t = "\ESC[2m" <> t <> reset

reset :: Text
reset = "\ESC[0m"

putLine :: Text -> IO ()
putLine = TIO.putStrLn

header :: Text -> IO ()
header title = TIO.putStrLn ("\n" <> cyan ("== " <> title <> " =="))

padRight :: Int -> Text -> Text
padRight n = T.justifyLeft n ' '
