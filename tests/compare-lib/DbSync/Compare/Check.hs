module DbSync.Compare.Check
  ( Check (..)
  , CheckOutcome (..)
  , CheckItem (..)
  , runChecks
  ) where

import Cardano.Prelude
import qualified Data.Text as T
import DbSync.Compare.Connect (DbConn)
import DbSync.Compare.Report (dim, green, header, putLine, red, yellow)

-- ---------------------------------------------------------------------------
-- * Check definition
-- ---------------------------------------------------------------------------

-- A single comparison unit. 'chkRun' is handed both connections and must run
-- its own (ideally single) query against each, returning one outcome.
data Check = Check
  { chkLabel :: !Text
  , chkRun :: DbConn -> DbConn -> IO CheckOutcome
  }

data CheckOutcome
  = Ok Text
  | Diff [Text]
  | Info Text
  | Skipped Text

-- A run is a flat stream of section headers interleaved with checks, so the
-- output can group checks (e.g. by era) without the runner knowing the
-- grouping.
data CheckItem
  = Section !Text
  | Item !Check

-- ---------------------------------------------------------------------------
-- * Runner
-- ---------------------------------------------------------------------------

-- Run every item sequentially, printing its outcome the moment it completes.
-- Each check is isolated with 'try' so a thrown query error is reported as a
-- failing line rather than aborting the whole run. Returns 'True' when any
-- check reported a 'Diff' or threw.
runChecks :: DbConn -> DbConn -> [CheckItem] -> IO Bool
runChecks oldDb newDb = foldM step False
  where
    step anyBad (Section title) = do
      header title
      pure anyBad
    step anyBad (Item chk) = do
      result <- try (chkRun chk oldDb newDb)
      bad <- report (chkLabel chk) result
      pure (anyBad || bad)

-- Print one line for the outcome and report whether it counts as a failure.
report :: Text -> Either SomeException CheckOutcome -> IO Bool
report label outcome = do
  putLine (mark <> " " <> padLabel label <> detailSuffix detail)
  pure bad
  where
    (bad, mark, detail) = case outcome of
      Right (Ok d) -> (False, green "ok  ", d)
      Right (Diff ds) -> (True, red "diff", T.intercalate "; " ds)
      Right (Info d) -> (False, dim "--  ", d)
      Right (Skipped why) -> (False, yellow "skip", why)
      Left err -> (True, red "err ", T.pack (displayException err))

    detailSuffix d
      | T.null d = ""
      | otherwise = "  " <> dim d

padLabel :: Text -> Text
padLabel = T.justifyLeft 48 ' '
