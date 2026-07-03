module DbSync.Compare.Connect
  ( DbRole (..)
  , DbConn (..)
  , roleLabel
  , mkSettings
  , withDb
  , queryScalarInt
  , queryMaybeInt
  , queryScalarText
  , queryTextList
  , queryBool
  , queryRows
  ) where

import Cardano.Prelude
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Hasql.Connection as Conn
import qualified Hasql.Connection.Settings as Settings
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

-- ---------------------------------------------------------------------------
-- * Connections
-- ---------------------------------------------------------------------------

data DbRole = OldDb | NewDb
  deriving stock (Eq, Show)

roleLabel :: DbRole -> Text
roleLabel = \case
  OldDb -> "old"
  NewDb -> "new"

data DbConn = DbConn
  { dbcRole :: !DbRole
  , dbcName :: !Text
  , dbcConn :: !Conn.Connection
  , dbcVerbose :: !Bool -- ^ Echo each SQL statement to stderr before running it.
  }

newtype CompareError = CompareError Text
  deriving stock (Show)

instance Exception CompareError

mkSettings :: Text -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> Settings.Settings
mkSettings name mHost mPort mUser mPass =
  mconcat $
    [Settings.dbname name]
      <> hostPort
      <> foldMap (pure . Settings.user) mUser
      <> foldMap (pure . Settings.password) mPass
  where
    hostPort
      | isJust mHost || isJust mPort =
          [Settings.hostAndPort (fromMaybe "localhost" mHost) (maybe 5432 fromIntegral mPort)]
      | otherwise = []

-- Acquire a connection, apply a per-statement timeout so a pathological query
-- fails loudly instead of hanging, and run the action.
withDb :: Settings.Settings -> DbRole -> Text -> Bool -> Int -> (DbConn -> IO a) -> IO a
withDb settings role name verbose timeoutSeconds action =
  bracket acquire Conn.release $ \rawConn -> do
    let conn = DbConn role name rawConn verbose
    setStatementTimeout conn timeoutSeconds
    action conn
  where
    acquire =
      Conn.acquire settings >>= \case
        Left err -> throwIO (CompareError ("Could not connect to " <> name <> ": " <> show err))
        Right conn -> pure conn

setStatementTimeout :: DbConn -> Int -> IO ()
setStatementTimeout conn seconds =
  void $ run conn ("SET statement_timeout = " <> T.pack (show (seconds * 1000))) (D.noResult)

-- ---------------------------------------------------------------------------
-- * Query helpers
-- ---------------------------------------------------------------------------

-- Echo (when verbose) and execute one statement, surfacing the SQL on failure.
run :: DbConn -> Text -> D.Result a -> IO a
run conn sql decoder = do
  when (dbcVerbose conn) $
    TIO.hPutStrLn stderr (roleLabel (dbcRole conn) <> "> " <> T.unwords (T.words sql))
  Conn.use (dbcConn conn) (Sess.statement () (Stmt.unpreparable sql E.noParams decoder)) >>= \case
    Left err -> throwIO (CompareError ("Query failed on " <> dbcName conn <> ":\n  " <> sql <> "\n  " <> show err))
    Right a -> pure a

queryScalarInt :: DbConn -> Text -> IO Int64
queryScalarInt conn sql =
  run conn sql (D.singleRow (D.column (D.nonNullable D.int8)))

queryMaybeInt :: DbConn -> Text -> IO (Maybe Int64)
queryMaybeInt conn sql =
  run conn sql (D.singleRow (D.column (D.nullable D.int8)))

queryScalarText :: DbConn -> Text -> IO Text
queryScalarText conn sql =
  run conn sql (D.singleRow (D.column (D.nonNullable D.text)))

queryTextList :: DbConn -> Text -> IO [Text]
queryTextList conn sql =
  run conn sql (D.rowList (D.column (D.nonNullable D.text)))

queryBool :: DbConn -> Text -> IO Bool
queryBool conn sql =
  run conn sql (D.singleRow (D.column (D.nonNullable D.bool)))

-- Run SQL whose SELECT projects @ncols@ columns, each read as nullable text.
-- Callers cast every column @::text@ so one decoder serves any row shape.
queryRows :: DbConn -> Int -> Text -> IO [[Maybe Text]]
queryRows conn ncols sql =
  run conn sql (D.rowList (replicateM ncols (D.column (D.nullable D.text))))