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

withDb :: Settings.Settings -> DbRole -> Text -> (DbConn -> IO a) -> IO a
withDb settings role name action =
  bracket acquire Conn.release (action . DbConn role name)
  where
    acquire =
      Conn.acquire settings >>= \case
        Left err -> throwIO (CompareError ("Could not connect to " <> name <> ": " <> show err))
        Right conn -> pure conn

-- ---------------------------------------------------------------------------
-- * Query helpers
-- ---------------------------------------------------------------------------

runSession :: DbConn -> Sess.Session a -> IO a
runSession conn session =
  Conn.use (dbcConn conn) session >>= \case
    Left err -> throwIO (CompareError ("Query failed on " <> dbcName conn <> ": " <> show err))
    Right a -> pure a

stmtText :: Text -> D.Result a -> Stmt.Statement () a
stmtText sql = Stmt.unpreparable sql E.noParams

queryScalarInt :: DbConn -> Text -> IO Int64
queryScalarInt conn sql =
  runSession conn (Sess.statement () (stmtText sql (D.singleRow (D.column (D.nonNullable D.int8)))))

queryMaybeInt :: DbConn -> Text -> IO (Maybe Int64)
queryMaybeInt conn sql =
  runSession conn (Sess.statement () (stmtText sql (D.singleRow (D.column (D.nullable D.int8)))))

queryScalarText :: DbConn -> Text -> IO Text
queryScalarText conn sql =
  runSession conn (Sess.statement () (stmtText sql (D.singleRow (D.column (D.nonNullable D.text)))))

queryTextList :: DbConn -> Text -> IO [Text]
queryTextList conn sql =
  runSession conn (Sess.statement () (stmtText sql (D.rowList (D.column (D.nonNullable D.text)))))

queryBool :: DbConn -> Text -> IO Bool
queryBool conn sql =
  runSession conn (Sess.statement () (stmtText sql (D.singleRow (D.column (D.nonNullable D.bool)))))

-- Run SQL whose SELECT projects @ncols@ columns, each read as nullable text.
-- Callers cast every column @::text@ so one decoder serves any row shape.
queryRows :: DbConn -> Int -> Text -> IO [[Maybe Text]]
queryRows conn ncols sql =
  runSession conn (Sess.statement () (stmtText sql decoder))
  where
    decoder = D.rowList (replicateM ncols (D.column (D.nullable D.text)))