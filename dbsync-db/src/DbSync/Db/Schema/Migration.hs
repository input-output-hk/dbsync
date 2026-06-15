-- | Schema-version gate and migration runner.
--
-- The database records the schema version it was built at in
-- @dbsync_sync_state.schema_version_applied@. 'decideMigrations' compares
-- that to the version the binary declares and classifies the gap;
-- 'runMigrations' applies the embedded migration files for the intervening
-- versions and re-stamps the row in a single transaction.
module DbSync.Db.Schema.Migration
  ( MigrationOutcome (..)
  , decideMigrations
  , migrationFiles
  , selectMigrationSql
  , stampSql
  , runMigrations
  ) where

import Cardano.Prelude

import Data.List (lookup, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO.Error (userError)

import qualified Hasql.Connection as Conn
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Migration.Files (embeddedMigrationFiles)
import DbSync.Db.Sql (quoteLiteral)

-- ---------------------------------------------------------------------------
-- * Decision
-- ---------------------------------------------------------------------------

data MigrationOutcome
  = NoMigrationNeeded
  | MigrationsToApply (NonEmpty Int)
  | DbNewerThanBinary
      Int -- ^ schema version the database was built at
      Int -- ^ schema version this binary declares
  | SchemaDriftUncovered
      Text -- ^ fingerprint stored in the database
      Text -- ^ fingerprint this binary declares
  deriving stock (Eq, Show)

-- | Classify the gap between the database's applied schema version and the
-- version the binary targets. Equal versions still fail when the stored
-- fingerprint diverges from the declared one — schema drift that no
-- migration covers.
decideMigrations
  :: Int  -- ^ version applied in the database
  -> Int  -- ^ version this binary targets
  -> Text -- ^ fingerprint stored in the database
  -> Text -- ^ fingerprint this binary declares
  -> MigrationOutcome
decideMigrations applied target storedFp declaredFp
  | applied > target = DbNewerThanBinary applied target
  | applied < target = MigrationsToApply (NE.fromList [applied + 1 .. target])
  | storedFp == declaredFp = NoMigrationNeeded
  | otherwise = SchemaDriftUncovered storedFp declaredFp

-- ---------------------------------------------------------------------------
-- * Embedded migration files
-- ---------------------------------------------------------------------------

-- | Migration files compiled into the binary, keyed by schema version and
-- sorted ascending. The frozen baseline is excluded — it seeds tests and
-- records v1, it is never replayed against a live database.
migrationFiles :: [(Int, Text)]
migrationFiles =
  sortOn fst
    [ (version, TE.decodeUtf8 contents)
    | (path, contents) <- embeddedMigrationFiles
    , not ("baseline" `T.isInfixOf` T.pack path)
    , Just version <- [parseVersion path]
    ]

parseVersion :: FilePath -> Maybe Int
parseVersion path =
  case T.takeWhile isDigit (T.pack path) of
    "" -> Nothing
    digits -> readMaybe (T.unpack digits)

-- | Concatenate the migration SQL for the requested versions, in order.
-- 'Left' names the versions whose files are missing — a packaging fault,
-- not operator error.
selectMigrationSql :: [(Int, Text)] -> NonEmpty Int -> Either Text Text
selectMigrationSql files wanted =
  case [v | v <- NE.toList wanted, isNothing (lookup v files)] of
    (v : vs) ->
      Left $
        "no migration file for schema version(s): "
          <> T.intercalate ", " (map show (v : vs))
    [] ->
      Right $ T.intercalate "\n" [sql | v <- NE.toList wanted, Just sql <- [lookup v files]]

-- ---------------------------------------------------------------------------
-- * SQL builders
-- ---------------------------------------------------------------------------

stampSql
  :: Int    -- ^ schema version now applied
  -> Text   -- ^ fingerprint to record
  -> [Text] -- ^ enabled extractor names
  -> Text
stampSql version fingerprint extractors =
  T.concat
    [ "UPDATE dbsync_sync_state SET schema_version_applied = "
    , show version
    , ", schema_fingerprint = "
    , quoteLiteral fingerprint
    , ", extractors = "
    , arrayLiteral extractors
    , ", updated_at = now() WHERE id = 1;"
    ]
  where
    arrayLiteral xs = "ARRAY[" <> T.intercalate ", " (map quoteLiteral xs) <> "]::text[]"

-- ---------------------------------------------------------------------------
-- * Runner
-- ---------------------------------------------------------------------------

-- | Read the applied version + fingerprint, decide, and on an upgrade apply
-- the intervening migration files and the stamp as one transaction. Abort
-- outcomes ('DbNewerThanBinary', 'SchemaDriftUncovered') touch nothing — the
-- caller turns them into a boot error. A missing row is left for the boot
-- check to report.
runMigrations
  :: Conn.Connection
  -> Int    -- ^ schema version this binary targets
  -> Text   -- ^ fingerprint this binary declares
  -> [Text] -- ^ enabled extractor names
  -> IO MigrationOutcome
runMigrations conn target declaredFp extractors = do
  mGate <- readSchemaGate conn
  case mGate of
    Nothing -> pure NoMigrationNeeded
    Just (applied, storedFp) ->
      case decideMigrations applied target storedFp declaredFp of
        outcome@(MigrationsToApply versions) -> do
          script <-
            either (throwIO . userError . T.unpack) pure $
              selectMigrationSql migrationFiles versions
          applyScript conn (script <> "\n" <> stampSql target declaredFp extractors)
          pure outcome
        outcome -> pure outcome

readSchemaGate :: Conn.Connection -> IO (Maybe (Int, Text))
readSchemaGate conn = runSession conn (Sess.statement () gateStatement)
  where
    gateStatement = Stmt.preparable sql E.noParams decoder
    sql = "SELECT schema_version_applied, schema_fingerprint FROM dbsync_sync_state WHERE id = 1"
    decoder =
      D.rowMaybe $
        (,)
          <$> (fromIntegral <$> D.column (D.nonNullable D.int4))
          <*> D.column (D.nonNullable D.text)

applyScript :: Conn.Connection -> Text -> IO ()
applyScript conn script = runSession conn (Sess.script script)

runSession :: Conn.Connection -> Sess.Session a -> IO a
runSession conn session =
  Conn.use conn session >>= either failHard pure
  where
    failHard e = throwIO (userError ("migration session failed: " <> T.unpack (show e)))
