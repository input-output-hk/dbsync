{-# LANGUAGE OverloadedStrings #-}

-- | Proves the committed migration ladder (the frozen baseline plus any
-- version files) builds byte-for-byte the same schema as 'initSchema', by
-- comparing normalized @pg_dump --schema-only@ output. Fails if the
-- declared schema drifts from the committed baseline. Needs a live
-- @dbsync_test@ database and @pg_dump@ on PATH.
module DbSync.Schema.Migration.LadderSpec (spec) where

import Cardano.Prelude

import Data.List (sortOn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Process (readProcessWithExitCode)
import Test.Hspec (Spec, afterAll_, describe, it, shouldBe)

import DbSync.Db.Schema.Init (execPsql, initSchema)
import DbSync.Db.Schema.Migration.Files (embeddedMigrationFiles)
import DbSync.Db.Schema.Types (TableDef)
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Extractor.Registry (allKnownExtractors)
import DbSync.Test.Database (testConnStr)

spec :: Spec
spec =
  describe "DbSync.Schema.Migration ladder" $
    afterAll_ resetPublicSchema $
      it "the baseline + version ladder builds the same schema as initSchema" $ do
        resetPublicSchema
        initSchema dataTables testConnStr
        initDump <- normalizeDump <$> schemaDump

        resetPublicSchema
        execPsql testConnStr ladderSql
        ladderDump <- normalizeDump <$> schemaDump

        ladderDump `shouldBe` initDump

-- ---------------------------------------------------------------------------
-- * Schema builders
-- ---------------------------------------------------------------------------

-- | The data tables 'initSchema' creates, in the order the baseline was
-- rendered from. 'initSchema' prepends the bookkeeping tables and appends
-- the epoch views itself.
dataTables :: [TableDef]
dataTables = concatMap pdTables allKnownExtractors

-- | Every embedded migration file applied in version order: the baseline
-- first (zero-padded filenames sort numerically), then each version file.
ladderSql :: Text
ladderSql =
  T.intercalate "\n"
    [TE.decodeUtf8 contents | (_, contents) <- sortOn fst embeddedMigrationFiles]

resetPublicSchema :: IO ()
resetPublicSchema =
  execPsql testConnStr "DROP SCHEMA IF EXISTS public CASCADE; CREATE SCHEMA public;"

-- ---------------------------------------------------------------------------
-- * Dump + normalize
-- ---------------------------------------------------------------------------

schemaDump :: IO Text
schemaDump = do
  (code, out, err) <-
    readProcessWithExitCode
      "pg_dump"
      ["--schema-only", "--no-owner", "--no-privileges", "--dbname", T.unpack testConnStr]
      ""
  case code of
    ExitSuccess -> pure (T.pack out)
    ExitFailure _ -> panic ("pg_dump failed: " <> T.pack err)

-- | Drop the dump preamble that varies with server/tool version (comments,
-- @SET@ lines, @search_path@ config, psql meta-commands) so only the schema
-- objects remain.
normalizeDump :: Text -> Text
normalizeDump = T.unlines . filter keep . map T.stripEnd . T.lines
  where
    keep line =
      let s = T.strip line
       in not (T.null s)
            && not ("--" `T.isPrefixOf` s)
            && not ("SET " `T.isPrefixOf` s)
            && not ("SELECT pg_catalog.set_config" `T.isPrefixOf` s)
            && not ("\\" `T.isPrefixOf` s)
