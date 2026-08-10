{-# LANGUAGE TemplateHaskell #-}

-- | 'embedDir' compiles the migration @.sql@ files into the binary as raw
-- bytes, so the runner needs no files on disk.
-- 'DbSync.Db.Schema.Migration' decodes and orders them.
module DbSync.Db.Schema.Migration.Files
  ( embeddedMigrationFiles
  ) where

import Cardano.Prelude (ByteString, FilePath)

import Data.FileEmbed (embedDir)

embeddedMigrationFiles :: [(FilePath, ByteString)]
embeddedMigrationFiles = $(embedDir "migrations")
