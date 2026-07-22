{-# LANGUAGE OverloadedStrings #-}

-- | Shape assertions for the @encode*Copy@ row encoders.
--
-- The loader's COPY statement lists 'copyableColumnList' columns;
-- every encoder must emit exactly those fields in that order. These
-- helpers pin that contract against the production column list
-- rather than a hand-counted tab total. Failures panic (the testlib
-- is hspec-free); hspec reports the message as the test failure.
module DbSync.Test.Copy
  ( copyRowFields
  , shouldMatchCopyShape
  ) where

import Cardano.Prelude

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import DbSync.Db.Schema.Types (ColumnDef (..), TableDef (..))
import DbSync.Db.Statement.Loader (copyableColumnList)

-- | Fields of one encoded COPY row: one trailing newline dropped,
-- split on tabs.
copyRowFields :: ByteString -> [ByteString]
copyRowFields row = BS8.split '\t' (dropNewline row)
  where
    dropNewline bs
      | BS8.isSuffixOf "\n" bs = BS8.init bs
      | otherwise              = bs

-- | The row is newline-terminated, has exactly one field per
-- copyable column (production 'copyableColumnList'), and no
-- non-nullable position holds @\\N@. Order-sensitive: a swapped or
-- omitted field shifts a @\\N@ into a non-nullable slot and fails.
shouldMatchCopyShape :: HasCallStack => TableDef -> ByteString -> IO ()
shouldMatchCopyShape td row = do
  unless (BS8.isSuffixOf "\n" row) $
    failWith "row is not newline-terminated"
  let names  = copyableNames td
      fields = copyRowFields row
  unless (length fields == length names) $
    failWith $ mconcat
      [ "expected ", show (length names), " fields for copyable columns "
      , show names
      , " but the encoder emitted ", show (length fields)
      ]
  forM_ (zip [0 :: Int ..] (zip names fields)) $ \(i, (name, field)) ->
    when (field == "\\N" && not (nullableOf name)) $
      failWith $ mconcat
        [ "non-nullable column ", name
        , " (position ", show i, ") holds \\N"
        ]
  where
    failWith :: Text -> IO ()
    failWith msg = panic $ "table " <> tdName td <> ": " <> msg

    nullableOf name =
      case find ((== name) . cdName) (tdColumns td) of
        Just c  -> cdNullable c
        Nothing -> False

-- | Copyable column names, decoded from the production column list.
copyableNames :: TableDef -> [Text]
copyableNames td =
  map (T.dropAround (== '"')) $
    T.splitOn ", " (TE.decodeUtf8 (copyableColumnList td))
