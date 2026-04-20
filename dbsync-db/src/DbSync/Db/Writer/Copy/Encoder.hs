{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : DbSync.Writer.Copy.Encoder
Description : Encoding helpers for PostgreSQL COPY text format.

Provides functions to encode field values into the tab-separated text
format expected by PostgreSQL @COPY ... FROM STDIN@. Fields are escaped,
joined with tabs, and terminated with a newline.
-}
module DbSync.Db.Writer.Copy.Encoder
  ( -- * Encoding
    encodeToCopyRow
  , escapeField
  , encodeNull
  ) where

import Cardano.Prelude

import Data.ByteString qualified as BS

-- ---------------------------------------------------------------------------
-- * Encoding
-- ---------------------------------------------------------------------------

-- | Encode a list of nullable fields into a single COPY row.
--
-- 'Nothing' values become @\\N@ (the PostgreSQL COPY null marker).
-- 'Just' values are escaped via 'escapeField'. Fields are joined with
-- tab characters and the row is terminated with a newline.
encodeToCopyRow :: [Maybe ByteString] -> ByteString
encodeToCopyRow fields =
  BS.intercalate "\t" (map encodeField fields) <> "\n"
  where
    encodeField :: Maybe ByteString -> ByteString
    encodeField Nothing  = encodeNull
    encodeField (Just v) = escapeField v

-- | Escape a single field value for PostgreSQL COPY text format.
--
-- Replaces backslash, tab, and newline characters with their
-- backslash-escaped equivalents (@\\\\@, @\\t@, @\\n@).
escapeField :: ByteString -> ByteString
escapeField =
    replaceBS "\\" "\\\\"   -- backslash first to avoid double-escaping
  . replaceBS "\t" "\\t"
  . replaceBS "\n" "\\n"

-- | The PostgreSQL COPY null representation: @\\N@.
encodeNull :: ByteString
encodeNull = "\\N"

-- ---------------------------------------------------------------------------
-- * Internal
-- ---------------------------------------------------------------------------

-- | Simple ByteString replacement.
-- Placeholder — a production implementation should use a streaming builder
-- for performance on large fields.
replaceBS :: ByteString -> ByteString -> ByteString -> ByteString
replaceBS needle replacement haystack =
  BS.intercalate replacement (splitOnBS needle haystack)

-- | Split a 'ByteString' on a delimiter.
-- Naive implementation suitable for short delimiters.
splitOnBS :: ByteString -> ByteString -> [ByteString]
splitOnBS delim bs
  | BS.null delim = [bs]
  | otherwise     = go bs
  where
    go s = case BS.breakSubstring delim s of
      (before, after)
        | BS.null after -> [before]
        | otherwise     -> before : go (BS.drop (BS.length delim) after)
