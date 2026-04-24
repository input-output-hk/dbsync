{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : DbSync.Writer.Copy.Encoder
Description : Encoding helpers for PostgreSQL COPY text format.

Builder-based encoding pipeline for PostgreSQL @COPY ... FROM STDIN@.
All field values are constructed as 'Builder's, joined with tabs,
and materialised to a strict 'ByteString' once via 'buildCopyRow'.

== Why Builders

The previous implementation used @BS8.concatMap@ for hex encoding
(~50K tiny pinned ByteStrings per tx_cbor row) and 3-pass @replaceBS@
for escaping. This caused massive GC pressure from pinned-memory
fragmentation. The Builder pipeline produces zero intermediate
ByteStrings — everything is assembled in a single buffer.
-}
module DbSync.Db.Writer.Copy.Encoder
  ( -- * Builder-based encoding
    CopyField
  , buildCopyRow
  , bInt64
  , bWord64
  , bWord16
  , bBool
  , bHex
  , bUTCTime
  , bText
  , bEscapeText

    -- * Legacy API (for test compatibility)
  , encodeToCopyRow
  , escapeField
  , encodeNull
  ) where

import Cardano.Prelude

import Data.ByteString.Builder
  ( Builder
  , byteString
  , char7
  , toLazyByteString
  , word8
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | A single COPY field. 'Nothing' encodes as @\\N@ (PostgreSQL NULL).
type CopyField = Maybe Builder

-- ---------------------------------------------------------------------------
-- * Row building
-- ---------------------------------------------------------------------------

-- | Materialise a list of 'CopyField's into a single COPY text row.
--
-- Tab-separated, newline-terminated. 'Nothing' → @\\N@.
-- The entire row is built as a single 'Builder' and materialised once.
buildCopyRow :: [CopyField] -> ByteString
buildCopyRow fields =
  LBS.toStrict . toLazyByteString $ go fields
  where
    go []     = char7 '\n'
    go [f]    = field f <> char7 '\n'
    go (f:fs) = field f <> char7 '\t' <> go fs

    field :: CopyField -> Builder
    field Nothing  = byteString "\\N"
    field (Just b) = b

-- ---------------------------------------------------------------------------
-- * Primitive builders
-- ---------------------------------------------------------------------------

-- | Encode an 'Int64' as decimal ASCII.
{-# INLINE bInt64 #-}
bInt64 :: Int64 -> Builder
bInt64 = byteString . BS8.pack . show

-- | Encode a 'Word64' as decimal ASCII.
{-# INLINE bWord64 #-}
bWord64 :: Word64 -> Builder
bWord64 = byteString . BS8.pack . show

-- | Encode a 'Word16' as decimal ASCII.
{-# INLINE bWord16 #-}
bWord16 :: Word16 -> Builder
bWord16 = byteString . BS8.pack . show

-- | Encode a 'Bool' as @t@ or @f@ (PostgreSQL COPY boolean format).
{-# INLINE bBool #-}
bBool :: Bool -> Builder
bBool True  = char7 't'
bBool False = char7 'f'

-- | Encode a 'ByteString' as hex with @\\\\x@ prefix for PostgreSQL
-- bytea COPY format.
--
-- Zero intermediate ByteStrings: each byte is emitted as two hex
-- nibbles directly into the Builder buffer.
{-# INLINE bHex #-}
bHex :: ByteString -> Builder
bHex bs =
  -- \\x prefix (two backslashes for COPY escaping + 'x')
  word8 0x5C <> word8 0x5C <> word8 0x78 <> BS.foldl' step mempty bs
  where
    step :: Builder -> Word8 -> Builder
    step acc w =
      acc <> word8 (hexNibble (w `shiftR` 4))
          <> word8 (hexNibble (w .&. 0x0F))

    hexNibble :: Word8 -> Word8
    hexNibble n
      | n < 10    = n + 0x30  -- '0'
      | otherwise = n - 10 + 0x61  -- 'a'

-- | Encode a 'UTCTime' as @YYYY-MM-DD HH:MM:SS@ (PostgreSQL timestamp).
{-# INLINE bUTCTime #-}
bUTCTime :: UTCTime -> Builder
bUTCTime = byteString . BS8.pack . formatTime defaultTimeLocale "%F %T"

-- | Encode 'Text' for COPY: UTF-8 encode then escape special chars.
{-# INLINE bText #-}
bText :: Text -> Builder
bText = bEscapeText . TE.encodeUtf8

-- | Escape a UTF-8 'ByteString' for COPY text format in a single pass.
--
-- Backslash → @\\\\@, tab → @\\t@, newline → @\\n@, all others pass through.
{-# INLINE bEscapeText #-}
bEscapeText :: ByteString -> Builder
bEscapeText = BS.foldl' step mempty
  where
    step :: Builder -> Word8 -> Builder
    step acc w = acc <> case w of
      0x5C -> word8 0x5C <> word8 0x5C  -- backslash → \\
      0x09 -> word8 0x5C <> word8 0x74  -- tab → \t
      0x0A -> word8 0x5C <> word8 0x6E  -- newline → \n
      _    -> word8 w

-- ---------------------------------------------------------------------------
-- * Legacy API (for test compatibility)
-- ---------------------------------------------------------------------------

-- | Encode a list of nullable fields into a single COPY row.
--
-- Legacy wrapper — new code should use 'buildCopyRow' with Builder fields.
encodeToCopyRow :: [Maybe ByteString] -> ByteString
encodeToCopyRow fields =
  BS.intercalate "\t" (map encodeField fields) <> "\n"
  where
    encodeField :: Maybe ByteString -> ByteString
    encodeField Nothing  = encodeNull
    encodeField (Just v) = escapeField v

-- | Escape a single field value for PostgreSQL COPY text format.
escapeField :: ByteString -> ByteString
escapeField =
    replaceBS "\\" "\\\\"
  . replaceBS "\t" "\\t"
  . replaceBS "\n" "\\n"

-- | The PostgreSQL COPY null representation: @\\N@.
encodeNull :: ByteString
encodeNull = "\\N"

-- ---------------------------------------------------------------------------
-- * Internal
-- ---------------------------------------------------------------------------

replaceBS :: ByteString -> ByteString -> ByteString -> ByteString
replaceBS needle replacement haystack =
  BS.intercalate replacement (splitOnBS needle haystack)

splitOnBS :: ByteString -> ByteString -> [ByteString]
splitOnBS delim bs
  | BS.null delim = [bs]
  | otherwise     = go bs
  where
    go s = case BS.breakSubstring delim s of
      (before, after)
        | BS.null after -> [before]
        | otherwise     -> before : go (BS.drop (BS.length delim) after)
