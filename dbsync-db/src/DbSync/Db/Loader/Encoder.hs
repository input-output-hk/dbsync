{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : DbSync.Db.Loader.Encoder
Description : Encoding helpers for the loader-stream wire format.

Builder-based encoding pipeline producing one row per call, ready to
hand to the loader-stream transport (currently PostgreSQL @COPY ... FROM
STDIN@). All field values are constructed as 'Builder's, joined with
tabs, and materialised to a strict 'ByteString' once via 'buildCopyRow'.

== Why Builders

The previous implementation used @BS8.concatMap@ for hex encoding
(~50K tiny pinned ByteStrings per tx_cbor row) and 3-pass @replaceBS@
for escaping. This caused massive GC pressure from pinned-memory
fragmentation. The Builder pipeline produces zero intermediate
ByteStrings — everything is assembled in a single buffer.
-}
module DbSync.Db.Loader.Encoder
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
  , byteStringHex
  , char7
  , int64Dec
  , word16Dec
  , word64Dec
  )
import Data.ByteString.Builder.Extra
  ( smallChunkSize
  , toLazyByteStringWith
  , untrimmedStrategy
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
--
-- The allocation strategy matters here: the default
-- 'Data.ByteString.Builder.toLazyByteString' starts with a ~4KB
-- buffer and trim-copies it, which for a typical 100-300B row is a
-- ~15-40x allocation amplification per row. A 512-byte untrimmed
-- first chunk fits most rows exactly once; 'LBS.toStrict' then does
-- the single copy of the bytes actually used.
buildCopyRow :: [CopyField] -> ByteString
buildCopyRow fields =
  LBS.toStrict
    . toLazyByteStringWith (untrimmedStrategy 512 smallChunkSize) LBS.empty
    $ go fields
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
bInt64 = int64Dec

-- | Encode a 'Word64' as decimal ASCII.
{-# INLINE bWord64 #-}
bWord64 :: Word64 -> Builder
bWord64 = word64Dec

-- | Encode a 'Word16' as decimal ASCII.
{-# INLINE bWord16 #-}
bWord16 :: Word16 -> Builder
bWord16 = word16Dec

-- | Encode a 'Bool' as @t@ or @f@ (PostgreSQL COPY boolean format).
{-# INLINE bBool #-}
bBool :: Bool -> Builder
bBool True  = char7 't'
bBool False = char7 'f'

-- | Encode a 'ByteString' as hex with @\\\\x@ prefix for PostgreSQL
-- bytea COPY format.
--
-- 'byteStringHex' is a fused fixed-size-primitive loop that writes
-- nibble pairs straight into the output buffer — no per-byte
-- 'Builder' closures (which for a 50KB @tx_cbor@ row used to mean
-- megabytes of transient heap per row).
{-# INLINE bHex #-}
bHex :: ByteString -> Builder
bHex bs =
  -- \\x prefix (two backslashes for COPY escaping + 'x')
  byteString "\\\\x" <> byteStringHex bs

-- | Encode a 'UTCTime' as @YYYY-MM-DD HH:MM:SS@ (PostgreSQL timestamp).
{-# INLINE bUTCTime #-}
bUTCTime :: UTCTime -> Builder
bUTCTime = byteString . BS8.pack . formatTime defaultTimeLocale "%F %T"

-- | Encode 'Text' for COPY: UTF-8 encode then escape special chars.
{-# INLINE bText #-}
bText :: Text -> Builder
bText = bEscapeText . TE.encodeUtf8

-- | Escape a UTF-8 'ByteString' for COPY text format.
--
-- Backslash → @\\\\@, tab → @\\t@, newline → @\\n@, carriage return
-- → @\\r@ (the server rejects a bare CR in text-format COPY data),
-- all others pass through. Clean spans are emitted as whole slices,
-- so the common no-escapes case is one scan with zero per-byte work.
bEscapeText :: ByteString -> Builder
bEscapeText bs =
  case BS.uncons rest of
    Nothing         -> byteString clean
    Just (w, rest') -> byteString clean <> escaped w <> bEscapeText rest'
  where
    (clean, rest) = BS.break needsEscape bs

    needsEscape :: Word8 -> Bool
    needsEscape w = w == 0x5C || w == 0x09 || w == 0x0A || w == 0x0D

    escaped :: Word8 -> Builder
    escaped 0x5C = byteString "\\\\"
    escaped 0x09 = byteString "\\t"
    escaped 0x0A = byteString "\\n"
    escaped _    = byteString "\\r"

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
