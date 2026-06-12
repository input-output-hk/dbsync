{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | HTTP utilities for the off-chain pool and vote workers.
--
-- Provides a 'Http.Manager' that refuses to connect to private or
-- loopback IP addresses (DNS rebinding and SSRF defence), a URL
-- validator that only allows http(s) GETs to non-localhost hosts,
-- and the per-type fetchers that the two workers wrap.
--
-- Pool and vote fetchers share 'httpGetBytes' for the actual HTTP
-- round-trip and content-type / hash validation; the rest is
-- per-domain decoding (pool: a ticker-bearing JSON object; vote: a
-- CIP-100/108/119 envelope — see "DbSync.Worker.OffChain.Vote.Types").
module DbSync.Worker.OffChain.Http
  ( -- * Manager
    newRestrictedManager

    -- * URL handling
  , parseOffChainUrl
  , rewriteIpfsUrl

    -- * Fetchers
  , fetchPoolMetadata
  , fetchVoteMetadata
  ) where

import Cardano.Prelude

import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Client (HttpException (..))
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.Restricted
  ( Restriction
  , addressRestriction
  , connectionRestricted
  , mkRestrictedManagerSettings
  )
import qualified Network.HTTP.Types as Http
import qualified Network.Socket as Socket

import DbSync.Db.Types (AnchorType)
import DbSync.Util (jsonValueContainsNul)
import DbSync.Worker.OffChain.Types
  ( FetchError (..)
  , PoolMetadata (..)
  , VoteMetadata (..)
  )
import qualified DbSync.Worker.OffChain.Vote.Types as Vote

-- ---------------------------------------------------------------------------
-- * Restricted Manager
-- ---------------------------------------------------------------------------

-- | A 'Http.Manager' that refuses to dial private, loopback, or
-- link-local IP addresses. The check runs at connect time on the
-- resolved IP, so it covers redirects and DNS-rebinding attacks.
newRestrictedManager :: IO Http.Manager
newRestrictedManager = do
  (settings, _mProxyRestricted) <-
    mkRestrictedManagerSettings offchainRestriction Nothing Nothing
  Http.newManager settings

offchainRestriction :: Restriction
offchainRestriction = addressRestriction $ \addr ->
  if isPrivateAddr (Socket.addrAddress addr)
    then Just $
      connectionRestricted
        ("Access to private, loopback, or link-local IP address is not allowed: " ++)
        addr
    else Nothing

isPrivateAddr :: Socket.SockAddr -> Bool
isPrivateAddr (Socket.SockAddrInet _ hostAddr) =
  let (a, b, _, _) = Socket.hostAddressToTuple hostAddr
   in a == 0                                  -- 0.0.0.0/8     (current network)
        || a == 10                            -- 10.0.0.0/8    (private)
        || (a == 100 && b >= 64 && b <= 127)  -- 100.64.0.0/10 (CGNAT)
        || a == 127                           -- 127.0.0.0/8   (loopback)
        || (a == 169 && b == 254)             -- 169.254.0.0/16 (link-local)
        || (a == 172 && b >= 16 && b <= 31)   -- 172.16.0.0/12 (private)
        || (a == 192 && b == 168)             -- 192.168.0.0/16 (private)
        || (a == 198 && b >= 18 && b <= 19)   -- 198.18.0.0/15 (benchmarking)
        || a >= 224                           -- 224.0.0.0+    (multicast/reserved)
isPrivateAddr (Socket.SockAddrInet6 _ _ hostAddr6 _) =
  let addr@(w1, w2, w3, w4, w5, w6, w7, w8) = Socket.hostAddress6ToTuple hostAddr6
      ipv4Mapped = (w1, w2, w3, w4, w5, w6) == (0, 0, 0, 0, 0, 0xFFFF)
      mappedIPv4Addr =
        Socket.SockAddrInet 0 $
          Socket.tupleToHostAddress
            ( fromIntegral (w7 `shiftR` 8)
            , fromIntegral (w7 .&. 0xFF)
            , fromIntegral (w8 `shiftR` 8)
            , fromIntegral (w8 .&. 0xFF)
            )
   in addr == (0, 0, 0, 0, 0, 0, 0, 0)          -- ::
        || addr == (0, 0, 0, 0, 0, 0, 0, 1)     -- ::1
        || (w1 .&. 0xFE00) == 0xFC00            -- fc00::/7  (ULA)
        || (w1 .&. 0xFFC0) == 0xFE80            -- fe80::/10 (link-local)
        || (ipv4Mapped && isPrivateAddr mappedIPv4Addr)
isPrivateAddr _ = False

-- ---------------------------------------------------------------------------
-- * URL handling
-- ---------------------------------------------------------------------------

-- | Validate an off-chain URL and build a 'Http.Request' for it.
--
-- Only http(s) GET requests to non-localhost hosts are allowed.
-- The request carries a placeholder @content-type@ header to satisfy
-- a small number of servers that 415 without one.
parseOffChainUrl :: Text -> Either FetchError Http.Request
parseOffChainUrl urlT = do
  unless (Text.isPrefixOf "https://" urlT || Text.isPrefixOf "http://" urlT) $
    Left (FetchErrorBadUrl "only http(s) URLs are allowed")
  request <- first (FetchErrorBadUrl . Text.pack . displayException)
    (Http.parseRequest (Text.unpack urlT) :: Either SomeException Http.Request)
  unless (Http.method request == "GET") $
    Left (FetchErrorBadUrl "only GET requests are allowed")
  when (isLocalhostHost (Http.host request)) $
    Left (FetchErrorBadUrl "access to localhost is not allowed")
  pure (applyContentType request)

applyContentType :: Http.Request -> Http.Request
applyContentType req = req
  { Http.requestHeaders =
      Http.requestHeaders req <> [(CI.mk "content-type", "application/json")]
  }

isLocalhostHost :: ByteString -> Bool
isLocalhostHost host =
  host == "localhost"
    || host == "127.0.0.1"
    || host == "::1"
    || host == "[::1]"

-- | If @url@ is an @ipfs://@ URI, return the list of HTTPS URLs the
-- caller should try (one per configured gateway). Otherwise return
-- 'Nothing'.
rewriteIpfsUrl :: Text -> [Text] -> Maybe [Text]
rewriteIpfsUrl url gateways = case Text.stripPrefix "ipfs://" url of
  Just suffix -> Just (map (<> suffix) gateways)
  Nothing     -> Nothing

-- ---------------------------------------------------------------------------
-- * Byte fetch (shared by pool + vote)
-- ---------------------------------------------------------------------------

-- | GET the request via @manager@, enforce a body-size limit, and
-- validate the response Content-Type. Returns the strict + lazy
-- response bodies and the raw Content-Type header value if present.
httpGetBytes
  :: Http.Manager
  -> Http.Request
  -> Int                                       -- ^ bytes to attempt to read
  -> Int                                       -- ^ maximum acceptable body size
  -> IO (Either FetchError (ByteString, LBS.ByteString, Maybe ByteString))
httpGetBytes manager request bytesToRead maxBytes =
  (Http.withResponse request manager checkResponse)
    `catch` (pure . Left . convertHttpException)
  where
    checkResponse responseBR = runExceptT $ do
      let status = Http.responseStatus responseBR
      unless (Http.statusCode status == 200) $
        ExceptT . pure . Left . FetchErrorHttp $
          "status " <> show (Http.statusCode status) <> " "
            <> Text.decodeLatin1 (Http.statusMessage status)

      respLBS <- liftIO $ Http.brReadSome (Http.responseBody responseBR) bytesToRead
      let respBS       = LBS.toStrict respLBS
          mContentType = L.lookup Http.hContentType (Http.responseHeaders responseBR)

      forM_ mContentType $ \ct ->
        -- Some servers tag valid JSON as text/html. Accept it iff the
        -- body actually starts with '{'.
        if "text/html" `BS.isInfixOf` ct && isPossiblyJsonObject respBS
          then pure ()
          else
            ExceptT . pure $
              if "text/html" `BS.isInfixOf` ct
                then Left (FetchErrorBadContentType (Text.decodeLatin1 ct))
                else
                  if isAcceptableContentType ct
                    then Right ()
                    else Left (FetchErrorBadContentType (Text.decodeLatin1 ct))

      when (BS.length respBS > maxBytes) $
        ExceptT . pure $ Left (FetchErrorTooLarge maxBytes)
      pure (respBS, respLBS, mContentType)

isAcceptableContentType :: ByteString -> Bool
isAcceptableContentType ct =
  any (`BS.isInfixOf` ct)
    [ "application/json"
    , "application/ld+json"
    , "text/plain"
    , "binary/octet-stream"
    , "application/octet-stream"
    , "application/binary"
    ]

-- | The body might be JSON if the first non-whitespace byte is @{@.
-- Used to whitelist mis-typed @text/html@ JSON responses.
isPossiblyJsonObject :: ByteString -> Bool
isPossiblyJsonObject bs = case BS.uncons (BS.strip bs) of
  Just ('{', _) -> True
  _             -> False

-- ---------------------------------------------------------------------------
-- * HttpException -> FetchError
-- ---------------------------------------------------------------------------

convertHttpException :: HttpException -> FetchError
convertHttpException = \case
  HttpExceptionRequest _req hec -> case hec of
    Http.ResponseTimeout       -> FetchErrorTimeout "response"
    Http.ConnectionTimeout     -> FetchErrorTimeout "connection"
    Http.ConnectionFailure {}  -> FetchErrorConnectionFailure
    Http.TooManyRedirects {}   -> FetchErrorHttp "too many redirects"
    Http.OverlongHeaders       -> FetchErrorHttp "overlong headers"
    Http.StatusCodeException r _ ->
      FetchErrorHttp ("status " <> Text.pack (show (Http.responseStatus r)))
    Http.InvalidStatusLine {}  -> FetchErrorHttp "invalid status line"
    other -> FetchErrorHttp (Text.take 100 (Text.pack (show other)))
  InvalidUrlException _ err    -> FetchErrorBadUrl (Text.pack err)

-- ---------------------------------------------------------------------------
-- * Pool fetcher
-- ---------------------------------------------------------------------------

-- Pool size limits mirror the original cardano-db-sync (600 / 512).
poolBytesToRead, poolMaxBytes :: Int
poolBytesToRead = 600
poolMaxBytes    = 512

fetchPoolMetadata
  :: Http.Manager
  -> Text                                      -- ^ url
  -> ByteString                                -- ^ expected Blake2b_256 hash
  -> IO (Either FetchError PoolMetadata)
fetchPoolMetadata manager url expectedHash = runExceptT $ do
  request <- ExceptT . pure $ parseOffChainUrl url
  (respBS, respLBS, _ct) <-
    ExceptT (httpGetBytes manager request poolBytesToRead poolMaxBytes)
  let computedHash = Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256) respBS
  when (computedHash /= expectedHash) $
    ExceptT . pure $ Left (FetchErrorHashMismatch expectedHash computedHash)
  case Aeson.eitherDecode' @Aeson.Value respLBS of
    Left e -> ExceptT . pure $ Left (FetchErrorDecode (Text.pack e))
    -- A Unicode NUL anywhere in the document poisons both the
    -- NOT NULL jsonb column and the text columns (ticker, …)
    -- derived from it — PostgreSQL rejects NUL in both. Treat
    -- it like any other undecodable body.
    Right v
      | jsonValueContainsNul v ->
          ExceptT . pure $ Left (FetchErrorDecode "metadata contains a Unicode NUL (\\u0000), which PostgreSQL cannot store")
      | otherwise -> case extractTicker v of
          Nothing     -> ExceptT . pure $ Left (FetchErrorDecode "missing 'ticker' field")
          Just ticker -> pure PoolMetadata
            { pmTicker        = ticker
            , pmHash          = computedHash
            , pmRawBytes      = respBS
            , pmCanonicalJson = Text.decodeUtf8 (LBS.toStrict (Aeson.encode v))
            }

extractTicker :: Aeson.Value -> Maybe Text
extractTicker (Aeson.Object o) = case KeyMap.lookup "ticker" o of
  Just (Aeson.String t) -> Just t
  _                     -> Nothing
extractTicker _ = Nothing

-- ---------------------------------------------------------------------------
-- * Vote fetcher
-- ---------------------------------------------------------------------------

-- Vote size limits mirror the original (3 MB attempt + 3 MB cap).
voteBytesToRead, voteMaxBytes :: Int
voteBytesToRead = 3_000_000
voteMaxBytes    = 3_000_000

-- | Fetch a vote anchor, optionally falling back through a list of
-- IPFS gateways when the URL is @ipfs://...@.
fetchVoteMetadata
  :: Http.Manager
  -> [Text]                                    -- ^ IPFS gateway prefixes
  -> Text                                      -- ^ anchor URL
  -> ByteString                                -- ^ expected Blake2b_256 hash
  -> AnchorType
  -> IO (Either FetchError VoteMetadata)
fetchVoteMetadata manager gateways url expectedHash anchorType =
  case rewriteIpfsUrl url gateways of
    Nothing   -> fetchSingle manager url expectedHash anchorType
    Just []   -> pure (Left (FetchErrorIpfsAllGatewaysFailed []))
    Just urls -> tryGateways urls []
  where
    tryGateways [] acc =
      pure (Left (FetchErrorIpfsAllGatewaysFailed (reverse acc)))
    tryGateways (u : rest) acc = do
      res <- fetchSingle manager u expectedHash anchorType
      case res of
        Right ok -> pure (Right ok)
        Left e   -> tryGateways rest (Text.take 200 (show e) : acc)

fetchSingle
  :: Http.Manager
  -> Text
  -> ByteString
  -> AnchorType
  -> IO (Either FetchError VoteMetadata)
fetchSingle manager url expectedHash anchorType = runExceptT $ do
  request <- ExceptT . pure $ parseOffChainUrl url
  (respBS, respLBS, _ct) <-
    ExceptT (httpGetBytes manager request voteBytesToRead voteMaxBytes)
  ExceptT . pure $ buildVoteMetadata respBS respLBS expectedHash anchorType

-- | Three-way validation of a vote anchor body. Pure, so tests and
-- the IPFS fallback share one source of truth for the hash-check +
-- JSON-validity + CIP-conformance decision tree.
buildVoteMetadata
  :: ByteString
  -> LBS.ByteString
  -> ByteString
  -> AnchorType
  -> Either FetchError VoteMetadata
buildVoteMetadata respBS respLBS expectedHash anchorType = do
  let computedHash = Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256) respBS
  when (computedHash /= expectedHash) $
    Left (FetchErrorHashMismatch expectedHash computedHash)
  let (decodedValue, isValidJson) = case Aeson.eitherDecode' @Aeson.Value respLBS of
        Left err -> (jsonParseErrorPayload (Text.pack err), False)
        -- Valid JSON, but PostgreSQL rejects a Unicode NUL anywhere
        -- in a jsonb value (and in the text columns the CIP decode
        -- would feed), so the document cannot be stored as-is.
        -- Substitute the placeholder and skip the CIP decode; the
        -- bytes column keeps the original document.
        Right v
          | jsonValueContainsNul v -> (unicodeNulErrorPayload, False)
          | otherwise              -> (v, True)
      (mVote, mWarning)
        | isValidJson = case Vote.eitherDecodeOffChainVoteData respLBS anchorType of
            Left e  -> (Nothing, Just (Text.pack e))
            Right d -> (Just d, Nothing)
        | otherwise   = (Nothing, Nothing)
  Right VoteMetadata
    { vmHash          = computedHash
    , vmRawBytes      = respBS
    , vmCanonicalJson = Text.decodeUtf8 (LBS.toStrict (Aeson.encode decodedValue))
    , vmIsValidJson   = isValidJson
    , vmWarning       = mWarning
    , vmVoteData      = mVote
    }

jsonParseErrorPayload :: Text -> Aeson.Value
jsonParseErrorPayload err = Aeson.object
  [ ("error",       Aeson.String "Content is not valid JSON. See bytes column for raw data.")
  , ("parse_error", Aeson.String err)
  ]

unicodeNulErrorPayload :: Aeson.Value
unicodeNulErrorPayload = Aeson.object
  [ ("error", Aeson.String "Content contains a Unicode NUL (\\u0000), which PostgreSQL cannot store. See bytes column for raw data.")
  ]
