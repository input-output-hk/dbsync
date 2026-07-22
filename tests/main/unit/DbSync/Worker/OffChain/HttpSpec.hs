{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pure surface of the off-chain HTTP layer: URL validation, IPFS
-- gateway rewriting, the SSRF address filter, and the vote-anchor
-- decision tree. The network round-trip itself is not exercised here.
module DbSync.Worker.OffChain.HttpSpec (spec) where

import Cardano.Prelude

import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Client as Http
import qualified Network.Socket as Socket

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Types (AnchorType (..))
import DbSync.Worker.OffChain.Http
  ( buildVoteMetadata
  , isPrivateAddr
  , parseOffChainUrl
  , rewriteIpfsUrl
  )
import DbSync.Worker.OffChain.Types (FetchError (..), VoteMetadata (..))

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

blake2b256 :: ByteString -> ByteString
blake2b256 = Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256)

v4 :: (Word8, Word8, Word8, Word8) -> Socket.SockAddr
v4 t = Socket.SockAddrInet 0 (Socket.tupleToHostAddress t)

v6 :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16) -> Socket.SockAddr
v6 t = Socket.SockAddrInet6 0 0 (Socket.tupleToHostAddress6 t) 0

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  parseOffChainUrlSpec
  rewriteIpfsUrlSpec
  isPrivateAddrSpec
  buildVoteMetadataSpec

parseOffChainUrlSpec :: Spec
parseOffChainUrlSpec = describe "DbSync.Worker.OffChain.Http.parseOffChainUrl" $ do

  it "accepts an https URL and keeps host + scheme" $ do
    (Http.host <$> parseOffChainUrl "https://example.com/meta.json")
      `shouldBe` Right "example.com"
    (Http.secure <$> parseOffChainUrl "https://example.com/meta.json")
      `shouldBe` Right True

  it "accepts a plain http URL as insecure" $
    (Http.secure <$> parseOffChainUrl "http://example.com/meta.json")
      `shouldBe` Right False

  it "rejects a non-http(s) scheme" $
    void (parseOffChainUrl "ftp://example.com/x")
      `shouldBe` Left (FetchErrorBadUrl "only http(s) URLs are allowed")

  it "rejects localhost by name" $
    void (parseOffChainUrl "https://localhost/x")
      `shouldBe` Left (FetchErrorBadUrl "access to localhost is not allowed")

  it "rejects the loopback IP literal" $
    void (parseOffChainUrl "http://127.0.0.1/x")
      `shouldBe` Left (FetchErrorBadUrl "access to localhost is not allowed")

rewriteIpfsUrlSpec :: Spec
rewriteIpfsUrlSpec = describe "DbSync.Worker.OffChain.Http.rewriteIpfsUrl" $ do

  it "expands an ipfs URL across every gateway" $
    rewriteIpfsUrl "ipfs://QmHash" ["https://gw1.test/", "https://gw2.test/"]
      `shouldBe` Just ["https://gw1.test/QmHash", "https://gw2.test/QmHash"]

  it "returns Nothing for a non-ipfs URL" $
    rewriteIpfsUrl "https://example.com/x" ["https://gw.test/"]
      `shouldBe` Nothing

  it "returns Just [] when no gateways are configured" $
    rewriteIpfsUrl "ipfs://QmHash" [] `shouldBe` Just []

isPrivateAddrSpec :: Spec
isPrivateAddrSpec = describe "DbSync.Worker.OffChain.Http.isPrivateAddr" $ do

  it "rejects private, loopback, link-local, CGNAT and reserved IPv4" $ do
    isPrivateAddr (v4 (0, 0, 0, 0))       `shouldBe` True
    isPrivateAddr (v4 (10, 0, 0, 1))      `shouldBe` True
    isPrivateAddr (v4 (100, 64, 0, 1))    `shouldBe` True
    isPrivateAddr (v4 (127, 0, 0, 1))     `shouldBe` True
    isPrivateAddr (v4 (169, 254, 0, 1))   `shouldBe` True
    isPrivateAddr (v4 (172, 16, 0, 1))    `shouldBe` True
    isPrivateAddr (v4 (192, 168, 1, 1))   `shouldBe` True
    isPrivateAddr (v4 (198, 18, 0, 1))    `shouldBe` True
    isPrivateAddr (v4 (224, 0, 0, 1))     `shouldBe` True

  it "allows public IPv4, including addresses just outside a private range" $ do
    isPrivateAddr (v4 (8, 8, 8, 8))       `shouldBe` False
    isPrivateAddr (v4 (1, 1, 1, 1))       `shouldBe` False
    isPrivateAddr (v4 (172, 32, 0, 1))    `shouldBe` False
    isPrivateAddr (v4 (100, 128, 0, 1))   `shouldBe` False
    isPrivateAddr (v4 (192, 167, 0, 1))   `shouldBe` False

  it "rejects loopback, ULA and link-local IPv6" $ do
    isPrivateAddr (v6 (0, 0, 0, 0, 0, 0, 0, 0)) `shouldBe` True
    isPrivateAddr (v6 (0, 0, 0, 0, 0, 0, 0, 1)) `shouldBe` True
    isPrivateAddr (v6 (0xfc00, 0, 0, 0, 0, 0, 0, 1)) `shouldBe` True
    isPrivateAddr (v6 (0xfe80, 0, 0, 0, 0, 0, 0, 1)) `shouldBe` True

  it "sees through an IPv4-mapped IPv6 address to the embedded v4" $ do
    isPrivateAddr (v6 (0, 0, 0, 0, 0, 0xFFFF, 0x0a00, 0x0001)) `shouldBe` True
    isPrivateAddr (v6 (0, 0, 0, 0, 0, 0xFFFF, 0x0808, 0x0808)) `shouldBe` False

  it "allows a public IPv6 address" $
    isPrivateAddr (v6 (0x2001, 0x4860, 0, 0, 0, 0, 0, 0x8888)) `shouldBe` False

buildVoteMetadataSpec :: Spec
buildVoteMetadataSpec = describe "DbSync.Worker.OffChain.Http.buildVoteMetadata" $ do

  it "fails when the body hash does not match the expected hash" $ do
    let body = "{}"
    void (buildVoteMetadata body (LBS.fromStrict body) "not-the-hash" VoteAnchor)
      `shouldBe` Left (FetchErrorHashMismatch "not-the-hash" (blake2b256 body))

  it "keeps an unparseable body but flags it invalid, with no CIP data" $ do
    let body = "this is not json"
        vm   = buildVoteMetadata body (LBS.fromStrict body) (blake2b256 body) VoteAnchor
    fmap vmIsValidJson vm       `shouldBe` Right False
    fmap (isNothing . vmVoteData) vm `shouldBe` Right True
    fmap (isNothing . vmWarning) vm  `shouldBe` Right True
    fmap vmHash vm              `shouldBe` Right (blake2b256 body)

  it "treats a NUL-bearing JSON document as invalid (PostgreSQL cannot store it)" $ do
    let body = "{\"x\":\"\\u0000\"}"
        vm   = buildVoteMetadata body (LBS.fromStrict body) (blake2b256 body) VoteAnchor
    fmap vmIsValidJson vm            `shouldBe` Right False
    fmap (isNothing . vmVoteData) vm `shouldBe` Right True

  it "accepts valid JSON that fails CIP validation, warning instead of dropping the row" $ do
    let body = "{}"
        vm   = buildVoteMetadata body (LBS.fromStrict body) (blake2b256 body) VoteAnchor
    fmap vmIsValidJson vm            `shouldBe` Right True
    fmap (isNothing . vmVoteData) vm `shouldBe` Right True
    fmap (isJust . vmWarning) vm     `shouldBe` Right True
