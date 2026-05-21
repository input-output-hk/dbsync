{-# LANGUAGE OverloadedStrings #-}

module DbSync.Phase.Ingest.UtxoCacheSpec (spec) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Ids (TxId (..), TxOutId (..))
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Phase.Ingest.UtxoCache

mkHash :: Word8 -> ByteString
mkHash w = BS.pack (replicate 32 w)

-- | Build an entry whose outputs all share one base 'TxOutId' (the
-- supplied tx id × 100 + the output index) so tests can assert on the
-- returned id without threading allocator state.
mkEntry :: Int64 -> [Word64] -> UtxoTxEntry
mkEntry tid vals = UtxoTxEntry
  { uteTxId    = TxId tid
  , uteOutputs = Seq.fromList
      [ (TxOutId (tid * 100 + fromIntegral i), DbLovelace v)
      | (i, v) <- zip [0 :: Int ..] vals
      ]
  }

spec :: Spec
spec = do
  describe "lookupInput" $ do
    it "returns Nothing for an unknown hash" $ do
      cache <- newUtxoCache 16
      result <- lookupInput cache (mkHash 0x11) 0
      result `shouldBe` Nothing

    it "returns the producer TxId, the per-output TxOutId, and the value on a hit" $ do
      cache <- newUtxoCache 16
      recordTx cache (mkHash 0x22) (mkEntry 42 [1000, 2000, 3000])
      r0 <- lookupInput cache (mkHash 0x22) 0
      r1 <- lookupInput cache (mkHash 0x22) 1
      r2 <- lookupInput cache (mkHash 0x22) 2
      r0 `shouldBe` Just (TxId 42, TxOutId 4200, DbLovelace 1000)
      r1 `shouldBe` Just (TxId 42, TxOutId 4201, DbLovelace 2000)
      r2 `shouldBe` Just (TxId 42, TxOutId 4202, DbLovelace 3000)

    it "returns Nothing for an output index past the end" $ do
      cache <- newUtxoCache 16
      recordTx cache (mkHash 0x33) (mkEntry 1 [500])
      result <- lookupInput cache (mkHash 0x33) 5
      result `shouldBe` Nothing

  describe "recordTx FIFO eviction" $ do
    it "evicts the oldest entry once capacity is exceeded" $ do
      cache <- newUtxoCache 2
      recordTx cache (mkHash 0xa1) (mkEntry 1 [100])
      recordTx cache (mkHash 0xa2) (mkEntry 2 [200])
      recordTx cache (mkHash 0xa3) (mkEntry 3 [300])
      r1 <- lookupInput cache (mkHash 0xa1) 0
      r2 <- lookupInput cache (mkHash 0xa2) 0
      r3 <- lookupInput cache (mkHash 0xa3) 0
      r1 `shouldBe` Nothing
      r2 `shouldBe` Just (TxId 2, TxOutId 200, DbLovelace 200)
      r3 `shouldBe` Just (TxId 3, TxOutId 300, DbLovelace 300)

    it "overwriting an existing hash does not consume ring capacity" $ do
      cache <- newUtxoCache 2
      recordTx cache (mkHash 0xb1) (mkEntry 1 [100])
      recordTx cache (mkHash 0xb1) (mkEntry 11 [110])
      recordTx cache (mkHash 0xb2) (mkEntry 2 [200])
      r1 <- lookupInput cache (mkHash 0xb1) 0
      r2 <- lookupInput cache (mkHash 0xb2) 0
      r1 `shouldBe` Just (TxId 11, TxOutId 1100, DbLovelace 110)
      r2 `shouldBe` Just (TxId 2, TxOutId 200, DbLovelace 200)

  describe "readCacheStats" $ do
    it "counts hits, misses, evictions, and live entries" $ do
      cache <- newUtxoCache 2
      recordTx cache (mkHash 0xc1) (mkEntry 1 [10])
      recordTx cache (mkHash 0xc2) (mkEntry 2 [20])
      _ <- lookupInput cache (mkHash 0xc1) 0
      _ <- lookupInput cache (mkHash 0xc2) 0
      _ <- lookupInput cache (mkHash 0xff) 0
      recordTx cache (mkHash 0xc3) (mkEntry 3 [30])
      stats <- readCacheStats cache
      csHits stats      `shouldBe` 2
      csMisses stats    `shouldBe` 1
      csEvictions stats `shouldBe` 1
      csEntries stats   `shouldBe` 2
