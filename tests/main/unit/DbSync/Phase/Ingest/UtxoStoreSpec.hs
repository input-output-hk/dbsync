{-# LANGUAGE OverloadedStrings #-}

module DbSync.Phase.Ingest.UtxoStoreSpec (spec) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Ids (TxId (..), TxOutId (..))
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Phase.Ingest.UtxoStore
import DbSync.Test.Lsm (withTestLsmSession, withTestUtxoStore)
import qualified DbSync.Phase.Ingest.UtxoStore as UtxoStore

mkHash :: Word8 -> ByteString
mkHash w = BS.pack (replicate 32 w)

-- | Build an entry whose outputs are derived from the supplied tx id
-- (tid × 100 + index) so tests can assert on the returned ids
-- without threading allocator state.
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
    it "returns Nothing for an unknown hash" $ withTestUtxoStore $ \store -> do
      result <- lookupInput store (mkHash 0x11) 0
      result `shouldBe` Nothing

    it "returns the producer TxId, the per-output TxOutId, and the value on a hit" $
      withTestUtxoStore $ \store -> do
        recordTx store (mkHash 0x22) (mkEntry 42 [1000, 2000, 3000])
        r0 <- lookupInput store (mkHash 0x22) 0
        r1 <- lookupInput store (mkHash 0x22) 1
        r2 <- lookupInput store (mkHash 0x22) 2
        r0 `shouldBe` Just (TxId 42, TxOutId 4200, DbLovelace 1000)
        r1 `shouldBe` Just (TxId 42, TxOutId 4201, DbLovelace 2000)
        r2 `shouldBe` Just (TxId 42, TxOutId 4202, DbLovelace 3000)

    it "returns Nothing for an output index past the end" $
      withTestUtxoStore $ \store -> do
        recordTx store (mkHash 0x33) (mkEntry 1 [500])
        result <- lookupInput store (mkHash 0x33) 5
        result `shouldBe` Nothing

  describe "recordTx" $ do
    it "replaces the prior value when re-recording the same hash" $
      withTestUtxoStore $ \store -> do
        recordTx store (mkHash 0xb1) (mkEntry 1 [100])
        recordTx store (mkHash 0xb1) (mkEntry 11 [110])
        recordTx store (mkHash 0xb2) (mkEntry 2 [200])
        r1 <- lookupInput store (mkHash 0xb1) 0
        r2 <- lookupInput store (mkHash 0xb2) 0
        r1 `shouldBe` Just (TxId 11, TxOutId 1100, DbLovelace 110)
        r2 `shouldBe` Just (TxId 2, TxOutId 200, DbLovelace 200)

    it "keeps every recorded output addressable independently" $
      withTestUtxoStore $ \store -> do
        recordTx store (mkHash 0xa1) (mkEntry 1 [100])
        recordTx store (mkHash 0xa2) (mkEntry 2 [200])
        recordTx store (mkHash 0xa3) (mkEntry 3 [300])
        r1 <- lookupInput store (mkHash 0xa1) 0
        r2 <- lookupInput store (mkHash 0xa2) 0
        r3 <- lookupInput store (mkHash 0xa3) 0
        r1 `shouldBe` Just (TxId 1, TxOutId 100, DbLovelace 100)
        r2 `shouldBe` Just (TxId 2, TxOutId 200, DbLovelace 200)
        r3 `shouldBe` Just (TxId 3, TxOutId 300, DbLovelace 300)

    it "is a no-op for a tx with no outputs" $
      withTestUtxoStore $ \store -> do
        recordTx store (mkHash 0xab) (UtxoTxEntry (TxId 99) Seq.empty)
        result <- lookupInput store (mkHash 0xab) 0
        result `shouldBe` Nothing

  describe "deleteConsumed" $ do
    it "removes the targeted output and leaves siblings hit-able" $
      withTestUtxoStore $ \store -> do
        recordTx store (mkHash 0xd0) (mkEntry 7 [70, 71, 72])
        deleteConsumed store (mkHash 0xd0) 1
        r0 <- lookupInput store (mkHash 0xd0) 0
        r1 <- lookupInput store (mkHash 0xd0) 1
        r2 <- lookupInput store (mkHash 0xd0) 2
        r0 `shouldBe` Just (TxId 7, TxOutId 700, DbLovelace 70)
        r1 `shouldBe` Nothing
        r2 `shouldBe` Just (TxId 7, TxOutId 702, DbLovelace 72)

    it "is a no-op for an absent key" $ withTestUtxoStore $ \store -> do
      recordTx store (mkHash 0xd1) (mkEntry 1 [100])
      deleteConsumed store (mkHash 0xff) 0
      result <- lookupInput store (mkHash 0xd1) 0
      result `shouldBe` Just (TxId 1, TxOutId 100, DbLovelace 100)

    it "allows re-recording after a delete (replay idempotency)" $
      withTestUtxoStore $ \store -> do
        recordTx store (mkHash 0xd2) (mkEntry 5 [50])
        deleteConsumed store (mkHash 0xd2) 0
        rGone <- lookupInput store (mkHash 0xd2) 0
        recordTx store (mkHash 0xd2) (mkEntry 5 [50])
        rBack <- lookupInput store (mkHash 0xd2) 0
        rGone `shouldBe` Nothing
        rBack `shouldBe` Just (TxId 5, TxOutId 500, DbLovelace 50)

  describe "readStoreStats" $ do
    it "counts hits, misses, inserts, and deletes" $ withTestUtxoStore $ \store -> do
      recordTx store (mkHash 0xc1) (mkEntry 1 [10])
      recordTx store (mkHash 0xc2) (mkEntry 2 [20])
      _ <- lookupInput store (mkHash 0xc1) 0
      _ <- lookupInput store (mkHash 0xc2) 0
      _ <- lookupInput store (mkHash 0xff) 0
      recordTx store (mkHash 0xc3) (mkEntry 3 [30])
      deleteConsumed store (mkHash 0xc1) 0
      stats <- readStoreStats store
      ssHits    stats `shouldBe` 2
      ssMisses  stats `shouldBe` 1
      ssInserts stats `shouldBe` 3
      ssDeletes stats `shouldBe` 1

  describe "compactUtxoStore" $ do
    -- After a compact the active table handle is a fresh one opened
    -- from the snapshot. The data must survive the swap so subsequent
    -- lookups still hit. Drives the session manually instead of
    -- through 'withTestUtxoStore' because compactUtxoStore takes the
    -- session as a second argument.
    it "preserves live entries across a compaction" $
      withTestLsmSession $ \lsm ->
        bracket (UtxoStore.openUtxoStore lsm) UtxoStore.closeUtxoStore $ \store -> do
          recordTx store (mkHash 0xd1) (mkEntry 7 [70])
          recordTx store (mkHash 0xd2) (mkEntry 8 [80, 81])
          UtxoStore.compactUtxoStore store lsm
          r1 <- lookupInput store (mkHash 0xd1) 0
          r2a <- lookupInput store (mkHash 0xd2) 0
          r2b <- lookupInput store (mkHash 0xd2) 1
          r1  `shouldBe` Just (TxId 7, TxOutId 700, DbLovelace 70)
          r2a `shouldBe` Just (TxId 8, TxOutId 800, DbLovelace 80)
          r2b `shouldBe` Just (TxId 8, TxOutId 801, DbLovelace 81)

    it "preserves deletions across a compaction" $
      withTestLsmSession $ \lsm ->
        bracket (UtxoStore.openUtxoStore lsm) UtxoStore.closeUtxoStore $ \store -> do
          recordTx store (mkHash 0xe2) (mkEntry 4 [40, 41])
          deleteConsumed store (mkHash 0xe2) 0
          UtxoStore.compactUtxoStore store lsm
          rGone <- lookupInput store (mkHash 0xe2) 0
          rLive <- lookupInput store (mkHash 0xe2) 1
          rGone `shouldBe` Nothing
          rLive `shouldBe` Just (TxId 4, TxOutId 401, DbLovelace 41)

    it "is safe to invoke twice in succession" $
      withTestLsmSession $ \lsm ->
        bracket (UtxoStore.openUtxoStore lsm) UtxoStore.closeUtxoStore $ \store -> do
          recordTx store (mkHash 0xe1) (mkEntry 9 [90])
          UtxoStore.compactUtxoStore store lsm
          UtxoStore.compactUtxoStore store lsm
          result <- lookupInput store (mkHash 0xe1) 0
          result `shouldBe` Just (TxId 9, TxOutId 900, DbLovelace 90)
