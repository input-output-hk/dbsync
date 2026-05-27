{-# LANGUAGE OverloadedStrings #-}

module DbSync.Phase.Ingest.DedupStoreSpec (spec) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS

import qualified Database.LSMTree as LSMTree

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Phase.Ingest.DedupStore
  ( DedupStores (..)
  , closeDedupStore
  , compactDedupStore
  , insertExisting
  , lookupOrInsert
  , openDedupStore
  , sizeApprox
  )
import DbSync.Test.Lsm (withTestDedupStores, withTestLsmSession)

-- | 28-byte key, the size dedup-store keys are normalised to.
mkKey :: Word8 -> ShortByteString
mkKey w = SBS.toShort (BS.pack (replicate 28 w))

spec :: Spec
spec = do
  describe "lookupOrInsert" $ do
    it "returns (1, True) on the first insert" $
      withTestDedupStores $ \stores -> do
        result <- lookupOrInsert (mkKey 0x11) (dstPoolHash stores)
        result `shouldBe` (1, True)

    it "returns (existingId, False) on a rehit" $
      withTestDedupStores $ \stores -> do
        let store = dstPoolHash stores
        _      <- lookupOrInsert (mkKey 0x11) store
        rehit  <- lookupOrInsert (mkKey 0x11) store
        rehit `shouldBe` (1, False)

    it "gives distinct keys distinct ids and stable ids on repeats" $
      withTestDedupStores $ \stores -> do
        let store = dstSlotLeader stores
        a1 <- lookupOrInsert (mkKey 0xa1) store
        a2 <- lookupOrInsert (mkKey 0xa2) store
        a3 <- lookupOrInsert (mkKey 0xa3) store
        a1Again <- lookupOrInsert (mkKey 0xa1) store
        a2Again <- lookupOrInsert (mkKey 0xa2) store
        a1 `shouldBe` (1, True)
        a2 `shouldBe` (2, True)
        a3 `shouldBe` (3, True)
        a1Again `shouldBe` (1, False)
        a2Again `shouldBe` (2, False)

  describe "insertExisting" $ do
    it "raises the counter past the imported id" $
      withTestDedupStores $ \stores -> do
        let store = dstMultiAsset stores
        insertExisting (mkKey 0x42) 99 store
        nextAllocated <- lookupOrInsert (mkKey 0x43) store
        nextAllocated `shouldBe` (100, True)

    it "is observable as a hit on subsequent lookupOrInsert" $
      withTestDedupStores $ \stores -> do
        let store = dstMultiAsset stores
        insertExisting (mkKey 0x42) 7 store
        result <- lookupOrInsert (mkKey 0x42) store
        result `shouldBe` (7, False)

    it "leaves the counter alone if the imported id is below the next allocation" $
      -- Sequence: allocate 1, allocate 2, then insertExisting a tiny
      -- id (0). The next fresh allocation must still be 3, not
      -- 1 (regression).
      withTestDedupStores $ \stores -> do
        let store = dstScriptHash stores
        _ <- lookupOrInsert (mkKey 0x01) store
        _ <- lookupOrInsert (mkKey 0x02) store
        insertExisting (mkKey 0x00) 0 store
        next <- lookupOrInsert (mkKey 0x03) store
        next `shouldBe` (3, True)

  describe "sizeApprox" $ do
    it "is 0 on an empty store" $
      withTestDedupStores $ \stores -> do
        n <- sizeApprox (dstPoolHash stores)
        n `shouldBe` 0

    it "tracks the next-id counter as lookupOrInsert allocates" $
      withTestDedupStores $ \stores -> do
        let store = dstPoolHash stores
        _ <- lookupOrInsert (mkKey 0x10) store
        _ <- lookupOrInsert (mkKey 0x20) store
        _ <- lookupOrInsert (mkKey 0x30) store
        sizeApprox store >>= (`shouldBe` 3)

    it "reports max(existingId) after an insertExisting" $
      withTestDedupStores $ \stores -> do
        let store = dstStakeAddress stores
        insertExisting (mkKey 0x55) 50 store
        sizeApprox store >>= (`shouldBe` 50)

  describe "store isolation across the five labels" $ do
    -- Same key in two different stores must allocate independently
    -- and not collide. Pins the "five distinct snapshot labels and
    -- names" invariant — if two stores shared a label/name they'd
    -- read each other's writes.
    it "different stores in the same session don't see each other's keys" $
      withTestDedupStores $ \stores -> do
        let key = mkKey 0x77
        a <- lookupOrInsert key (dstPoolHash stores)
        b <- lookupOrInsert key (dstStakeAddress stores)
        c <- lookupOrInsert key (dstSlotLeader stores)
        d <- lookupOrInsert key (dstMultiAsset stores)
        e <- lookupOrInsert key (dstScriptHash stores)
        a `shouldBe` (1, True)
        b `shouldBe` (1, True)
        c `shouldBe` (1, True)
        d `shouldBe` (1, True)
        e `shouldBe` (1, True)
        -- Rehit each in turn.
        lookupOrInsert key (dstPoolHash stores)     >>= (`shouldBe` (1, False))
        lookupOrInsert key (dstStakeAddress stores) >>= (`shouldBe` (1, False))
        lookupOrInsert key (dstSlotLeader stores)   >>= (`shouldBe` (1, False))
        lookupOrInsert key (dstMultiAsset stores)   >>= (`shouldBe` (1, False))
        lookupOrInsert key (dstScriptHash stores)   >>= (`shouldBe` (1, False))

  describe "compactDedupStore" $ do
    -- 'compactDedupStore' delete-then-save-then-reopens the active
    -- table. Inserts must survive the swap so subsequent lookups
    -- still hit. Drives the session manually because
    -- 'withTestDedupStores' doesn't expose the session.
    it "preserves entries across a compaction" $
      withTestLsmSession $ \lsm ->
        bracket (openDedupStore lsm testLabel testName) closeDedupStore $ \store -> do
          _ <- lookupOrInsert (mkKey 0x01) store
          _ <- lookupOrInsert (mkKey 0x02) store
          compactDedupStore store lsm
          a <- lookupOrInsert (mkKey 0x01) store
          b <- lookupOrInsert (mkKey 0x02) store
          a `shouldBe` (1, False)
          b `shouldBe` (2, False)

    it "is safe to invoke twice in succession" $
      withTestLsmSession $ \lsm ->
        bracket (openDedupStore lsm testLabel testName) closeDedupStore $ \store -> do
          _ <- lookupOrInsert (mkKey 0x99) store
          compactDedupStore store lsm
          compactDedupStore store lsm
          result <- lookupOrInsert (mkKey 0x99) store
          result `shouldBe` (1, False)

  describe "restart-resume from snapshot" $ do
    -- Close the active table, then open a fresh one against the
    -- same snapshot label/name — production restart-resume path.
    -- The PG rebuild step (which raises the counter) is not run
    -- here; the counter restarts at 1 on the reopened store, but
    -- existing keys still resolve to their saved ids.
    it "open-after-compact-then-close re-restores the saved entries" $
      withTestLsmSession $ \lsm -> do
        bracket (openDedupStore lsm testLabel testName) closeDedupStore $ \store -> do
          _ <- lookupOrInsert (mkKey 0xa1) store
          _ <- lookupOrInsert (mkKey 0xa2) store
          compactDedupStore store lsm

        bracket (openDedupStore lsm testLabel testName) closeDedupStore $ \store -> do
          a <- lookupOrInsert (mkKey 0xa1) store
          b <- lookupOrInsert (mkKey 0xa2) store
          a `shouldBe` (1, False)
          b `shouldBe` (2, False)

-- ---------------------------------------------------------------------------
-- Internal fixtures
-- ---------------------------------------------------------------------------

-- | Test-only label/name. Any non-empty string works; pick something
-- that doesn't clash with the production ones used by 'newStores' so
-- a buggy implementation that ignores the label/name shows up here.
testLabel :: LSMTree.SnapshotLabel
testLabel = LSMTree.SnapshotLabel "dedup-spec"

testName :: LSMTree.SnapshotName
testName = LSMTree.toSnapshotName "dedup-spec-current"
