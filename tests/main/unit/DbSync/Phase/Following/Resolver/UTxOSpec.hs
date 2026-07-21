{-# LANGUAGE OverloadedStrings #-}

-- | Strictness bombs for the buffered Follow resolver's
-- force-on-entry points. Consumed-by pairs queued on the
-- 'WriteBuffer' and entries in the block-local UTxO map outlive the
-- extractor call that created them, so both must be forced at entry.
-- Each test feeds a named bottom and asserts that this exact bomb
-- detonates, pinning force order as well as presence.
module DbSync.Phase.Following.Resolver.UTxOSpec (spec) where

import Cardano.Prelude

import qualified Control.Exception as E
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import Test.Hspec (Spec, describe, errorCall, it, shouldBe, shouldThrow)

import DbSync.Db.Schema.Ids (TxId (..), TxOutId (..))
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Phase.Following.Resolver.Internal (BlockDedupCache (..), newBlockDedupCache)
import DbSync.Phase.Following.Resolver.UTxO
  ( ConsumedTracking (..)
  , recordConsumedBuf
  , recordConsumedConn
  , recordTxOutputsBuf
  )
import DbSync.Phase.Following.WriteBuffer (newWriteBuffer)
import DbSync.Phase.Ingest.UtxoStore (UtxoTxEntry (..))

bomb :: [Char] -> a
bomb = E.throw . E.ErrorCall

spec :: Spec
spec = do
  describe "Phase.Following.Resolver.UTxO recordConsumedBuf" $ do
    it "forces the producer id on entry (bomb)" $ do
      buf <- newWriteBuffer
      recordConsumedBuf buf TrackConsumedBy (bomb "unforced producer id") (TxId 1)
        `shouldThrow` errorCall "unforced producer id"

    it "forces the consumer id on entry (bomb)" $ do
      buf <- newWriteBuffer
      recordConsumedBuf buf TrackConsumedBy (TxOutId 1) (bomb "unforced consumer id")
        `shouldThrow` errorCall "unforced consumer id"

    -- The bangs sit on the function's own arguments, ahead of the
    -- tracking dispatch: a SkipConsumedBy deployment must not
    -- accumulate thunks either.
    it "forces ids even when tracking is off (bomb)" $ do
      buf <- newWriteBuffer
      recordConsumedBuf buf SkipConsumedBy (bomb "unforced producer id") (TxId 1)
        `shouldThrow` errorCall "unforced producer id"

    it "queues evaluated ids without error (positive control)" $ do
      buf <- newWriteBuffer
      recordConsumedBuf buf TrackConsumedBy (TxOutId 1) (TxId 2)

  describe "Phase.Following.Resolver.UTxO recordConsumedConn" $ do
    -- Two live bombs: the test passes only if the id detonates
    -- first, i.e. the ids are forced before the connection is used.
    it "forces both ids before touching the connection (bomb)" $ do
      recordConsumedConn (bomb "connection touched") TrackConsumedBy (bomb "unforced producer id") (TxId 1)
        `shouldThrow` errorCall "unforced producer id"
      recordConsumedConn (bomb "connection touched") TrackConsumedBy (TxOutId 1) (bomb "unforced consumer id")
        `shouldThrow` errorCall "unforced consumer id"

    -- With tracking off the connection must never be demanded: the
    -- call succeeds even though the connection is a bomb.
    it "skip mode does no database work at all" $
      recordConsumedConn (bomb "connection touched") SkipConsumedBy (TxOutId 1) (TxId 2)

  describe "Phase.Following.Resolver.UTxO recordTxOutputsBuf" $ do
    it "forces the tx hash on entry (bomb)" $ do
      cache <- newBlockDedupCache
      recordTxOutputsBuf cache (bomb "unforced hash") (UtxoTxEntry (TxId 1) mempty)
        `shouldThrow` errorCall "unforced hash"

    it "forces the entry on entry (bomb)" $ do
      cache <- newBlockDedupCache
      recordTxOutputsBuf cache "aa" (bomb "unforced entry")
        `shouldThrow` errorCall "unforced entry"

    -- WHNF at entry is enough only because 'UtxoTxEntry' has strict
    -- fields; this pins that the constructor keeps forcing them.
    it "a bottom inside the entry's fields detonates at entry (bomb)" $ do
      cache <- newBlockDedupCache
      recordTxOutputsBuf cache "aa" (UtxoTxEntry (bomb "unforced tx id") mempty)
        `shouldThrow` errorCall "unforced tx id"

    it "stores the entry under the tx hash (positive control)" $ do
      cache <- newBlockDedupCache
      let entry = UtxoTxEntry (TxId 7) (Seq.fromList [(TxOutId 9, DbLovelace 1_000)])
      recordTxOutputsBuf cache "aa" entry
      m <- readIORef (bdcUtxo cache)
      Map.lookup "aa" m `shouldBe` Just entry
