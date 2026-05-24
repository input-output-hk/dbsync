{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for 'DbSync.Phase.Ingest.Consumer'.
module DbSync.Phase.Ingest.ConsumerSpec (spec) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import Control.Concurrent.STM (newTVarIO)
import qualified Control.Concurrent.STM as STM
import Data.IORef (newIORef, writeIORef)

import Cardano.Slotting.Block (BlockNo (..))

import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

import DbSync.Phase.Ingest.Consumer
  ( ingestRollbackPanicMessage
  , renderBoundaryPercent
  , rollbackBoundaryReached
  )

import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  rollbackBoundarySpec
  renderBoundaryPercentSpec
  ingestRollbackPanicSpec

rollbackBoundarySpec :: Spec
rollbackBoundarySpec = describe "DbSync.Phase.Ingest.Consumer.rollbackBoundaryReached" $ do
  it "returns False when no block has been processed" $ do
    lastRef     <- newIORef Nothing
    boundaryVar <- newTVarIO (Just (BlockNo 200))
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` False

  it "returns False when the receiver hasn't seen a tip yet" $ do
    lastRef     <- newIORef (Just (50, 50, BS.empty))
    boundaryVar <- newTVarIO Nothing
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` False

  it "returns False when neither ref is set" $ do
    lastRef     <- newIORef Nothing
    boundaryVar <- newTVarIO Nothing
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` False

  it "returns False when the last block is below the boundary" $ do
    lastRef     <- newIORef (Just (1_000, 100, BS.empty))
    boundaryVar <- newTVarIO (Just (BlockNo 200))
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` False

  it "returns True when the last block equals the boundary" $ do
    lastRef     <- newIORef (Just (1_000, 200, BS.empty))
    boundaryVar <- newTVarIO (Just (BlockNo 200))
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` True

  it "returns True when the last block is past the boundary" $ do
    lastRef     <- newIORef (Just (1_000, 250, BS.empty))
    boundaryVar <- newTVarIO (Just (BlockNo 200))
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` True

  it "reflects updates to either ref" $ do
    lastRef     <- newIORef (Just (1_000, 100, BS.empty))
    boundaryVar <- newTVarIO (Just (BlockNo 200))
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` False
    -- New boundary arrives; same last block still in front of it.
    STM.atomically $ STM.writeTVar boundaryVar (Just (BlockNo 90))
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` True
    -- Boundary moves back; last block still ahead.
    writeIORef lastRef (Just (1_000, 89, BS.empty))
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` False

renderBoundaryPercentSpec :: Spec
renderBoundaryPercentSpec = describe "DbSync.Phase.Ingest.Consumer.renderBoundaryPercent" $ do
  let k = 2160  -- mainnet security parameter

  it "renders empty when the rollback boundary is not yet known" $
    renderBoundaryPercent Nothing k (Just 100) `shouldBe` ""

  it "renders empty when no block has been processed yet" $
    renderBoundaryPercent (Just (BlockNo 9_000_000)) k Nothing `shouldBe` ""

  it "renders empty when both inputs are missing" $
    renderBoundaryPercent Nothing k Nothing `shouldBe` ""

  it "renders 0% at genesis with a real tip" $
    renderBoundaryPercent (Just (BlockNo 9_000_000)) k (Just 0)
      `shouldBe` " | [0.00%]"

  it "renders ~50% halfway to tip" $
    -- tip = 9_000_000 + 2160 = 9_002_160; half = 4_501_080
    renderBoundaryPercent (Just (BlockNo 9_000_000)) k (Just 4_501_080)
      `shouldBe` " | [50.00%]"

  it "approaches 100% just below tip but never reaches it during Ingest" $ do
    -- At the rollback boundary we exit Ingest; pct = boundary / (boundary+k)
    let pct = renderBoundaryPercent (Just (BlockNo 9_000_000)) k (Just 9_000_000)
    pct `shouldBe` " | [99.98%]"

  it "clamps to 100% when current exceeds tip" $
    -- Defensive: receiver might publish a stale boundary while consumer races ahead.
    renderBoundaryPercent (Just (BlockNo 100)) k (Just 999_999)
      `shouldBe` " | [100.00%]"

  it "renders 100% when current equals tip exactly" $
    renderBoundaryPercent (Just (BlockNo 1000)) k (Just (1000 + k))
      `shouldBe` " | [100.00%]"

ingestRollbackPanicSpec :: Spec
ingestRollbackPanicSpec = describe "DbSync.Phase.Ingest.Consumer.ingestRollbackPanicMessage" $ do
  it "names the offending rollback point in the panic text" $ do
    let msg = ingestRollbackPanicMessage ("some-rollback-point" :: Text)
    T.isInfixOf "IngestChainHistory" msg `shouldBe` True
    T.isInfixOf "some-rollback-point"  msg `shouldBe` True
    T.isInfixOf "k-safety violation"   msg `shouldBe` True
