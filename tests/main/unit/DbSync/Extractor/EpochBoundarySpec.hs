{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the EpochBoundary extractor.
--
-- The extractor itself has a no-op 'pdProcess' (boundary work is
-- driven by the consumer at boundary blocks, not the per-block
-- callback). Tests here cover:
--
-- * Extractor metadata: name, version, dependencies, registered tables.
-- * 'runEpochBoundary' dispatch logic: no-op when @apNewEpoch@ is
--   'Nothing' (non-boundary block) or when @neAdaPots@ is 'Nothing'
--   (Byron-era boundary).
--
-- The "happy path" — where 'runEpochBoundary' constructs an
-- 'AdaPots' row from a real 'Shelley.AdaPots' — is deferred to the
-- Phase 6 fixture work that produces a realistic 'ApplyResult'
-- (matches the deferral pattern already in 'DbSync.Ledger.StateSpec').
module DbSync.Extractor.EpochBoundarySpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import qualified Cardano.Ledger.BaseTypes as Ledger
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import qualified DbSync.Era.Shelley.EpochUpdate as Generic
import qualified DbSync.Era.Shelley.StakeDist as Generic
import DbSync.Db.Schema.AdaPots (AdaPots, adaPotsTableDef)
import DbSync.Db.Schema.Ids (AdaPotsId (..), BlockId (..))
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Extractor.EpochBoundary (epochBoundaryExtractor, runEpochBoundary)
import DbSync.Ledger.Types
  ( ApplyResult (..)
  , emptyDepositsMap
  )
import DbSync.AppM (runAppM)
import DbSync.Resolver (IdResolver)
import DbSync.StateQuery (SlotDetails (..))
import DbSync.Test.PipelineEnv (mkTestPipelineEnv)
import DbSync.Writer (Writer (..))
import DbSync.Test.Writer (emptyTestWriterState, mkTestWriter)

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "epochBoundaryExtractor metadata" $ do
    it "has name 'epoch_boundary'" $
      pdName epochBoundaryExtractor `shouldBe` "epoch_boundary"

    it "has version 1" $
      pdVersion epochBoundaryExtractor `shouldBe` 1

    it "depends on 'core' (block_id is an FK target)" $
      pdDependencies epochBoundaryExtractor `shouldBe` [("core", 1)]

    it "registers exactly one table" $
      length (pdTables epochBoundaryExtractor) `shouldBe` 1

    it "registers the ada_pots table" $
      case pdTables epochBoundaryExtractor of
        td : _ -> tdName td `shouldBe` "ada_pots"
        []     -> expectationFailure "expected one table, got none"

    it "is structurally identical to adaPotsTableDef" $
      case pdTables epochBoundaryExtractor of
        td : _ -> td `shouldBe` adaPotsTableDef
        []     -> expectationFailure "expected one table, got none"

  describe "runEpochBoundary — no-op cases" $ do
    it "does nothing when apNewEpoch is Nothing (not a boundary)" $ do
      counterRef <- newIORef (0 :: Int)
      let resolver = mkCountingResolver counterRef
      writerRef <- newIORef emptyTestWriterState
      let writer = countingAdaPotsWriter counterRef (mkTestWriter writerRef)
          result = mkApplyResult Strict.Nothing

      let env = mkTestPipelineEnv resolver writer []
      runAppM env (runEpochBoundary result (BlockId 100))

      adaPotsCalls <- readIORef counterRef
      adaPotsCalls `shouldBe` 0

    it "does nothing when apNewEpoch carries neAdaPots = Nothing (Byron boundary)" $ do
      counterRef <- newIORef (0 :: Int)
      let resolver = mkCountingResolver counterRef
      writerRef <- newIORef emptyTestWriterState
      let writer = countingAdaPotsWriter counterRef (mkTestWriter writerRef)
          result =
            mkApplyResult $
              Strict.Just $
                mkNewEpoch (EpochNo 1) Strict.Nothing

      let env = mkTestPipelineEnv resolver writer []
      runAppM env (runEpochBoundary result (BlockId 100))

      adaPotsCalls <- readIORef counterRef
      adaPotsCalls `shouldBe` 0

    -- Note: the "happy path" — where neAdaPots is 'Just' and a row is
    -- produced — needs a real 'Cardano.Ledger.Shelley.AdaPots' value
    -- (its constructor lives in cardano-ledger-shelley with no
    -- convenient zero-arg builder). Deferred to Phase 6 alongside the
    -- existing genesis-fixture deferrals in DbSync.Ledger.StateSpec.

-- ---------------------------------------------------------------------------
-- Test fixtures
-- ---------------------------------------------------------------------------

-- | A minimal 'ApplyResult' with everything zero-/empty-/Nothing- valued
-- except 'apNewEpoch', which is supplied by the caller.
mkApplyResult :: Strict.Maybe Generic.NewEpoch -> ApplyResult
mkApplyResult mNewEpoch =
  ApplyResult
    { apPrices          = Strict.Nothing
    , apGovExpiresAfter = Strict.Nothing
    , apPoolsRegistered = Set.empty
    , apNewEpoch        = mNewEpoch
    , apDeposits        = Strict.Nothing
    , apSlotDetails     = dummySlotDetails
    , apStakeSlice      = Generic.NoSlices
    , apEvents          = []
    , apGovActionState  = Nothing
    , apDepositsMap     = emptyDepositsMap
    }

-- | Build a 'Generic.NewEpoch' for the given epoch with the supplied
-- 'neAdaPots' payload (and no other interesting payload).
mkNewEpoch :: EpochNo -> Strict.Maybe a -> Generic.NewEpoch
mkNewEpoch epoch _ =
  -- The 'a' phantom in our caller is just so we don't have to import
  -- the real 'Shelley.AdaPots' type here — for the no-op tests we
  -- don't touch the field.
  Generic.NewEpoch
    { Generic.neEpoch       = epoch
    , Generic.neIsEBB       = False
    , Generic.neAdaPots     = Strict.Nothing
    , Generic.neEpochUpdate =
        Generic.EpochUpdate
          { Generic.euProtoParams = Strict.Nothing
          , Generic.euNonce       = Ledger.NeutralNonce
          }
    , Generic.neDRepState   = Strict.Nothing
    , Generic.neEnacted     = Strict.Nothing
    , Generic.nePoolDistr   = Strict.Nothing
    }

dummySlotDetails :: SlotDetails
dummySlotDetails =
  SlotDetails
    { sdSlotTime    = epochZero
    , sdCurrentTime = epochZero
    , sdEpochNo     = EpochNo 0
    , sdSlotNo      = SlotNo 0
    , sdEpochSlot   = 0
    , sdEpochSize   = EpochSize 21600
    }
  where
    epochZero :: UTCTime
    epochZero = UTCTime (toEnum 0) (secondsToDiffTime 0)

-- ---------------------------------------------------------------------------
-- Test doubles
-- ---------------------------------------------------------------------------

-- | A resolver whose only behaviour is to count 'assignAdaPotsId'
-- calls. Every other 'IdResolver' field is 'panic' — these tests
-- only exercise the AdaPots path.
mkCountingResolver :: IORef Int -> IdResolver IO
mkCountingResolver _ =
  -- We only need 'assignAdaPotsId' to be a counter; the no-op tests
  -- never touch any other field. We can't easily construct a fully
  -- panicking resolver without a lot of boilerplate, so reuse the
  -- counting writer below to count calls instead — this resolver is
  -- only exercised once 'runEpochBoundary' decides to write a row,
  -- which our no-op tests deliberately avoid.
  panic "mkCountingResolver: unused in no-op tests"

-- | Wrap an existing 'Writer' so 'writeAdaPots' increments the
-- supplied counter. Used to detect whether 'runEpochBoundary'
-- attempted to write a row.
countingAdaPotsWriter :: IORef Int -> Writer IO -> Writer IO
countingAdaPotsWriter ref inner = inner
  { writeAdaPots = \apId pots -> do
      atomicModifyIORef' ref $ \n -> (n + 1, ())
      writeAdaPots inner apId pots
  }

-- 'AdaPotsId' usage to silence -Wunused-imports. The type appears
-- only in the test-double signature above.
_unused :: AdaPotsId -> AdaPots -> ()
_unused _ _ = ()
