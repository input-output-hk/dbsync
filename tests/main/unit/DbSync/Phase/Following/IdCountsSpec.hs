{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Follow pre-allocation count walker.
--
-- 'countAssignableIds' must tally exactly what the extractor pass
-- later consumes via @assignXxxId@; an undercount exhausts the
-- 'PreAllocatedIds' queues and panics at @popHead@. The fixtures
-- mirror the parser's real invariants for collateral-return outputs:
--
--   * A /valid/ tx exposes its collateral return via
--     'txCollateralOutput' (@Just@); the extractor assigns a
--     @collateral_tx_out@ id for it.
--   * A /failed/ phase-2 tx has 'txCollateralOutput' = @Nothing@ —
--     the parser folds the collateral return into 'txOutputs', so it
--     lands in @tx_out@ and consumes a @tx_out@ id instead.
module DbSync.Phase.Following.IdCountsSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import qualified Data.ByteString as BS

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Types (ScriptPurpose (..))
import DbSync.Extractor (ExtractorDef)
import DbSync.Extractor.ScriptsDatums (scriptsDatumsExtractor)
import DbSync.Parser.Types
  ( BlockEra (..)
  , GenericBlock (..)
  , GenericTx (..)
  , GenericTxIn (..)
  , GenericTxOut (..)
  , GenericTxRedeemer (..)
  )
import DbSync.Phase.Following.IdCounts
  ( IdCounts (..)
  , countAssignableIds
  )

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

spec :: Spec
spec =
  describe "countAssignableIds" $ do
    it "counts one collateral_tx_out id for a valid tx with a collateral return" $ do
      let counts = countAssignableIds withScriptsDatums (blockWith validTxWithCollateralReturn)
      icCollateralTxOutIds counts `shouldBe` 1
      -- The valid tx's single regular output still wants a tx_out id;
      -- the collateral return does not double-count into tx_out.
      icTxOutIds counts `shouldBe` 1

    it "counts no collateral_tx_out id for a failed phase-2 tx" $ do
      let counts = countAssignableIds withScriptsDatums (blockWith failedTxFoldedCollateral)
      icCollateralTxOutIds counts `shouldBe` 0
      -- The folded collateral return is the tx's only surviving output
      -- and is written to tx_out, so it consumes a tx_out id.
      icTxOutIds counts `shouldBe` 1

    it "tallies one tx id per tx" $
      icTxIds (countAssignableIds withScriptsDatums (blockWith validTxWithCollateralReturn))
        `shouldBe` 1

    it "counts redeemer ids only when scripts_datums is enabled" $ do
      let blk = blockWith validTxWithCollateralReturn { txRedeemers = [sampleRedeemer] }
      icRedeemerIds (countAssignableIds withScriptsDatums blk) `shouldBe` 1
      -- Disabled extractor writes no rows; its sequence may not even
      -- exist, so no ids may be drawn from it.
      icRedeemerIds (countAssignableIds [] blk) `shouldBe` 0

    it "counts no redeemer ids for a failed phase-2 tx" $ do
      let blk = blockWith failedTxFoldedCollateral { txRedeemers = [sampleRedeemer] }
      icRedeemerIds (countAssignableIds withScriptsDatums blk) `shouldBe` 0

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

blockWith :: GenericTx -> GenericBlock
blockWith tx = emptyBlock { blkTxs = [tx] }

withScriptsDatums :: [ExtractorDef]
withScriptsDatums = [scriptsDatumsExtractor]

sampleRedeemer :: GenericTxRedeemer
sampleRedeemer = GenericTxRedeemer
  { gtrUnitMem    = 1000
  , gtrUnitSteps  = 250_000
  , gtrPurpose    = Spend
  , gtrIndex      = 0
  , gtrScriptHash = Nothing
  , gtrDataHash   = BS.replicate 32 0xab
  , gtrDataBytes  = "d87980"
  , gtrDataValue  = Nothing
  }

-- | Babbage+ valid tx that declared a collateral-return output. The
-- parser keeps the real output in 'txOutputs' and the collateral
-- return in 'txCollateralOutput'.
validTxWithCollateralReturn :: GenericTx
validTxWithCollateralReturn = baseTx
  { txValidContract    = True
  , txOutputs          = [mkOut 0 5_000_000]
  , txCollateralInputs = [GenericTxIn (BS.replicate 32 0xcc) 0 Nothing]
  , txCollateralOutput = Just (mkOut 1 4_000_000)
  }

-- | Failed phase-2 tx as the parser emits it: the collateral return
-- has been folded into 'txOutputs', 'txCollateralOutput' is 'Nothing'.
failedTxFoldedCollateral :: GenericTx
failedTxFoldedCollateral = baseTx
  { txValidContract    = False
  , txOutputs          = [mkOut 0 4_000_000]
  , txCollateralInputs = [GenericTxIn (BS.replicate 32 0xcc) 0 Nothing]
  , txCollateralOutput = Nothing
  }

mkOut :: Word16 -> Word64 -> GenericTxOut
mkOut idx value = GenericTxOut
  { txOutIndex       = idx
  , txOutAddressRaw  = BS.replicate 57 0x00
  , txOutValue       = value
  , txOutDataHash    = Nothing
  , txOutInlineDatum = Nothing
  , txOutRefScript   = Nothing
  , txOutMultiAssets = []
  }

baseTx :: GenericTx
baseTx = GenericTx
  { txHash             = BS.replicate 32 0xab
  , txBlockIndex       = 0
  , txSize             = 300
  , txFee              = 174_000
  , txOutSum           = 0
  , txValidContract    = True
  , txScriptSize       = 0
  , txTreasuryDonation = 0
  , txInvalidBefore    = Nothing
  , txInvalidHereafter = Nothing
  , txInputs           = []
  , txOutputs          = []
  , txCollateralInputs = []
  , txReferenceInputs  = []
  , txCollateralOutput = Nothing
  , txCertificates     = []
  , txWithdrawals      = []
  , txMetadata         = Nothing
  , txMint             = []
  , txCborRaw          = Nothing
  , txScripts          = []
  , txDatums           = []
  , txRedeemers        = []
  , txExtraKeyWitnesses = []
  , txParamProposal    = []
  , txProposals        = []
  , txVotingProcedures = []
  , txVotingAnchors    = []
  }

emptyBlock :: GenericBlock
emptyBlock = GenericBlock
  { blkEra          = Babbage
  , blkHash         = BS.replicate 32 0xff
  , blkPreviousHash = BS.replicate 32 0xee
  , blkSlotNo       = SlotNo 42
  , blkBlockNo      = BlockNo 1
  , blkEpochNo      = EpochNo 5
  , blkEpochSlotNo  = 100
  , blkSize         = 512
  , blkTime         = sampleTime
  , blkSlotLeader   = BS.replicate 28 0xab
  , blkProtoMajor   = 9
  , blkProtoMinor   = 0
  , blkVrfKey       = Just "vrf_vk1test"
  , blkOpCert       = Just (BS.replicate 32 0)
  , blkOpCertCounter = Just 0
  , blkIsEBB        = False
  , blkTxs          = []
  }

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2021 1 1) (secondsToDiffTime 0)
