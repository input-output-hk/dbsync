{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the off_chain_votes extractor's per-block pass:
-- 'enqueueVoteMetaFetch' is invoked once per anchor on
-- proposals, votes, DRep certs, and committee resignations; skipped
-- otherwise.
module DbSync.Extractor.OffChainVotesSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.IORef (atomicModifyIORef', newIORef, readIORef)

import qualified Data.ByteString as BS

import Test.Hspec (Spec, describe, it, shouldBe)

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import DbSync.Db.Types (AnchorType (..), Vote (..))
import DbSync.Parser.Types
  ( AnchorData (..)
  , BlockEra (..)
  , CertAction (..)
  , CredHash (..)
  , GenericBlock (..)
  , GenericGovAction (..)
  , GenericGovActionProposal (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  , GenericVoter (..)
  , GenericVotingProcedure (..)
  , GovActionRef (..)
  )

import DbSync.Extractor (freshExtractState)
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.OffChainVotes (offChainVotesExtractor)
import DbSync.Extractor.Pipeline (processBlock)
import DbSync.Phase.Ingest.Resolver (mkIngestResolver)
import DbSync.Resolver (IdResolver (..))
import DbSync.Test.Lsm (withTestIngestStores)
import DbSync.Test.PipelineEnv (mkTestPipelineEnv)
import DbSync.Test.Writer (emptyTestWriterState, mkTestWriter)
import DbSync.Worker.OffChain.Types (VotingAnchorRef (..))
import DbSync.Worker.TxOut.AddressBuffer (newAddressBufferRef)

spec :: Spec
spec =
  describe "off_chain_votes extractor" $ do
    it "enqueues a fetch for a governance proposal anchor" $ do
      refs <- runExtractor (blockWithProposal sampleProposalAnchor infoProposal)
      length refs `shouldBe` 1
      let ref = headDef (panic "no enqueue") refs
      varUrl ref        `shouldBe` adUrl sampleProposalAnchor
      varMetaHash ref   `shouldBe` adHash sampleProposalAnchor
      varAnchorType ref `shouldBe` GovActionAnchor

    it "enqueues both the proposal and constitution anchor for GovNewConstitution" $ do
      refs <- runExtractor
        ( blockWithProposal sampleProposalAnchor
            (newConstitutionProposal sampleConstitutionAnchor)
        )
      map varAnchorType refs `shouldBe` [GovActionAnchor, ConstitutionAnchor]
      map varUrl refs        `shouldBe`
        [ adUrl sampleProposalAnchor
        , adUrl sampleConstitutionAnchor
        ]

    it "enqueues a vote anchor" $ do
      refs <- runExtractor (blockWithVote (Just sampleVoteAnchor))
      length refs `shouldBe` 1
      let ref = headDef (panic "no enqueue") refs
      varAnchorType ref `shouldBe` VoteAnchor
      varUrl ref        `shouldBe` adUrl sampleVoteAnchor

    it "does not enqueue when a vote carries no anchor" $ do
      refs <- runExtractor (blockWithVote Nothing)
      length refs `shouldBe` 0

    it "enqueues a DRep registration anchor" $ do
      refs <- runExtractor
        ( blockWithCert
            (CertDRepRegistration (CredHash drepCred False) 500_000_000 (Just sampleDrepAnchor))
        )
      length refs `shouldBe` 1
      varAnchorType (headDef (panic "no enqueue") refs) `shouldBe` DrepAnchor

    it "enqueues a DRep update anchor" $ do
      refs <- runExtractor
        ( blockWithCert (CertDRepUpdate (CredHash drepCred False) (Just sampleDrepAnchor)) )
      length refs `shouldBe` 1
      varAnchorType (headDef (panic "no enqueue") refs) `shouldBe` DrepAnchor

    it "enqueues a committee resignation anchor" $ do
      refs <- runExtractor
        ( blockWithCert
            (CertCommitteeResign (CredHash committeeKey False) (Just sampleCommitteeAnchor))
        )
      length refs `shouldBe` 1
      varAnchorType (headDef (panic "no enqueue") refs)
        `shouldBe` CommitteeDeRegAnchor

    it "skips phase-2 failed txs" $ do
      refs <- runExtractor
        ((blockWithProposal sampleProposalAnchor infoProposal)
           { blkTxs = map (markFailed . headTx) [blockWithProposal sampleProposalAnchor infoProposal] })
      length refs `shouldBe` 0

    it "does not enqueue when nothing matches" $ do
      refs <- runExtractor (shelleyEmptyBlock 1)
      length refs `shouldBe` 0

-- ---------------------------------------------------------------------------
-- Harness
-- ---------------------------------------------------------------------------

-- | Run the off_chain_votes extractor against a block, capturing every
-- 'enqueueVoteMetaFetch' call in order.
runExtractor :: GenericBlock -> IO [VotingAnchorRef]
runExtractor block = withTestIngestStores $ \utxoStore dedupStores -> do
  stRef     <- newIORef freshExtractState
  addrBuf   <- newAddressBufferRef
  wrRef     <- newIORef emptyTestWriterState
  capturedR <- newIORef []
  let baseResolver = mkIngestResolver stRef dedupStores addrBuf utxoStore Nothing
      resolver = baseResolver
        { enqueueVoteMetaFetch = \r ->
            atomicModifyIORef' capturedR (\rs -> (rs ++ [r], ()))
        }
      env = mkTestPipelineEnv resolver (mkTestWriter wrRef)
                              [coreExtractor, offChainVotesExtractor]
  runReaderT (processBlock block) env
  readIORef capturedR

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

sampleProposalAnchor :: AnchorData
sampleProposalAnchor = AnchorData
  { adUrl  = "https://example.test/proposal.json"
  , adHash = BS.replicate 32 0x11
  }

sampleConstitutionAnchor :: AnchorData
sampleConstitutionAnchor = AnchorData
  { adUrl  = "https://example.test/constitution.json"
  , adHash = BS.replicate 32 0x22
  }

sampleVoteAnchor :: AnchorData
sampleVoteAnchor = AnchorData
  { adUrl  = "https://example.test/vote.json"
  , adHash = BS.replicate 32 0x33
  }

sampleDrepAnchor :: AnchorData
sampleDrepAnchor = AnchorData
  { adUrl  = "https://example.test/drep.json"
  , adHash = BS.replicate 32 0x44
  }

sampleCommitteeAnchor :: AnchorData
sampleCommitteeAnchor = AnchorData
  { adUrl  = "https://example.test/committee.json"
  , adHash = BS.replicate 32 0x55
  }

returnAddr :: ByteString
returnAddr = BS.cons 0xE1 (BS.replicate 28 0xab)

drepCred :: ByteString
drepCred = BS.replicate 28 0xa1

committeeKey :: ByteString
committeeKey = BS.replicate 28 0xb2

stakePoolKey :: ByteString
stakePoolKey = BS.replicate 28 0xc3

infoProposal :: GenericGovAction
infoProposal = GovInfoAction

newConstitutionProposal :: AnchorData -> GenericGovAction
newConstitutionProposal ca = GovNewConstitution Nothing ca Nothing

sampleProposal :: AnchorData -> GenericGovAction -> GenericGovActionProposal
sampleProposal anchor action = GenericGovActionProposal
  { ggapTxIndex         = 0
  , ggapReturnAddrCred  = CredHash returnAddr False
  , ggapDeposit         = 100_000_000_000
  , ggapAnchor          = anchor
  , ggapAction          = action
  , ggapDescriptionJson = "{}"
  }

sampleVote :: Maybe AnchorData -> GenericVotingProcedure
sampleVote mAnchor = GenericVotingProcedure
  { gvpTxIndex     = 0
  , gvpVoter       = VoterStakePool stakePoolKey
  , gvpGovActionId = GovActionRef (BS.replicate 32 0xee) 0
  , gvpVote        = VoteYes
  , gvpAnchor      = mAnchor
  , gvpRedeemerIx  = Nothing
  }

emptyTx :: GenericTx
emptyTx = GenericTx
  { txHash             = BS.replicate 32 0x00
  , txBlockIndex       = 0
  , txSize             = 500
  , txFee              = 200_000
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

markFailed :: GenericTx -> GenericTx
markFailed tx = tx { txValidContract = False }

headTx :: GenericBlock -> GenericTx
headTx blk = case blkTxs blk of
  (t : _) -> t
  []      -> panic "headTx: empty block"

shelleyEmptyBlock :: Word64 -> GenericBlock
shelleyEmptyBlock epoch = GenericBlock
  { blkEra          = Conway
  , blkHash         = BS.replicate 32 (fromIntegral (epoch `mod` 256))
  , blkPreviousHash = ""
  , blkSlotNo       = SlotNo (epoch * 432_000)
  , blkBlockNo      = BlockNo epoch
  , blkEpochNo      = EpochNo epoch
  , blkEpochSlotNo  = 0
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
sampleTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)

blockWithProposal :: AnchorData -> GenericGovAction -> GenericBlock
blockWithProposal anchor action = (shelleyEmptyBlock 5)
  { blkTxs = [emptyTx { txProposals = [sampleProposal anchor action] }]
  }

blockWithVote :: Maybe AnchorData -> GenericBlock
blockWithVote mAnchor = (shelleyEmptyBlock 5)
  { blkTxs = [emptyTx { txVotingProcedures = [sampleVote mAnchor] }]
  }

blockWithCert :: CertAction -> GenericBlock
blockWithCert action = (shelleyEmptyBlock 5)
  { blkTxs =
      [ emptyTx
          { txCertificates =
              [GenericTxCertificate { txCertIndex = 0, txCertAction = action, txCertRedeemerIx = Nothing }]
          }
      ]
  }
