{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Governance extractor.
--
-- Cert pass, proposal pass, vote pass, anchor dedup, and the
-- ledger-derived boundary handler.
module DbSync.Extractor.GovernanceSpec (spec) where

import Cardano.Prelude

import Cardano.Ledger.Alonzo.Scripts (CostModel, mkCostModel)
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Plutus.Language (Language (PlutusV2))
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef)
import Data.List ((!!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import qualified DbSync.Db.Schema.Governance as SG
import DbSync.Db.Schema.Ids (BlockId (..))
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Types
  ( Vote (..)
  , VoterRole (..)
  )
import DbSync.Extractor (ExtractorDef (..), freshExtractState)
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.Governance (governanceExtractor, runGovernanceBoundary)
import DbSync.Extractor.Pipeline (processBlock)
import qualified DbSync.Worker.Ledger.EpochUpdate as Generic
import qualified DbSync.Worker.Ledger.StakeDist as Generic
import DbSync.Worker.Ledger.Types (ApplyResult (..), BoundaryApplyData (..), emptyDepositsMap)
import DbSync.Parser.ParamProposal (GenericParamProposal (..))
import DbSync.Parser.Types
  ( AnchorData (..)
  , BlockEra (..)
  , CertAction (..)
  , CredHash (..)
  , DRepIdent (..)
  , GenericBlock (..)
  , GenericGovAction (..)
  , GenericGovActionProposal (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  , GenericVoter (..)
  , GenericVotingProcedure (..)
  , GovActionRef (..)
  )
import DbSync.Phase.Ingest.Resolver (mkIngestResolver)
import DbSync.AppM (runAppM)
import DbSync.StateQuery (SlotDetails (..))
import DbSync.Test.Lsm (withTestIngestStores)
import DbSync.Test.PipelineEnv (mkTestPipelineEnv)
import DbSync.Test.Writer (TestWriterState (..), emptyTestWriterState, mkTestWriter)
import DbSync.Worker.TxOut.AddressBuffer (newAddressBufferRef)

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "governanceExtractor metadata" $ do
    it "has name 'governance'" $
      pdName governanceExtractor `shouldBe` "governance"

    it "registers 16 tables" $
      length (pdTables governanceExtractor) `shouldBe` 16

    it "registers the expected table names" $
      map tdName (pdTables governanceExtractor) `shouldBe`
        [ "drep_hash", "drep_registration", "drep_distr"
        , "delegation_vote"
        , "gov_action_proposal", "voting_procedure", "voting_anchor"
        , "constitution"
        , "committee", "committee_hash", "committee_member"
        , "committee_registration", "committee_de_registration"
        , "param_proposal", "treasury_withdrawal", "event_info"
        ]

  describe "DRep cert pass" $ do
    it "registration writes drep_hash + drep_registration" $ do
      written <- runGovernance [txWithCerts [drepRegCert drepCredA 500_000_000 Nothing]]
      length (twDrepHashes written) `shouldBe` 1
      length (twDrepRegistrations written) `shouldBe` 1
      let dr = headDef (panic "expected drep_registration") (twDrepRegistrations written)
      SG.drepRegistrationDeposit dr `shouldBe` Just 500_000_000

    it "deregistration records a negative deposit" $ do
      written <- runGovernance
        [txWithCerts
          [ drepRegCert drepCredA 500_000_000 Nothing
          , drepDeregCert drepCredA 500_000_000
          ]
        ]
      length (twDrepRegistrations written) `shouldBe` 2
      let regs = twDrepRegistrations written
      SG.drepRegistrationDeposit (regs !! 0) `shouldBe` Just 500_000_000
      SG.drepRegistrationDeposit (regs !! 1) `shouldBe` Just (-500_000_000)

    it "delegation vote writes a delegation_vote row" $ do
      written <- runGovernance
        [txWithCerts
          [ GenericTxCertificate 0 (CertConwayDelegVote (CredHash stakeCredA False) (DRepCred (CredHash drepCredA False)) Nothing)
          ]
        ]
      length (twDelegationVotes written) `shouldBe` 1
      let dv = headDef (panic "expected delegation_vote") (twDelegationVotes written)
      SG.delegationVoteCertIndex dv `shouldBe` 0

  describe "Committee cert pass" $ do
    it "cold->hot pair writes 2 committee_hash + 1 committee_registration" $ do
      written <- runGovernance
        [txWithCerts
          [ GenericTxCertificate 0 (CertCommitteeAuth (CredHash coldKey False) (CredHash hotKey False))
          ]
        ]
      length (twCommitteeHashes written) `shouldBe` 2
      length (twCommitteeRegistrations written) `shouldBe` 1

  describe "Proposal pass" $ do
    it "ParameterChange with cost model writes param_proposal + cost_model + gov_action_proposal" $ do
      let paramAction = GovParameterChange Nothing emptyParamProposal Nothing
      written <- runGovernance [txWithProposal (proposal paramAction)]
      length (twParamProposals written) `shouldBe` 1
      length (twGovActionProposals written) `shouldBe` 1

    it "TreasuryWithdraw with 2 recipients writes 2 treasury_withdrawal rows" $ do
      let action = GovTreasuryWithdraw
            [(CredHash stakeCredA False, 1_000_000), (CredHash stakeCredB False, 2_000_000)]
            Nothing
      written <- runGovernance [txWithProposal (proposal action)]
      length (twGovActionProposals written) `shouldBe` 1
      length (twTreasuryWithdrawals written) `shouldBe` 2

    it "NewConstitution writes a constitution row" $ do
      let action =
            GovNewConstitution Nothing
              (AnchorData "https://constitution.example" anchorHashA)
              Nothing
      written <- runGovernance [txWithProposal (proposal action)]
      length (twConstitutions written) `shouldBe` 1

    it "UpdateCommittee with 2 added members writes committee + 2 committee_member" $ do
      let action = GovUpdateCommittee Nothing Set.empty
                    [(CredHash coldKey False, 200), (CredHash hotKey True, 201)] 2 3
      written <- runGovernance [txWithProposal (proposal action)]
      length (twCommittees written) `shouldBe` 1
      length (twCommitteeMembers written) `shouldBe` 2
      -- A script-tagged added member keeps has_script=True rather than
      -- collapsing onto a defaulted key header (which would duplicate
      -- the committee_hash row against the auth/resign path).
      sort [ (SG.committeeHashRaw c, SG.committeeHashHasScript c)
           | (_, c) <- twCommitteeHashes written ]
        `shouldBe` sort [(coldKey, False), (hotKey, True)]

  describe "Pre-Conway update-proposal pass" $ do
    it "update proposal writes param_proposal with target epoch and genesis key, no gov_action_proposal" $ do
      let gpp = emptyParamProposal { gppEpochNo = Just 365, gppKey = Just coldKey }
      written <- runGovernance [(emptyTx ()) { txParamProposal = [gpp] }]
      length (twParamProposals written) `shouldBe` 1
      let (_, pp) = headDef (panic "expected param_proposal") (twParamProposals written)
      SG.paramProposalEpochNo pp `shouldBe` Just 365
      SG.paramProposalKey pp `shouldBe` Just coldKey
      length (twGovActionProposals written) `shouldBe` 0

    it "one proposal per genesis key in the same tx" $ do
      let gpps =
            [ emptyParamProposal { gppEpochNo = Just 365, gppKey = Just coldKey }
            , emptyParamProposal { gppEpochNo = Just 365, gppKey = Just hotKey }
            ]
      written <- runGovernance [(emptyTx ()) { txParamProposal = gpps }]
      length (twParamProposals written) `shouldBe` 2

    it "update proposal carrying a cost model writes cost_model and sets param_proposal.cost_model_id" $ do
      let costModels :: Map.Map Language CostModel
          costModels = Map.singleton PlutusV2 sampleCostModel
          gpp = emptyParamProposal
                  { gppEpochNo  = Just 365
                  , gppKey      = Just coldKey
                  , gppCostmdls = Just costModels
                  }
      written <- runGovernance [(emptyTx ()) { txParamProposal = [gpp] }]
      length (twCostModels written) `shouldBe` 1
      length (twParamProposals written) `shouldBe` 1
      let (cmId, _) = headDef (panic "expected cost_model") (twCostModels written)
          (_, pp)   = headDef (panic "expected param_proposal") (twParamProposals written)
      SG.paramProposalCostModelId pp `shouldBe` Just cmId
      length (twGovActionProposals written) `shouldBe` 0

  describe "Vote pass" $ do
    it "drep voter writes voting_procedure with drep_voter populated" $ do
      let propTx = txWithProposal (proposal GovInfoAction)
          voteTx = (emptyTx ()) { txVotingProcedures = [drepVote drepCredA (proposalRef propTx)] }
      written <- runGovernance [propTx, voteTx]
      length (twVotingProcedures written) `shouldBe` 1
      let vp = headDef (panic "expected voting_procedure") (twVotingProcedures written)
      SG.votingProcedureVoterRole vp `shouldBe` DRep
      SG.votingProcedureDrepVoter vp `shouldSatisfy` isJust
      SG.votingProcedurePoolVoter vp `shouldBe` Nothing
      SG.votingProcedureCommitteeVoter vp `shouldBe` Nothing

    it "cross-block proposal references are silently skipped" $ do
      let voteTx = (emptyTx ())
            { txVotingProcedures =
                [drepVote drepCredA (GovActionRef otherTxHash 0)]
            }
      written <- runGovernance [voteTx]
      length (twVotingProcedures written) `shouldBe` 0

  describe "Anchor dedup" $ do
    it "two proposals sharing an anchor write one voting_anchor row" $ do
      let anchor = AnchorData "https://shared.example" anchorHashA
          mkProp idx = (proposal GovInfoAction)
            { ggapTxIndex = idx
            , ggapAnchor  = anchor
            }
          tx = (emptyTx ()) { txProposals = [mkProp 0, mkProp 1] }
      written <- runGovernance [tx]
      length (twVotingAnchors written) `shouldBe` 1
      length (twGovActionProposals written) `shouldBe` 2

  describe "runGovernanceBoundary" $ do
    it "writes no drep_distr rows when apNewEpoch is Nothing" $ do
      written <- runBoundaryOnly (mkApplyResult Strict.Nothing)
      twDrepDistrs written `shouldBe` []

    it "writes no drep_distr rows when neDRepState is Nothing" $ do
      written <- runBoundaryOnly
        (mkApplyResult (Strict.Just (mkNewEpoch (EpochNo 1) Strict.Nothing)))
      twDrepDistrs written `shouldBe` []

-- ---------------------------------------------------------------------------
-- Test plumbing
-- ---------------------------------------------------------------------------

runGovernance :: [GenericTx] -> IO TestWriterState
runGovernance txs = runGovernanceBlocks [conwayBlock 5 txs]

runGovernanceBlocks :: [GenericBlock] -> IO TestWriterState
runGovernanceBlocks blocks = withTestIngestStores $ \utxoStore dedupStores -> do
  stRef <- newIORef freshExtractState
  addrBuf <- newAddressBufferRef
  wrRef <- newIORef emptyTestWriterState
  let env = mkTestPipelineEnv
              (mkIngestResolver stRef dedupStores addrBuf utxoStore Nothing)
              (mkTestWriter wrRef)
              [coreExtractor, governanceExtractor]
  for_ blocks $ \b -> runReaderT (processBlock b) env
  readIORef wrRef

runBoundaryOnly :: ApplyResult -> IO TestWriterState
runBoundaryOnly applyResult = withTestIngestStores $ \utxoStore dedupStores -> do
  stRef <- newIORef freshExtractState
  addrBuf <- newAddressBufferRef
  wrRef <- newIORef emptyTestWriterState
  let env = mkTestPipelineEnv
              (mkIngestResolver stRef dedupStores addrBuf utxoStore Nothing)
              (mkTestWriter wrRef)
              [coreExtractor, governanceExtractor]
  runAppM env (runGovernanceBoundary (boundaryApplyData applyResult) (BlockId 100))
  readIORef wrRef

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

drepCredA :: ByteString
drepCredA = BS.replicate 28 0xd1

stakeCredA :: ByteString
stakeCredA = BS.replicate 28 0xa1

stakeCredB :: ByteString
stakeCredB = BS.replicate 28 0xa2

coldKey :: ByteString
coldKey = BS.replicate 28 0xc1

hotKey :: ByteString
hotKey = BS.replicate 28 0x71

-- A well-formed PlutusV2 cost model; only its presence matters for the
-- cost_model dedup assertion, so the parameter values are arbitrary.
sampleCostModel :: CostModel
sampleCostModel =
  fromRight (panic "sampleCostModel: malformed") (mkCostModel PlutusV2 (replicate 175 0))

anchorHashA :: ByteString
anchorHashA = BS.replicate 32 0xe1

otherTxHash :: ByteString
otherTxHash = BS.replicate 32 0x77

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)

dummySlotDetails :: SlotDetails
dummySlotDetails = SlotDetails
  { sdSlotTime    = sampleTime
  , sdCurrentTime = sampleTime
  , sdEpochNo     = EpochNo 0
  , sdSlotNo      = SlotNo 0
  , sdEpochSlot   = 0
  , sdEpochSize   = EpochSize 432000
  }

drepRegCert :: ByteString -> Word64 -> Maybe AnchorData -> GenericTxCertificate
drepRegCert cred deposit mAnchor =
  GenericTxCertificate 0 (CertDRepRegistration (CredHash cred False) deposit mAnchor)

drepDeregCert :: ByteString -> Word64 -> GenericTxCertificate
drepDeregCert cred refund =
  GenericTxCertificate 1 (CertDRepDeregistration (CredHash cred False) refund)

emptyTx :: () -> GenericTx
emptyTx () = GenericTx
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

txWithCerts :: [GenericTxCertificate] -> GenericTx
txWithCerts certs = (emptyTx ()) { txCertificates = certs }

txWithProposal :: GenericGovActionProposal -> GenericTx
txWithProposal p = (emptyTx ()) { txProposals = [p] }

proposal :: GenericGovAction -> GenericGovActionProposal
proposal action = GenericGovActionProposal
  { ggapTxIndex         = 0
  , ggapReturnAddrCred  = CredHash stakeCredA False
  , ggapDeposit         = 1_000_000_000
  , ggapAnchor          = AnchorData "https://prop.example" anchorHashA
  , ggapAction          = action
  , ggapDescriptionJson = "{}"
  }

proposalRef :: GenericTx -> GovActionRef
proposalRef tx = GovActionRef (txHash tx) 0

drepVote :: ByteString -> GovActionRef -> GenericVotingProcedure
drepVote cred ref = GenericVotingProcedure
  { gvpTxIndex     = 0
  , gvpVoter       = VoterDRep (DRepCred (CredHash cred False))
  , gvpGovActionId = ref
  , gvpVote        = VoteYes
  , gvpAnchor      = Nothing
  }

conwayBlock :: Word64 -> [GenericTx] -> GenericBlock
conwayBlock epoch txs = GenericBlock
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
  , blkTxs          = zipWith withIndex [0..] txs
  }
  where
    withIndex i t = t
      { txBlockIndex = i
      , txHash       = BS.replicate 32 (fromIntegral i)
      }

-- | Project an 'ApplyResult' onto the boundary payload, mirroring what the
-- worker enqueues, so the existing 'mkApplyResult' fixtures can drive the
-- 'BoundaryApplyData'-typed boundary extractors.
boundaryApplyData :: ApplyResult -> BoundaryApplyData
boundaryApplyData ar =
  BoundaryApplyData
    { bndNewEpoch        = apNewEpoch ar
    , bndEvents          = apEvents ar
    , bndGovActionState  = apGovActionState ar
    , bndGovExpiresAfter = apGovExpiresAfter ar
    , bndSlotDetails     = apSlotDetails ar
    }

mkApplyResult :: Strict.Maybe Generic.NewEpoch -> ApplyResult
mkApplyResult mNewEpoch = ApplyResult
  { apPrices          = Strict.Nothing
  , apGovExpiresAfter = Strict.Nothing
  , apNewEpoch        = mNewEpoch
  , apDeposits        = Strict.Nothing
  , apSlotDetails     = dummySlotDetails
  , apStakeSlice      = Generic.NoSlices
  , apEvents          = []
  , apGovActionState  = Nothing
  , apDepositsMap     = emptyDepositsMap
  , apPoolsRegistered = Set.empty
  }

mkNewEpoch :: EpochNo -> Strict.Maybe a -> Generic.NewEpoch
mkNewEpoch epoch _ = Generic.NewEpoch
  { Generic.neEpoch       = epoch
  , Generic.neIsEBB       = False
  , Generic.neAdaPots     = Strict.Nothing
  , Generic.neEpochUpdate = Generic.EpochUpdate
      { Generic.euProtoParams = Strict.Nothing
      , Generic.euNonce       = Ledger.NeutralNonce
      }
  , Generic.neDRepState   = Strict.Nothing
  , Generic.neEnacted     = Strict.Nothing
  , Generic.nePoolDistr   = Strict.Nothing
  }

emptyParamProposal :: GenericParamProposal
emptyParamProposal = GenericParamProposal
  { gppEpochNo                    = Nothing
  , gppKey                        = Nothing
  , gppMinFeeA                    = Just 44
  , gppMinFeeB                    = Nothing
  , gppMaxBlockSize               = Nothing
  , gppMaxTxSize                  = Nothing
  , gppMaxBhSize                  = Nothing
  , gppKeyDeposit                 = Nothing
  , gppPoolDeposit                = Nothing
  , gppMaxEpoch                   = Nothing
  , gppOptimalPoolCount           = Nothing
  , gppInfluence                  = Nothing
  , gppMonetaryExpandRate         = Nothing
  , gppTreasuryGrowthRate         = Nothing
  , gppDecentralisation           = Nothing
  , gppEntropy                    = Nothing
  , gppProtocolMajor              = Nothing
  , gppProtocolMinor              = Nothing
  , gppMinUtxoValue               = Nothing
  , gppMinPoolCost                = Nothing
  , gppCoinsPerUtxoSize           = Nothing
  , gppCostmdls                   = Nothing
  , gppPriceMem                   = Nothing
  , gppPriceStep                  = Nothing
  , gppMaxTxExMem                 = Nothing
  , gppMaxTxExSteps               = Nothing
  , gppMaxBlockExMem              = Nothing
  , gppMaxBlockExSteps            = Nothing
  , gppMaxValSize                 = Nothing
  , gppCollateralPercent          = Nothing
  , gppMaxCollateralInputs        = Nothing
  , gppPvtMotionNoConfidence      = Nothing
  , gppPvtCommitteeNormal         = Nothing
  , gppPvtCommitteeNoConfidence   = Nothing
  , gppPvtHardForkInitiation      = Nothing
  , gppPvtppSecurityGroup         = Nothing
  , gppDvtMotionNoConfidence      = Nothing
  , gppDvtCommitteeNormal         = Nothing
  , gppDvtCommitteeNoConfidence   = Nothing
  , gppDvtUpdateToConstitution    = Nothing
  , gppDvtHardForkInitiation      = Nothing
  , gppDvtPPNetworkGroup          = Nothing
  , gppDvtPPEconomicGroup         = Nothing
  , gppDvtPPTechnicalGroup        = Nothing
  , gppDvtPPGovGroup              = Nothing
  , gppDvtTreasuryWithdrawal      = Nothing
  , gppCommitteeMinSize           = Nothing
  , gppCommitteeMaxTermLength     = Nothing
  , gppGovActionLifetime          = Nothing
  , gppGovActionDeposit           = Nothing
  , gppDrepDeposit                = Nothing
  , gppDrepActivity               = Nothing
  , gppMinFeeRefScriptCostPerByte = Nothing
  }
