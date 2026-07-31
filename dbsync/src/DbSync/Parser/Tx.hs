{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Transaction extraction from Shelley+ blocks.
--
-- Each @from*Tx@ function converts an era-specific ledger transaction
-- into our era-independent 'GenericTx'. Helpers are shared across eras
-- where possible; later eras add capabilities progressively.
module DbSync.Parser.Tx
  ( -- * Era-specific converters
    fromShelleyTx
  , fromAllegraTx
  , fromMaryTx
  , fromAlonzoTx
  , fromBabbageTx
  , fromConwayTx
  , fromDijkstraTx

    -- * Internal helpers (exported for tests)
  , drepToIdent
  , anchorData
  , conwayDelegAction
  , conwayGovAction
  , credToCredHash
  , dijkstraDelegAction
  , scriptHashBytes
  , shelleyCertToAction
  ) where

import Cardano.Prelude

import Cardano.Binary (serialize')
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Binary.Encoding as LedgerCBOR
import Cardano.Ledger.Binary.Version (shelleyProtVer)

-- Ledger re-export module that bundles most lenses for Babbage+.
-- Also re-exports Core, Mary, Alonzo, Allegra lenses.
import Cardano.Ledger.Babbage.Core as Core hiding (Tx, TxOut)
import qualified Cardano.Ledger.Core as Core

-- Era-specific modules for things not in the re-export bundle
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Allegra.Scripts as Allegra
import qualified Cardano.Ledger.Allegra.TxAuxData as Allegra
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxOut as Alonzo
import qualified Cardano.Ledger.Alonzo.TxAuxData as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWits as Alonzo
import Cardano.Ledger.BaseTypes (Anchor (..), StrictMaybe, TxIx (..), strictMaybeToMaybe, unboundRational, portToWord16, dnsToText, urlToText)
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Conway.Governance
  ( Constitution (..)
  , GovAction (..)
  , GovActionId (..)
  , GovActionIx (..)
  , GovPurposeId (..)
  , ProposalProcedure (..)
  , Vote (..)
  , Voter (..)
  , VotingProcedure (..)
  , VotingProcedures (..)
  , constitutionAnchor
  , constitutionGuardrailsScriptHashL
  )
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.Scripts (PlutusScript (ConwayPlutusV1, ConwayPlutusV2, ConwayPlutusV3))
import Cardano.Ledger.Babbage.Scripts (PlutusScript (BabbagePlutusV1, BabbagePlutusV2))
import Cardano.Ledger.Conway.TxBody
  ( ctbProposalProcedures
  , ctbTreasuryDonation
  , ctbVotingProcedures
  )
import Cardano.Ledger.Conway.TxCert
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.DRep as Ledger
import Cardano.Ledger.Dijkstra.Scripts (DijkstraPlutusPurpose (..))
import Cardano.Ledger.Dijkstra.Scripts (PlutusScript (DijkstraPlutusV1, DijkstraPlutusV2, DijkstraPlutusV3, DijkstraPlutusV4))
import Cardano.Ledger.Dijkstra.TxBody (dtbTreasuryDonation)
import Cardano.Ledger.Dijkstra.TxCert (DijkstraDelegCert (..), DijkstraTxCert (..))
import qualified Cardano.Ledger.Keys as Ledger
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), PolicyID (..), AssetName (..))
import qualified Cardano.Ledger.Plutus.Data as Plutus
import qualified Cardano.Ledger.Plutus.ExUnits as Plutus
import qualified Cardano.Ledger.Shelley.PParams as Shelley
import Cardano.Ledger.Shelley.TxCert
import qualified Cardano.Ledger.Shelley.Scripts as Shelley
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.State as PoolP
import qualified Cardano.Ledger.TxIn as Ledger
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import qualified Data.Set as Set

import qualified DbSync.Parser.Script as Script
import qualified DbSync.Parser.ScriptData as ScriptData
import qualified DbSync.Db.Types as Db
import DbSync.Db.Types (ScriptPurpose (..), ScriptType (..))

import qualified Data.Map.Strict as Map

import Data.Array.Byte (ByteArray (..))
import Data.ByteString.Short (ShortByteString (SBS))
import qualified Data.ByteString.Short as SBS
import qualified Data.Text.Encoding as Text
import Lens.Micro ((^.))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS

import qualified DbSync.Parser.Metadata as Metadata
import qualified DbSync.Parser.ParamProposal as PP
import DbSync.Util (coinToWord64, jsonValueContainsNul)

import Ouroboros.Consensus.Cardano.Block
  ( AllegraEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra
  , DijkstraEra
  , MaryEra
  , ShelleyEra
  )

import DbSync.Parser.Types
  ( GenericTx (..)
  , GenericTxIn (..)
  , GenericTxOut (..)
  , GenericTxCertificate (..)
  , GenericTxWithdrawal (..)
  , GenericTxScript (..)
  , GenericTxDatum (..)
  , GenericTxRedeemer (..)
  , CertAction (..)
  , CredHash (..)
  , DRepIdent (..)
  , AnchorData (..)
  , MirAction (..)
  , MirPot (..)
  , PoolRegistrationData (..)
  , PoolRelayData (..)
  , GenericGovAction (..)
  , GenericGovActionProposal (..)
  , GenericVoter (..)
  , GenericVotingProcedure (..)
  , GovActionRef (..)
  , rewardAddrCredHash
  )

-- ---------------------------------------------------------------------------
-- * Shared helpers
-- ---------------------------------------------------------------------------

-- | Transaction hash as raw bytes.
--
-- In cardano-node 10.7.1 the @Tx@ / @TxBody@ type families gained a @TxLevel@
-- parameter (e.g. @Core.Tx Core.TopTx era@).  Helpers stay polymorphic in the
-- level variable @l@ so they work for both top-level and (future) inner
-- transactions.  Top-level entry points (@fromShelleyTx@ etc.) use
-- @Core.TopTx@ specifically because consensus blocks only contain top-level
-- transactions.
txHashId :: Core.EraTx era => Core.Tx l era -> ByteString
txHashId = Crypto.hashToBytes . extractHash . txSafeHash

-- | SafeHash of a transaction body.  Split out so GHC can resolve the
-- ambiguous 'HashAnnotated' instance.
txSafeHash :: Core.EraTx era => Core.Tx l era -> SafeHash EraIndependentTxBody
txSafeHash tx = Core.hashAnnotated (tx ^. Core.bodyTxL)

-- | Transaction size in bytes.
getTxSize :: Core.EraTx era => Core.Tx l era -> Word64
getTxSize tx = fromIntegral $ tx ^. Core.sizeTxF

-- | Raw CBOR bytes of the full transaction (for tx_cbor table).
getTxCborBytes :: (Core.EraTx era, Typeable l) => Core.Tx l era -> ByteString
getTxCborBytes = toStrictBytes . serialize'
  where
    toStrictBytes = toS

-- | Extract inputs from a transaction body.
mkTxIn :: Core.EraTxBody era => Core.TxBody l era -> [GenericTxIn]
mkTxIn txBody = map fromTxIn $ toList $ txBody ^. Core.inputsTxBodyL

-- | Convert a single ledger TxIn.
fromTxIn :: Ledger.TxIn -> GenericTxIn
fromTxIn (Ledger.TxIn (Ledger.TxId txid) (TxIx ix)) =
  GenericTxIn
    { txInHash       = Crypto.hashToBytes (extractHash txid)
    , txInIndex      = ix
    , txInRedeemerIx = Nothing
    }

-- | Extract outputs from a Shelley/Allegra body (Coin-only, no multi-assets).
mkTxOutCoin ::
  forall l era.
  (Core.EraTxBody era, Core.Value era ~ Coin) =>
  Core.TxBody l era ->
  [GenericTxOut]
mkTxOutCoin txBody = zipWith fromCoinTxOut [0 ..] $ toList (txBody ^. Core.outputsTxBodyL)
  where
    fromCoinTxOut :: Word16 -> Core.TxOut era -> GenericTxOut
    fromCoinTxOut idx txOut =
      let addr = txOut ^. Core.addrTxOutL
          !raw = Ledger.serialiseAddr addr
      in GenericTxOut
        { txOutIndex       = idx
        , txOutAddressRaw  = raw
        , txOutValue       = fromIntegral (unCoin (txOut ^. Core.valueTxOutL))
        , txOutDataHash    = Nothing
        , txOutInlineDatum = Nothing
        , txOutRefScript   = Nothing
        , txOutMultiAssets  = []
        }

-- | Extract outputs from a Mary+ body (with multi-asset values).
--
-- The datum extractor varies by era: Mary uses @\\_ -> (Nothing, Nothing)@,
-- Alonzo uses 'getAlonzoDatum', Babbage+ uses 'getBabbageDatum'. It
-- returns @(data_hash, inline_datum)@. The reference-script extractor
-- is @const Nothing@ before Babbage and 'outputRefScript' after.
mkTxOutMaryValue ::
  forall l era.
  (Core.EraTxBody era, Core.Value era ~ MaryValue) =>
  (Core.TxOut era -> (Maybe ByteString, Maybe GenericTxDatum)) ->
  (Core.TxOut era -> Maybe GenericTxScript) ->
  Core.TxBody l era ->
  [GenericTxOut]
mkTxOutMaryValue datum refScript txBody =
  zipWith (mkMaryTxOut datum refScript) [0 ..] $ toList (txBody ^. Core.outputsTxBodyL)

-- | Build one Mary-shape 'GenericTxOut' from a ledger TxOut and a
-- datum extractor.  Reused by 'mkTxOutMaryValue' and by the
-- Babbage+ collateral-return path.
mkMaryTxOut ::
  forall era.
  (Core.EraTxOut era, Core.Value era ~ MaryValue) =>
  (Core.TxOut era -> (Maybe ByteString, Maybe GenericTxDatum)) ->
  (Core.TxOut era -> Maybe GenericTxScript) ->
  Word16 ->
  Core.TxOut era ->
  GenericTxOut
mkMaryTxOut datum refScript idx txOut =
  let MaryValue ada multiAsset = txOut ^. Core.valueTxOutL
      addr = txOut ^. Core.addrTxOutL
      !raw = Ledger.serialiseAddr addr
      (dataHash, inlineDatum) = datum txOut
  in GenericTxOut
    { txOutIndex       = idx
    , txOutAddressRaw  = raw
    , txOutValue       = fromIntegral (unCoin ada)
    , txOutDataHash    = dataHash
    , txOutInlineDatum = inlineDatum
    , txOutRefScript   = refScript txOut
    , txOutMultiAssets = flattenMultiAsset multiAsset
    }

-- | Extract withdrawals. Uses 'EraTxBody' constraint which works across all eras.
mkTxWithdrawals :: Core.EraTxBody era => Core.TxBody l era -> [GenericTxWithdrawal]
mkTxWithdrawals bd =
  map fromWithdrawal $ Map.toList $ Ledger.unWithdrawals $ bd ^. Core.withdrawalsTxBodyL
  where
    fromWithdrawal (ra, Coin c) =
      GenericTxWithdrawal
        { txwRewardAddress = Ledger.serialiseAccountAddress ra
        , txwAmount        = fromIntegral c
        , txwRedeemerIx    = Nothing
        }

-- | Extract certificates from a Shelley-Babbage era tx body.
-- These eras share the ShelleyTxCert certificate type.
mkTxCertificatesShelleyEra :: Core.EraTxBody era => (Core.TxCert era -> CertAction) -> Core.TxBody l era -> [GenericTxCertificate]
mkTxCertificatesShelleyEra convert bd =
  zipWith toCert [0 ..] $ toList (bd ^. Core.certsTxBodyL)
  where
    toCert idx cert =
      GenericTxCertificate
        { txCertIndex      = idx
        , txCertAction     = convert cert
        , txCertRedeemerIx = Nothing
        }

-- | Convert a ShelleyTxCert (any Shelley-Babbage era) to CertAction.
--
-- Genesis-key delegation certs are pre-Shelley housekeeping with no
-- downstream consumer; the parser collapses them into 'CertOther'
-- and the downstream extractors ignore them.
shelleyCertToAction
  :: forall era. EraTxCert era
  => ShelleyTxCert era -> CertAction
shelleyCertToAction = \case
  ShelleyTxCertDelegCert deleg -> shelleyDelegAction deleg
  ShelleyTxCertPool pool       -> poolCertAction pool
  ShelleyTxCertMir mir         -> mirCertAction mir
  cert@(ShelleyTxCertGenesisDeleg _) ->
    CertOther (LedgerCBOR.serialize' shelleyProtVer cert)

-- | Destructure a Shelley-era MIR certificate into our structured
-- 'CertMir' representation. The two payload shapes are kept
-- separate at the type level so the 'stake_delegation' extractor
-- can dispatch without re-parsing CBOR.
mirCertAction :: MIRCert -> CertAction
mirCertAction mc =
  CertMir (mirPotToOurs (mirPot mc)) (mirTargetToAction (mirRewards mc))
  where
    mirPotToOurs :: MIRPot -> MirPot
    mirPotToOurs ReservesMIR = MirReserves
    mirPotToOurs TreasuryMIR = MirTreasury

    mirTargetToAction :: MIRTarget -> MirAction
    mirTargetToAction (StakeAddressesMIR rwds) =
      MirToStakeAddresses
        [ (credToCredHash cred, deltaCoinToInteger d)
        | (cred, d) <- Map.toList rwds
        ]
    mirTargetToAction (SendToOppositePotMIR coin) =
      MirPotToPot (unCoin coin)

    deltaCoinToInteger :: DeltaCoin -> Integer
    deltaCoinToInteger (DeltaCoin i) = i

-- | Convert a ConwayTxCert to CertAction.
conwayCertToAction :: ConwayTxCert era -> CertAction
conwayCertToAction = \case
  ConwayTxCertDeleg deleg -> conwayDelegAction deleg
  ConwayTxCertPool pool   -> poolCertAction pool
  ConwayTxCertGov gov     -> conwayGovAction gov

-- | Convert a DijkstraTxCert to CertAction.
--
-- Dijkstra ships its own 'DijkstraTxCert' wrapper but the delegation
-- and governance payloads are byte-identical to Conway's. We funnel
-- both into the same 'CertAction' so downstream extractors stay
-- era-agnostic.
dijkstraCertToAction :: DijkstraTxCert era -> CertAction
dijkstraCertToAction = \case
  DijkstraTxCertDeleg deleg -> dijkstraDelegAction deleg
  DijkstraTxCertPool pool   -> poolCertAction pool
  DijkstraTxCertGov gov     -> conwayGovAction gov

-- | Convert Dijkstra delegation cert subtypes.
--
-- Dijkstra renamed Conway's @ConwayRegCert@ / @ConwayUnRegCert@ to
-- @DijkstraRegCert@ / @DijkstraUnRegCert@, which are now mandatory-
-- deposit (no @StrictMaybe@). Otherwise structurally identical.
dijkstraDelegAction :: DijkstraDelegCert -> CertAction
dijkstraDelegAction = \case
  DijkstraRegCert cred deposit ->
    CertStakeRegistration (credToCredHash cred) (Just $ coinToWord64 deposit)
  DijkstraUnRegCert cred _deposit ->
    CertStakeDeregistration (credToCredHash cred)
  DijkstraDelegCert cred delegatee -> case delegatee of
    DelegStake poolHash ->
      CertDelegation (credToCredHash cred) (keyHashToBytes poolHash)
    DelegVote drep ->
      CertConwayDelegVote (credToCredHash cred) (drepToIdent drep) Nothing
    DelegStakeVote poolHash drep ->
      CertConwayDelegStakeVote (credToCredHash cred) (keyHashToBytes poolHash) (drepToIdent drep) Nothing
  DijkstraRegDelegCert cred delegatee deposit -> case delegatee of
    DelegStake poolHash ->
      CertConwayRegDeleg (credToCredHash cred) (keyHashToBytes poolHash) (Just $ coinToWord64 deposit)
    DelegVote drep ->
      -- Combined register + vote-delegation. The deposit signals the
      -- stake-registration half so it is not lost; the vote half rides
      -- the DRep into the governance extractor.
      CertConwayDelegVote (credToCredHash cred) (drepToIdent drep) (Just $ coinToWord64 deposit)
    DelegStakeVote poolHash drep ->
      CertConwayDelegStakeVote (credToCredHash cred) (keyHashToBytes poolHash) (drepToIdent drep) (Just $ coinToWord64 deposit)

-- | Convert Shelley delegation cert subtypes.
shelleyDelegAction :: ShelleyDelegCert -> CertAction
shelleyDelegAction = \case
  ShelleyRegCert cred   -> CertStakeRegistration (credToCredHash cred) Nothing
  ShelleyUnRegCert cred -> CertStakeDeregistration (credToCredHash cred)
  ShelleyDelegCert cred poolHash ->
    CertDelegation (credToCredHash cred) (keyHashToBytes poolHash)

-- | Convert Conway delegation cert subtypes.
conwayDelegAction :: ConwayDelegCert -> CertAction
conwayDelegAction = \case
  ConwayRegCert cred mDeposit ->
    CertStakeRegistration (credToCredHash cred) (coinToWord64 <$> strictMaybeToMaybe mDeposit)
  ConwayUnRegCert cred _mDeposit ->
    CertStakeDeregistration (credToCredHash cred)
  ConwayDelegCert cred delegatee -> case delegatee of
    DelegStake poolHash ->
      CertDelegation (credToCredHash cred) (keyHashToBytes poolHash)
    DelegVote drep ->
      CertConwayDelegVote (credToCredHash cred) (drepToIdent drep) Nothing
    DelegStakeVote poolHash drep ->
      CertConwayDelegStakeVote (credToCredHash cred) (keyHashToBytes poolHash) (drepToIdent drep) Nothing
  ConwayRegDelegCert cred delegatee mDeposit -> case delegatee of
    DelegStake poolHash ->
      CertConwayRegDeleg (credToCredHash cred) (keyHashToBytes poolHash) (Just $ coinToWord64 mDeposit)
    DelegVote drep ->
      -- Combined register + vote-delegation. The deposit signals the
      -- stake-registration half so it is not lost; the vote half rides
      -- the DRep into the governance extractor.
      CertConwayDelegVote (credToCredHash cred) (drepToIdent drep) (Just $ coinToWord64 mDeposit)
    DelegStakeVote poolHash drep ->
      -- Combined register + stake-delegation + vote-delegation. The
      -- deposit signals the stake-registration half so it is not lost.
      CertConwayDelegStakeVote (credToCredHash cred) (keyHashToBytes poolHash) (drepToIdent drep) (Just $ coinToWord64 mDeposit)

-- | Convert Conway governance cert subtypes.
conwayGovAction :: ConwayGovCert -> CertAction
conwayGovAction = \case
  ConwayRegDRep cred coin mAnchor ->
    CertDRepRegistration (credToCredHash cred) (coinToWord64 coin) (anchorData <$> strictMaybeToMaybe mAnchor)
  ConwayUnRegDRep cred coin ->
    CertDRepDeregistration (credToCredHash cred) (coinToWord64 coin)
  ConwayAuthCommitteeHotKey coldKey hotKey ->
    CertCommitteeAuth (credToCredHash coldKey) (credToCredHash hotKey)
  ConwayResignCommitteeColdKey coldKey mAnchor ->
    CertCommitteeResign (credToCredHash coldKey) (anchorData <$> strictMaybeToMaybe mAnchor)
  ConwayUpdateDRep cred mAnchor ->
    CertDRepUpdate (credToCredHash cred) (anchorData <$> strictMaybeToMaybe mAnchor)

-- | Project a ledger 'Ledger.DRep' into our three-way 'DRepIdent'.
drepToIdent :: Ledger.DRep -> DRepIdent
drepToIdent = \case
  Ledger.DRepCredential cred    -> DRepCred (credToCredHash cred)
  Ledger.DRepAlwaysAbstain      -> DRepAlwaysAbstain
  Ledger.DRepAlwaysNoConfidence -> DRepAlwaysNoConfidence

-- | Pull the URL and 32-byte data hash out of a ledger 'Anchor'.
anchorData :: Anchor -> AnchorData
anchorData a = AnchorData
  { adUrl  = urlToText (anchorUrl a)
  , adHash = Crypto.hashToBytes (extractHash (anchorDataHash a))
  }

-- | Convert a pool certificate (shared between Shelley and Conway).
poolCertAction :: Core.PoolCert -> CertAction
poolCertAction = \case
  Core.RegPool params ->
    CertPoolRegistration $ poolParamsToData params
  Core.RetirePool poolHash epochNo ->
    CertPoolRetirement (keyHashToBytes poolHash) (unEpochNo epochNo)

-- | Extract pool registration data from StakePoolParams.
poolParamsToData :: PoolP.StakePoolParams -> PoolRegistrationData
poolParamsToData pp = PoolRegistrationData
  { prdPoolHash    = keyHashToBytes (PoolP.sppId pp)
  , prdVrfKeyHash  = Crypto.hashToBytes (Ledger.fromVRFVerKeyHash (PoolP.sppVrf pp))
  , prdPledge      = coinToWord64 (PoolP.sppPledge pp)
  , prdCost        = coinToWord64 (PoolP.sppCost pp)
  , prdMargin      = unboundRational (PoolP.sppMargin pp)
  , prdRewardAddr  = Ledger.serialiseAccountAddress (PoolP.sppAccountAddress pp)
  , prdOwners      = map keyHashToBytes $ toList (PoolP.sppOwners pp)
  , prdRelays      = map relayToData $ toList (PoolP.sppRelays pp)
  , prdMetadata    = poolMetadataToData <$> strictMaybeToMaybe (PoolP.sppMetadata pp)
  }

-- | Convert a pool relay to our generic type.
relayToData :: PoolP.StakePoolRelay -> PoolRelayData
relayToData = \case
  PoolP.SingleHostAddr mPort mIpv4 mIpv6 ->
    PoolRelaySingleAddr
      (portToWord16 <$> strictMaybeToMaybe mPort)
      (show <$> strictMaybeToMaybe mIpv4)
      (show <$> strictMaybeToMaybe mIpv6)
  PoolP.SingleHostName mPort name ->
    PoolRelayDnsName
      (portToWord16 <$> strictMaybeToMaybe mPort)
      (dnsToText name)
  PoolP.MultiHostName name ->
    PoolRelayDnsSrv (dnsToText name)

-- | Convert pool metadata to (URL, hash).
--
-- In cardano-node 10.7.1, @PoolP.pmHash@ now returns @ByteArray@ rather
-- than a bare @ByteString@, so we wrap the conversion in a local
-- @byteArrayToSBS@ helper.
poolMetadataToData :: PoolP.PoolMetadata -> (Text, ByteString)
poolMetadataToData md =
  (urlToText (PoolP.pmUrl md), SBS.fromShort (byteArrayToSBS (PoolP.pmHash md)))
  where
    byteArrayToSBS :: ByteArray -> ShortByteString
    byteArrayToSBS (ByteArray ba) = SBS ba

-- | Flatten a ledger credential to its 28-byte hash plus whether it is
-- a script hash, preserving the key\/script tag the raw bytes alone
-- cannot express.
credToCredHash :: Ledger.Credential kr -> CredHash
credToCredHash (Ledger.KeyHashObj (Ledger.KeyHash h)) =
  CredHash (Crypto.hashToBytes h) False
credToCredHash (Ledger.ScriptHashObj (Core.ScriptHash h)) =
  CredHash (Crypto.hashToBytes h) True

-- | The 28-byte hash of a credential, discarding the key\/script tag.
-- For sites that store the raw hash without a reward-address header.
credBytes :: Ledger.Credential kr -> ByteString
credBytes = chHash . credToCredHash

-- | Serialise a KeyHash to raw bytes.
keyHashToBytes :: Ledger.KeyHash r -> ByteString
keyHashToBytes (Ledger.KeyHash h) = Crypto.hashToBytes h

-- | Project the era-specific auxiliary data, if present.
-- Per-era 'from*Metadata' helpers consume this to recover the
-- @Map Word64 Metadatum@.
getTxAuxData :: Core.EraTx era => Core.Tx l era -> Maybe (Core.TxAuxData era)
getTxAuxData tx = strictMaybeToMaybe (tx ^. Core.auxDataTxL)

-- | Validity interval extraction (Allegra+ eras).
getInterval :: AllegraEraTxBody era => Core.TxBody l era -> (Maybe Word64, Maybe Word64)
getInterval txBody =
  ( fmap unSlotNo $ strictMaybeToMaybe $ invalidBefore interval
  , fmap unSlotNo $ strictMaybeToMaybe $ invalidHereafter interval
  )
  where
    interval = txBody ^. vldtTxBodyL

-- | Sum of output values.
sumOutputValues :: [GenericTxOut] -> Word64
sumOutputValues = sum . map txOutValue

-- | The collateral a failed phase-2 tx actually pays as its fee.
totalCollateral :: Core.BabbageEraTxBody era => Core.TxBody Core.TopTx era -> Word64
totalCollateral txBody =
  maybe 0 (fromIntegral . unCoin) (strictMaybeToMaybe (txBody ^. Core.totalCollateralTxBodyL))

-- | Extract minting from a Mary+ body as flat list.
getMint :: MaryEraTxBody era => Core.TxBody l era -> [(ByteString, ByteString, Integer)]
getMint txBody = flattenMultiAsset (txBody ^. mintTxBodyL)

-- | Flatten a MultiAsset into @[(policy_id, asset_name, quantity)]@.
flattenMultiAsset :: MultiAsset -> [(ByteString, ByteString, Integer)]
flattenMultiAsset (MultiAsset m) =
  [ (policyIdBytes pid, assetNameBytes' an, qty)
  | (pid, assets) <- Map.toList m
  , (an, qty) <- Map.toList assets
  ]
  where
    policyIdBytes (PolicyID (Core.ScriptHash h)) = Crypto.hashToBytes h
    assetNameBytes' (AssetName sbs) = SBS.fromShort sbs

-- | Extract collateral inputs (Alonzo+ eras).
--
-- 'collateralInputsTxBodyL' only exists for top-level transactions, so this
-- helper specialises to @Core.TopTx@ rather than being polymorphic in the
-- TxLevel.  Same for 'mkRefTxIn' below.
mkCollTxIn :: AlonzoEraTxBody era => Core.TxBody Core.TopTx era -> [GenericTxIn]
mkCollTxIn txBody = map fromTxIn $ toList $ txBody ^. collateralInputsTxBodyL

-- | Extract reference inputs (Babbage+ eras).
mkRefTxIn :: BabbageEraTxBody era => Core.TxBody Core.TopTx era -> [GenericTxIn]
mkRefTxIn txBody = map fromTxIn $ toList $ txBody ^. referenceInputsTxBodyL

-- | Sum of Plutus script sizes in a transaction's witness set.
--
-- Native (Timelock) scripts return 'Nothing' from 'getPlutusScriptSize'
-- and so contribute zero. Empty witness sets yield 0.
getPlutusScriptSizesSum
  :: ( Core.EraTx era
     , Core.TxWits era ~ Alonzo.AlonzoTxWits era
     , Core.Script era ~ Alonzo.AlonzoScript era
     , Alonzo.AlonzoEraScript era
     )
  => Core.Tx l era -> Word64
getPlutusScriptSizesSum tx =
  sum $ mapMaybe plutusSize $ Map.elems $
    tx ^. (Core.witsTxL . Alonzo.scriptAlonzoTxWitsL)
  where
    plutusSize :: Alonzo.AlonzoEraScript era => Alonzo.AlonzoScript era -> Maybe Word64
    plutusSize = \case
      Alonzo.NativeScript {}  -> Nothing
      Alonzo.PlutusScript ps  ->
        Just $ fromIntegral $ SBS.length $ Alonzo.unPlutusBinary $ Alonzo.plutusScriptBinary ps

-- | Datum projection for an Alonzo TxOut: the @(data_hash, inline)@
-- pair. Alonzo outputs only ever carry hashes, never inline datums, so
-- the second component is always 'Nothing'.
getAlonzoDatum
  :: ( Alonzo.AlonzoEraTxOut era
     , Core.TxOut era ~ Alonzo.AlonzoTxOut era
     )
  => Alonzo.AlonzoTxOut era -> (Maybe ByteString, Maybe GenericTxDatum)
getAlonzoDatum txOut =
  case strictMaybeToMaybe (txOut ^. Alonzo.dataHashTxOutL) of
    Nothing -> (Nothing, Nothing)
    Just dh -> (Just (Crypto.hashToBytes (extractHash dh)), Nothing)

-- | Datum projection for a Babbage+ TxOut: the @(data_hash, inline)@
-- pair. A hash-only output yields just the hash; an inline datum yields
-- both the hash and the 'GenericTxDatum' (CBOR + JSON) so the UTxO
-- extractor can write the deduplicated @datum@ row and the FK.
getBabbageDatum
  :: ( Core.BabbageEraTxOut era
     , Core.TxOut era ~ Babbage.BabbageTxOut era
     )
  => Babbage.BabbageTxOut era -> (Maybe ByteString, Maybe GenericTxDatum)
getBabbageDatum txOut =
  case txOut ^. Core.datumTxOutL of
    Plutus.DatumHash dh -> (Just (Crypto.hashToBytes (extractHash dh)), Nothing)
    Plutus.NoDatum      -> (Nothing, Nothing)
    Plutus.Datum bd     ->
      let d   = Plutus.binaryDataToData bd
          gtd = GenericTxDatum
            { gtdHash  = dataHashBytes (Alonzo.hashData d)
            , gtdBytes = Core.originalBytes d
            , gtdValue = Just (ScriptData.plutusDataToJson d)
            }
      in (Just (gtdHash gtd), Just gtd)

-- | Extract the Babbage+ collateral-return output, if present.
--
-- The collateral output is a single optional output that survives a
-- failed phase-2 transaction. Its index is the count of regular
-- outputs in the body, mirroring how the chain numbers it.
getCollateralOutput
  :: ( Core.BabbageEraTxBody era
     , Core.Value era ~ MaryValue
     , Core.TxOut era ~ Babbage.BabbageTxOut era
     )
  => (Core.TxOut era -> Maybe GenericTxScript)
  -> Core.TxBody Core.TopTx era
  -> Maybe GenericTxOut
getCollateralOutput refScript txBody =
  fmap (mkMaryTxOut getBabbageDatum refScript collIdx) $
    strictMaybeToMaybe (txBody ^. Core.collateralReturnTxBodyL)
  where
    collIdx = fromIntegral (length (toList (txBody ^. Core.outputsTxBodyL)))

-- ---------------------------------------------------------------------------
-- * Scripts, datums, redeemers
-- ---------------------------------------------------------------------------

scriptHashBytes :: Core.ScriptHash -> ByteString
scriptHashBytes (Core.ScriptHash h) = Crypto.hashToBytes h

dataHashBytes :: Core.SafeHash Core.EraIndependentData -> ByteString
dataHashBytes = Crypto.hashToBytes . extractHash

-- | Extract the Shelley-era native scripts (witness set only;
-- Shelley aux data carries no scripts).
shelleyScripts
  :: Core.Tx Core.TopTx ShelleyEra -> [GenericTxScript]
shelleyScripts tx =
  map fromMultiSig $ Map.toList (tx ^. Core.witsTxL . Core.scriptTxWitsL)
  where
    fromMultiSig :: (Core.ScriptHash, Shelley.MultiSig ShelleyEra) -> GenericTxScript
    fromMultiSig (h, sc) = GenericTxScript
      { gtsHash           = scriptHashBytes h
      , gtsType           = MultiSig
      , gtsJson           = Just (Script.multiSigToJson sc)
      , gtsBytes          = Nothing
      , gtsSerialisedSize = Nothing
      }

-- | Extract Allegra/Mary-era timelock scripts from witness set and
-- aux data.
timelockScripts
  :: forall era.
     ( Allegra.AllegraEraScript era
     , Core.EraTx era
     , Core.NativeScript era ~ Allegra.Timelock era
     , Core.Script era ~ Allegra.Timelock era
     , Core.TxAuxData era ~ Allegra.AllegraTxAuxData era
     )
  => Core.Tx Core.TopTx era
  -> [GenericTxScript]
timelockScripts tx =
  map fromTimelock $
    Map.toList (tx ^. Core.witsTxL . Core.scriptTxWitsL)
      <> auxScripts (tx ^. Core.auxDataTxL)
  where
    fromTimelock :: (Core.ScriptHash, Allegra.Timelock era) -> GenericTxScript
    fromTimelock (h, sc) = GenericTxScript
      { gtsHash           = scriptHashBytes h
      , gtsType           = Timelock
      , gtsJson           = Just (Script.timelockToJson sc)
      , gtsBytes          = Nothing
      , gtsSerialisedSize = Nothing
      }

    auxScripts :: StrictMaybe (Allegra.AllegraTxAuxData era)
               -> [(Core.ScriptHash, Allegra.Timelock era)]
    auxScripts =
      maybe [] indexed . strictMaybeToMaybe
      where
        indexed (Allegra.AllegraTxAuxData _ scrs) =
          [ (Core.hashScript @era s, s) | s <- toList scrs ]

-- | Extract scripts (native and Plutus) from the witness set and
-- auxiliary data. The caller supplies the Plutus-version mapping
-- so the same body works for Alonzo, Babbage, and Conway.
alonzoEraScripts
  :: forall era.
     ( Alonzo.AlonzoEraScript era
     , Core.EraTx era
     , Core.NativeScript era ~ Allegra.Timelock era
     , Core.Script era ~ Alonzo.AlonzoScript era
     , Core.TxAuxData era ~ Alonzo.AlonzoTxAuxData era
     )
  => (Alonzo.PlutusScript era -> ScriptType)
  -> Core.Tx Core.TopTx era
  -> [GenericTxScript]
alonzoEraScripts mkPlutusType tx =
  map (fromAlonzoEraScript mkPlutusType) $
    Map.toList (tx ^. Core.witsTxL . Core.scriptTxWitsL)
      <> auxScripts (tx ^. Core.auxDataTxL)
  where
    auxScripts :: StrictMaybe (Alonzo.AlonzoTxAuxData era)
               -> [(Core.ScriptHash, Alonzo.AlonzoScript era)]
    auxScripts = maybe [] indexed . strictMaybeToMaybe
      where
        indexed auxData =
          [ (Core.hashScript @era s, s)
          | s <- toList (Alonzo.getAlonzoTxAuxDataScripts auxData)
          ]

-- | Convert one Alonzo-family script — from a witness set, auxiliary
-- data, or a Babbage+ output reference — with the era's
-- Plutus-version mapping.
fromAlonzoEraScript
  :: forall era.
     ( Alonzo.AlonzoEraScript era
     , Core.NativeScript era ~ Allegra.Timelock era
     , Core.Script era ~ Alonzo.AlonzoScript era
     )
  => (Alonzo.PlutusScript era -> ScriptType)
  -> (Core.ScriptHash, Alonzo.AlonzoScript era)
  -> GenericTxScript
fromAlonzoEraScript mkPlutusType (h, sc) = case sc of
  Alonzo.NativeScript ns -> GenericTxScript
    { gtsHash           = scriptHashBytes h
    , gtsType           = Timelock
    , gtsJson           = Just (Script.timelockToJson ns)
    , gtsBytes          = Nothing
    , gtsSerialisedSize = Nothing
    }
  Alonzo.PlutusScript ps -> GenericTxScript
    { gtsHash           = scriptHashBytes h
    , gtsType           = mkPlutusType ps
    , gtsJson           = Nothing
    , gtsBytes          = Just (Core.originalBytes sc)
    , gtsSerialisedSize = Just
        (fromIntegral . SBS.length $
          Alonzo.unPlutusBinary (Alonzo.plutusScriptBinary ps))
    }

-- | Dijkstra has its own native script that does not have a JSON
-- encoder; the row stores CBOR bytes instead.
dijkstraEraScripts
  :: forall era.
     ( Alonzo.AlonzoEraScript era
     , Core.EraTx era
     , Core.Script era ~ Alonzo.AlonzoScript era
     , Core.TxAuxData era ~ Alonzo.AlonzoTxAuxData era
     )
  => (Alonzo.PlutusScript era -> ScriptType)
  -> Core.Tx Core.TopTx era
  -> [GenericTxScript]
dijkstraEraScripts mkPlutusType tx =
  map (fromDijkstraEraScript mkPlutusType) $
    Map.toList (tx ^. Core.witsTxL . Core.scriptTxWitsL)
      <> auxScripts (tx ^. Core.auxDataTxL)
  where
    auxScripts :: StrictMaybe (Alonzo.AlonzoTxAuxData era)
               -> [(Core.ScriptHash, Alonzo.AlonzoScript era)]
    auxScripts = maybe [] indexed . strictMaybeToMaybe
      where
        indexed auxData =
          [ (Core.hashScript @era s, s)
          | s <- toList (Alonzo.getAlonzoTxAuxDataScripts auxData)
          ]

-- | Dijkstra variant of 'fromAlonzoEraScript': the native script has
-- no JSON encoder, so the row stores CBOR bytes instead.
fromDijkstraEraScript
  :: forall era.
     ( Alonzo.AlonzoEraScript era
     , Core.Script era ~ Alonzo.AlonzoScript era
     )
  => (Alonzo.PlutusScript era -> ScriptType)
  -> (Core.ScriptHash, Alonzo.AlonzoScript era)
  -> GenericTxScript
fromDijkstraEraScript mkPlutusType (h, sc) = case sc of
  Alonzo.NativeScript {} -> GenericTxScript
    { gtsHash           = scriptHashBytes h
    , gtsType           = Timelock
    , gtsJson           = Nothing
    , gtsBytes          = Just (Core.originalBytes sc)
    , gtsSerialisedSize = Nothing
    }
  Alonzo.PlutusScript ps -> GenericTxScript
    { gtsHash           = scriptHashBytes h
    , gtsType           = mkPlutusType ps
    , gtsJson           = Nothing
    , gtsBytes          = Just (Core.originalBytes sc)
    , gtsSerialisedSize = Just
        (fromIntegral . SBS.length $
          Alonzo.unPlutusBinary (Alonzo.plutusScriptBinary ps))
    }

-- | The Babbage+ output reference script, converted with the era's
-- script builder ('fromAlonzoEraScript' / 'fromDijkstraEraScript').
outputRefScript
  :: Core.BabbageEraTxOut era
  => ((Core.ScriptHash, Core.Script era) -> GenericTxScript)
  -> Core.TxOut era
  -> Maybe GenericTxScript
outputRefScript fromSc txOut =
  (\sc -> fromSc (Core.hashScript sc, sc))
    <$> strictMaybeToMaybe (txOut ^. Core.referenceScriptTxOutL)

-- | Map an era's Plutus script constructor to its 'ScriptType'.
-- Each era exposes only the Plutus versions it supports.
alonzoPlutusType :: Alonzo.PlutusScript AlonzoEra -> ScriptType
alonzoPlutusType _ = PlutusV1

babbagePlutusType :: Alonzo.PlutusScript BabbageEra -> ScriptType
babbagePlutusType = \case
  BabbagePlutusV1 _ -> PlutusV1
  BabbagePlutusV2 _ -> PlutusV2

conwayPlutusType :: Alonzo.PlutusScript ConwayEra -> ScriptType
conwayPlutusType = \case
  ConwayPlutusV1 _ -> PlutusV1
  ConwayPlutusV2 _ -> PlutusV2
  ConwayPlutusV3 _ -> PlutusV3

dijkstraPlutusType :: Alonzo.PlutusScript DijkstraEra -> ScriptType
dijkstraPlutusType = \case
  DijkstraPlutusV1 _ -> PlutusV1
  DijkstraPlutusV2 _ -> PlutusV2
  DijkstraPlutusV3 _ -> PlutusV3
  DijkstraPlutusV4 _ -> PlutusV4

-- | Extract Plutus datum witnesses (Alonzo+).
witnessDatums
  :: forall era l.
     ( Alonzo.AlonzoEraScript era
     , Core.EraTx era
     , Core.TxWits era ~ Alonzo.AlonzoTxWits era
     )
  => Core.Tx l era
  -> [GenericTxDatum]
witnessDatums tx =
  map mkDatum . Map.toList . Alonzo.unTxDats . Alonzo.txdats $ tx ^. Core.witsTxL
  where
    mkDatum :: (Core.SafeHash Core.EraIndependentData, Plutus.Data era) -> GenericTxDatum
    mkDatum (h, d) = GenericTxDatum
      { gtdHash  = dataHashBytes h
      , gtdBytes = Core.originalBytes d
      , gtdValue = Just (ScriptData.plutusDataToJson d)
      }

-- | Extract Plutus redeemer witnesses (Alonzo+).
--
-- The per-era purpose projection returns @(tag, index)@ so the
-- enum and the index come from the same constructor match. The
-- item projection recovers the witnessed script hash from the
-- pointer resolved against the tx body; a dangling pointer (the
-- ledger validates redeemer sets only for phase-2-valid txs)
-- leaves the hash 'Nothing'.
witnessRedeemers
  :: forall era l.
     ( Alonzo.AlonzoEraTxWits era
     , Core.EraTx era
     , AlonzoEraTxBody era
     , Core.TxWits era ~ Alonzo.AlonzoTxWits era
     )
  => (Alonzo.PlutusPurpose Alonzo.AsIx era -> (ScriptPurpose, Word32))
  -> (Alonzo.PlutusPurpose Alonzo.AsIxItem era -> Maybe ByteString)
  -> Core.Tx l era
  -> [GenericTxRedeemer]
witnessRedeemers project itemScriptHash tx =
  map mkRedeemer . Map.toList . Alonzo.unRedeemers $
    tx ^. (Core.witsTxL . Alonzo.rdmrsTxWitsL)
  where
    txBody = tx ^. Core.bodyTxL
    mkRedeemer (purpose, (d, exUnits)) =
      let (tag, idx) = project purpose
      in GenericTxRedeemer
        { gtrUnitMem    = fromIntegral (Plutus.exUnitsMem exUnits)
        , gtrUnitSteps  = fromIntegral (Plutus.exUnitsSteps exUnits)
        , gtrPurpose    = tag
        , gtrIndex      = fromIntegral idx
        , gtrScriptHash =
            itemScriptHash =<< strictMaybeToMaybe (redeemerPointerInverse txBody purpose)
        , gtrDataHash   = dataHashBytes (Alonzo.hashData d)
        , gtrDataBytes  = Core.originalBytes d
        , gtrDataValue  = Just (ScriptData.plutusDataToJson d)
        }

alonzoPurpose :: Alonzo.AlonzoPlutusPurpose Alonzo.AsIx era -> (ScriptPurpose, Word32)
alonzoPurpose = \case
  Alonzo.AlonzoSpending idx   -> (Spend, Alonzo.unAsIx idx)
  Alonzo.AlonzoMinting idx    -> (Mint,  Alonzo.unAsIx idx)
  Alonzo.AlonzoCertifying idx -> (Cert,  Alonzo.unAsIx idx)
  Alonzo.AlonzoRewarding idx  -> (Rewrd, Alonzo.unAsIx idx)

conwayPurpose :: ConwayPlutusPurpose Alonzo.AsIx era -> (ScriptPurpose, Word32)
conwayPurpose = \case
  ConwaySpending idx    -> (Spend,   Alonzo.unAsIx idx)
  ConwayMinting idx     -> (Mint,    Alonzo.unAsIx idx)
  ConwayCertifying idx  -> (Cert,    Alonzo.unAsIx idx)
  ConwayRewarding idx   -> (Rewrd,   Alonzo.unAsIx idx)
  ConwayVoting idx      -> (Vote,    Alonzo.unAsIx idx)
  ConwayProposing idx   -> (Propose, Alonzo.unAsIx idx)

dijkstraPurpose :: DijkstraPlutusPurpose Alonzo.AsIx era -> (ScriptPurpose, Word32)
dijkstraPurpose = \case
  DijkstraSpending idx    -> (Spend,   Alonzo.unAsIx idx)
  DijkstraMinting idx     -> (Mint,    Alonzo.unAsIx idx)
  DijkstraCertifying idx  -> (Cert,    Alonzo.unAsIx idx)
  DijkstraRewarding idx   -> (Rewrd,   Alonzo.unAsIx idx)
  DijkstraVoting idx      -> (Vote,    Alonzo.unAsIx idx)
  DijkstraProposing idx   -> (Propose, Alonzo.unAsIx idx)
  DijkstraGuarding idx    -> (Propose, Alonzo.unAsIx idx)

-- | Script hash named by a resolved redeemer pointer. A 'Spend'
-- pointer resolves to a 'Ledger.TxIn' whose payment credential lives
-- on the spent output, which the parser cannot see; those stay
-- 'Nothing' until input resolution fills them from the database.
alonzoItemScriptHash
  :: EraTxCert era
  => Alonzo.AlonzoPlutusPurpose Alonzo.AsIxItem era -> Maybe ByteString
alonzoItemScriptHash = \case
  Alonzo.AlonzoSpending {} -> Nothing
  Alonzo.AlonzoMinting (Alonzo.AsIxItem _ (PolicyID sh)) -> Just (scriptHashBytes sh)
  Alonzo.AlonzoCertifying (Alonzo.AsIxItem _ cert) ->
    scriptHashBytes <$> getScriptWitnessTxCert cert
  Alonzo.AlonzoRewarding (Alonzo.AsIxItem _ acct) -> accountScriptHash acct

conwayItemScriptHash
  :: EraTxCert era
  => ConwayPlutusPurpose Alonzo.AsIxItem era -> Maybe ByteString
conwayItemScriptHash = \case
  ConwaySpending {} -> Nothing
  ConwayMinting (Alonzo.AsIxItem _ (PolicyID sh)) -> Just (scriptHashBytes sh)
  ConwayCertifying (Alonzo.AsIxItem _ cert) ->
    scriptHashBytes <$> getScriptWitnessTxCert cert
  ConwayRewarding (Alonzo.AsIxItem _ acct) -> accountScriptHash acct
  ConwayVoting (Alonzo.AsIxItem _ voter) -> voterScriptHash voter
  ConwayProposing (Alonzo.AsIxItem _ prop) -> proposalGuardrailScriptHash prop

dijkstraItemScriptHash
  :: EraTxCert era
  => DijkstraPlutusPurpose Alonzo.AsIxItem era -> Maybe ByteString
dijkstraItemScriptHash = \case
  DijkstraSpending {} -> Nothing
  DijkstraMinting (Alonzo.AsIxItem _ (PolicyID sh)) -> Just (scriptHashBytes sh)
  DijkstraCertifying (Alonzo.AsIxItem _ cert) ->
    scriptHashBytes <$> getScriptWitnessTxCert cert
  DijkstraRewarding (Alonzo.AsIxItem _ acct) -> accountScriptHash acct
  DijkstraVoting (Alonzo.AsIxItem _ voter) -> voterScriptHash voter
  DijkstraProposing (Alonzo.AsIxItem _ prop) -> proposalGuardrailScriptHash prop
  DijkstraGuarding (Alonzo.AsIxItem _ sh) -> Just (scriptHashBytes sh)

-- | The credential's script hash, or 'Nothing' for a key credential.
scriptCredHash :: Ledger.Credential kr -> Maybe ByteString
scriptCredHash = \case
  Ledger.ScriptHashObj sh -> Just (scriptHashBytes sh)
  Ledger.KeyHashObj {}    -> Nothing

accountScriptHash :: Ledger.AccountAddress -> Maybe ByteString
accountScriptHash (Ledger.AccountAddress _ (Ledger.AccountId cred)) =
  scriptCredHash cred

voterScriptHash :: Voter -> Maybe ByteString
voterScriptHash = \case
  CommitteeVoter cred -> scriptCredHash cred
  DRepVoter cred      -> scriptCredHash cred
  StakePoolVoter {}   -> Nothing

-- | Guardrail script demanded by the proposal's action, where the
-- action carries one.
proposalGuardrailScriptHash :: ProposalProcedure era -> Maybe ByteString
proposalGuardrailScriptHash prop = case pProcGovAction prop of
  ParameterChange _ _ sh    -> scriptHashBytes <$> strictMaybeToMaybe sh
  TreasuryWithdrawals _ sh  -> scriptHashBytes <$> strictMaybeToMaybe sh
  _                         -> Nothing

-- | Positions (in 'txRedeemers' order) of the redeemers pointing at
-- each body element, keyed by within-purpose body index. Drives the
-- @redeemer_id@ back-references on inputs, certificates,
-- withdrawals, and votes. Mint and propose redeemers point at
-- elements with no back-reference column and are not tracked.
data RedeemerIndexes = RedeemerIndexes
  { riSpend :: !(Map Word32 Word64)
  , riCert  :: !(Map Word32 Word64)
  , riRewrd :: !(Map Word32 Word64)
  , riVote  :: !(Map Word32 Word64)
  }

-- | Walk the redeemer map in the same order as 'witnessRedeemers'
-- so positions line up with the emitted redeemer list.
redeemerIndexes
  :: forall era l.
     ( Alonzo.AlonzoEraTxWits era
     , Core.EraTx era
     , Core.TxWits era ~ Alonzo.AlonzoTxWits era
     )
  => (Alonzo.PlutusPurpose Alonzo.AsIx era -> (ScriptPurpose, Word32))
  -> Core.Tx l era
  -> RedeemerIndexes
redeemerIndexes project tx =
  foldl' step (RedeemerIndexes Map.empty Map.empty Map.empty Map.empty) $
    zip [0 ..] (Map.keys (Alonzo.unRedeemers (tx ^. (Core.witsTxL . Alonzo.rdmrsTxWitsL))))
  where
    step :: RedeemerIndexes -> (Word64, Alonzo.PlutusPurpose Alonzo.AsIx era) -> RedeemerIndexes
    step ri (pos, purpose) = case project purpose of
      (Spend, ix) -> ri { riSpend = Map.insert ix pos (riSpend ri) }
      (Cert,  ix) -> ri { riCert  = Map.insert ix pos (riCert ri) }
      (Rewrd, ix) -> ri { riRewrd = Map.insert ix pos (riRewrd ri) }
      (Vote,  ix) -> ri { riVote  = Map.insert ix pos (riVote ri) }
      _           -> ri

-- Annotation by list position works because each body element list
-- is built in the same order the ledger's pointer indexes count:
-- inputs from the sorted input set, certificates and withdrawals in
-- body order.

annotateInputRedeemers :: RedeemerIndexes -> [GenericTxIn] -> [GenericTxIn]
annotateInputRedeemers ri = zipWith ann [0 ..]
  where
    ann ix txIn = txIn { txInRedeemerIx = Map.lookup ix (riSpend ri) }

annotateCertRedeemers :: RedeemerIndexes -> [GenericTxCertificate] -> [GenericTxCertificate]
annotateCertRedeemers ri = zipWith ann [0 ..]
  where
    ann ix cert = cert { txCertRedeemerIx = Map.lookup ix (riCert ri) }

annotateWithdrawalRedeemers :: RedeemerIndexes -> [GenericTxWithdrawal] -> [GenericTxWithdrawal]
annotateWithdrawalRedeemers ri = zipWith ann [0 ..]
  where
    ann ix w = w { txwRedeemerIx = Map.lookup ix (riRewrd ri) }

-- | Required-signer key hashes from the tx body. The ledger
-- constrains 'reqSignerHashesTxBodyL' to Alonzo..Conway; Dijkstra
-- has no equivalent yet so 'fromDijkstraTx' emits @[]@ directly.
extraKeyHashes
  :: (Core.AlonzoEraTxBody era, Core.AtMostEra "Conway" era)
  => Core.TxBody l era -> [ByteString]
extraKeyHashes txBody =
  map keyHashToBytes
    . Set.toList
    $ txBody ^. Core.reqSignerHashesTxBodyL

-- ---------------------------------------------------------------------------
-- * Shelley era
-- ---------------------------------------------------------------------------

fromShelleyTx :: (Word64, Core.Tx Core.TopTx ShelleyEra) -> GenericTx
fromShelleyTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      outputs = mkTxOutCoin txBody
      fee = txBody ^. Core.feeTxBodyL
  in GenericTx
    { txHash             = txHashId tx
    , txBlockIndex       = blkIndex
    , txSize             = getTxSize tx
    , txFee              = fromIntegral (unCoin fee)
    , txOutSum           = sumOutputValues outputs
    , txValidContract    = True
    , txScriptSize       = 0
    , txTreasuryDonation = 0
    , txInvalidBefore    = Nothing
    , txInvalidHereafter = Just $ unSlotNo (txBody ^. Shelley.ttlTxBodyL)
    , txInputs           = mkTxIn txBody
    , txOutputs          = outputs
    , txCollateralInputs = []
    , txReferenceInputs  = []
    , txCollateralOutput = Nothing
    , txCertificates     = mkTxCertificatesShelleyEra shelleyCertToAction txBody
    , txWithdrawals      = mkTxWithdrawals txBody
    , txMetadata         = Metadata.getMetadata <$> getTxAuxData tx
    , txMint             = []
    , txCborRaw          = Just (getTxCborBytes tx)
      -- Only MultiSig witness scripts; Plutus datums, redeemers,
      -- and required-signer sets are not part of the Shelley era.
    , txScripts           = shelleyScripts tx
    , txDatums            = []
    , txRedeemers         = []
    , txExtraKeyWitnesses = []
      -- Shelley genesis-key parameter proposals from the Update field.
      -- Conway+ has no Update field; this stays empty there.
    , txParamProposal     = mkParamProposalsUpdate PP.shelleyParamProposal txBody
    , txProposals         = []  -- pre-Conway
    , txVotingProcedures  = []  -- pre-Conway
    , txVotingAnchors     = []  -- pre-Conway
    }

-- ---------------------------------------------------------------------------
-- * Allegra era (adds validity intervals)
-- ---------------------------------------------------------------------------

fromAllegraTx :: (Word64, Core.Tx Core.TopTx AllegraEra) -> GenericTx
fromAllegraTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      outputs = mkTxOutCoin txBody
      fee = txBody ^. Core.feeTxBodyL
      (invBefore, invAfter) = getInterval txBody
  in GenericTx
    { txHash             = txHashId tx
    , txBlockIndex       = blkIndex
    , txSize             = getTxSize tx
    , txFee              = fromIntegral (unCoin fee)
    , txOutSum           = sumOutputValues outputs
    , txValidContract    = True
    , txScriptSize       = 0
    , txTreasuryDonation = 0
    , txInvalidBefore    = invBefore
    , txInvalidHereafter = invAfter
    , txInputs           = mkTxIn txBody
    , txOutputs          = outputs
    , txCollateralInputs = []
    , txReferenceInputs  = []
    , txCollateralOutput = Nothing
    , txCertificates     = mkTxCertificatesShelleyEra shelleyCertToAction txBody
    , txWithdrawals      = mkTxWithdrawals txBody
    , txMetadata         = Metadata.getMetadata <$> getTxAuxData tx
    , txMint             = []
    , txCborRaw          = Just (getTxCborBytes tx)
      -- Timelock scripts from witness set and auxiliary data; no
      -- Plutus support.
    , txScripts           = timelockScripts tx
    , txDatums            = []
    , txRedeemers         = []
    , txExtraKeyWitnesses = []
    , txParamProposal     = mkParamProposalsUpdate PP.shelleyParamProposal txBody
    , txProposals         = []  -- pre-Conway
    , txVotingProcedures  = []  -- pre-Conway
    , txVotingAnchors     = []  -- pre-Conway
    }

-- ---------------------------------------------------------------------------
-- * Mary era (adds multi-assets)
-- ---------------------------------------------------------------------------

fromMaryTx :: (Word64, Core.Tx Core.TopTx MaryEra) -> GenericTx
fromMaryTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      outputs = mkTxOutMaryValue (\_ -> (Nothing, Nothing)) (const Nothing) txBody
      fee = txBody ^. Core.feeTxBodyL
      (invBefore, invAfter) = getInterval txBody
  in GenericTx
    { txHash             = txHashId tx
    , txBlockIndex       = blkIndex
    , txSize             = getTxSize tx
    , txFee              = fromIntegral (unCoin fee)
    , txOutSum           = sumOutputValues outputs
    , txValidContract    = True
    , txScriptSize       = 0
    , txTreasuryDonation = 0
    , txInvalidBefore    = invBefore
    , txInvalidHereafter = invAfter
    , txInputs           = mkTxIn txBody
    , txOutputs          = outputs
    , txCollateralInputs = []
    , txReferenceInputs  = []
    , txCollateralOutput = Nothing
    , txCertificates     = mkTxCertificatesShelleyEra shelleyCertToAction txBody
    , txWithdrawals      = mkTxWithdrawals txBody
    , txMetadata         = Metadata.getMetadata <$> getTxAuxData tx
    , txMint             = getMint txBody
    , txCborRaw          = Just (getTxCborBytes tx)
      -- Same Timelock witness shape as Allegra; no Plutus support.
    , txScripts           = timelockScripts tx
    , txDatums            = []
    , txRedeemers         = []
    , txExtraKeyWitnesses = []
    , txParamProposal     = mkParamProposalsUpdate PP.shelleyParamProposal txBody
    , txProposals         = []  -- pre-Conway
    , txVotingProcedures  = []  -- pre-Conway
    , txVotingAnchors     = []  -- pre-Conway
    }

-- ---------------------------------------------------------------------------
-- * Alonzo era (adds Plutus, collateral, phase-2 validation)
-- ---------------------------------------------------------------------------

fromAlonzoTx :: (Word64, Core.Tx Core.TopTx AlonzoEra) -> GenericTx
fromAlonzoTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      Alonzo.IsValid isValid = tx ^. Alonzo.isValidTxL
      outputs = mkTxOutMaryValue getAlonzoDatum (const Nothing) txBody
      fee = txBody ^. Core.feeTxBodyL
      (invBefore, invAfter) = getInterval txBody
      collIns = mkCollTxIn txBody
      rIdxs = redeemerIndexes alonzoPurpose tx
  in GenericTx
    { txHash             = txHashId tx
    , txBlockIndex       = blkIndex
    , txSize             = getTxSize tx
      -- Failed phase-2 txs do not pay the body fee; the chain charges
      -- collateral instead. We emit 0 here and let a post-load SQL
      -- pass backfill the actual collateral diff.
    , txFee              = if isValid then fromIntegral (unCoin fee) else 0
    , txOutSum           = if isValid then sumOutputValues outputs else 0
    , txValidContract    = isValid
    , txScriptSize       = getPlutusScriptSizesSum tx
    , txTreasuryDonation = 0
    , txInvalidBefore    = invBefore
    , txInvalidHereafter = invAfter
      -- Failed phase-2 txs consume collateral, not the declared inputs, so
      -- tx_in carries the collateral set (matching Babbage+); Alonzo has no
      -- collateral-return field, so there are no on-chain outputs.
    , txInputs           = if isValid then annotateInputRedeemers rIdxs (mkTxIn txBody) else collIns
    , txOutputs          = if isValid then outputs else []
    , txCollateralInputs = collIns
    , txReferenceInputs  = []
    , txCollateralOutput = Nothing
    , txCertificates     = annotateCertRedeemers rIdxs (mkTxCertificatesShelleyEra shelleyCertToAction txBody)
    , txWithdrawals      = annotateWithdrawalRedeemers rIdxs (mkTxWithdrawals txBody)
    , txMetadata         = Metadata.getMetadata <$> getTxAuxData tx
    , txMint             = getMint txBody
    , txCborRaw          = Just (getTxCborBytes tx)
      -- Plutus V1 scripts plus datum and redeemer witnesses;
      -- required-signer set from the tx body.
    , txScripts           = alonzoEraScripts alonzoPlutusType tx
    , txDatums            = witnessDatums tx
    , txRedeemers         = witnessRedeemers alonzoPurpose alonzoItemScriptHash tx
    , txExtraKeyWitnesses = extraKeyHashes txBody
    , txParamProposal     = mkParamProposalsUpdate PP.alonzoParamProposal txBody
    , txProposals         = []  -- pre-Conway
    , txVotingProcedures  = []  -- pre-Conway
    , txVotingAnchors     = []  -- pre-Conway
    }

-- ---------------------------------------------------------------------------
-- * Babbage era (adds reference inputs, collateral outputs, inline datums)
-- ---------------------------------------------------------------------------

fromBabbageTx :: (Word64, Core.Tx Core.TopTx BabbageEra) -> GenericTx
fromBabbageTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      Alonzo.IsValid isValid = tx ^. Alonzo.isValidTxL
      refScript = outputRefScript (fromAlonzoEraScript babbagePlutusType)
      outputs = mkTxOutMaryValue getBabbageDatum refScript txBody
      fee = txBody ^. Core.feeTxBodyL
      (invBefore, invAfter) = getInterval txBody
      collIns = mkCollTxIn txBody
      refIns = mkRefTxIn txBody
      collOut = getCollateralOutput refScript txBody
      rIdxs = redeemerIndexes alonzoPurpose tx
  in GenericTx
    { txHash             = txHashId tx
    , txBlockIndex       = blkIndex
    , txSize             = getTxSize tx
    , txFee              = if isValid then fromIntegral (unCoin fee) else totalCollateral txBody
    , txOutSum           = if isValid then sumOutputValues outputs else sumOutputValues (maybeToList collOut)
    , txValidContract    = isValid
    , txScriptSize       = getPlutusScriptSizesSum tx
    , txTreasuryDonation = 0
    , txInvalidBefore    = invBefore
    , txInvalidHereafter = invAfter
    , txInputs           = if isValid then annotateInputRedeemers rIdxs (mkTxIn txBody) else collIns
    , txOutputs          = if isValid then outputs else maybeToList collOut
    , txCollateralInputs = collIns
    , txReferenceInputs  = refIns
    , txCollateralOutput = if isValid then collOut else Nothing
    , txCertificates     = annotateCertRedeemers rIdxs (mkTxCertificatesShelleyEra shelleyCertToAction txBody)
    , txWithdrawals      = annotateWithdrawalRedeemers rIdxs (mkTxWithdrawals txBody)
    , txMetadata         = Metadata.getMetadata <$> getTxAuxData tx
    , txMint             = getMint txBody
    , txCborRaw          = Just (getTxCborBytes tx)
      -- Plutus V1/V2 scripts; redeemer purpose set is the Alonzo
      -- four (spend, mint, cert, reward).
    , txScripts           = alonzoEraScripts babbagePlutusType tx
    , txDatums            = witnessDatums tx
    , txRedeemers         = witnessRedeemers alonzoPurpose alonzoItemScriptHash tx
    , txExtraKeyWitnesses = extraKeyHashes txBody
    , txParamProposal     = mkParamProposalsUpdate PP.babbageParamProposal txBody
    , txProposals         = []  -- pre-Conway
    , txVotingProcedures  = []  -- pre-Conway
    , txVotingAnchors     = []  -- pre-Conway
    }

-- ---------------------------------------------------------------------------
-- * Conway era (adds governance, treasury donations)
-- ---------------------------------------------------------------------------

fromConwayTx :: (Word64, Core.Tx Core.TopTx ConwayEra) -> GenericTx
fromConwayTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      Alonzo.IsValid isValid = tx ^. Alonzo.isValidTxL
      refScript = outputRefScript (fromAlonzoEraScript conwayPlutusType)
      outputs = mkTxOutMaryValue getBabbageDatum refScript txBody
      fee = txBody ^. Core.feeTxBodyL
      (invBefore, invAfter) = getInterval txBody
      collIns = mkCollTxIn txBody
      refIns = mkRefTxIn txBody
      collOut = getCollateralOutput refScript txBody
      Coin donation = ctbTreasuryDonation txBody
      rIdxs = redeemerIndexes conwayPurpose tx
      certs = annotateCertRedeemers rIdxs (mkTxCertificatesShelleyEra conwayCertToAction txBody)
      props = conwayProposals txBody
      votes = conwayVotingProcedures txBody (riVote rIdxs)
  in GenericTx
    { txHash             = txHashId tx
    , txBlockIndex       = blkIndex
    , txSize             = getTxSize tx
    , txFee              = if isValid then fromIntegral (unCoin fee) else totalCollateral txBody
    , txOutSum           = if isValid then sumOutputValues outputs else sumOutputValues (maybeToList collOut)
    , txValidContract    = isValid
    , txScriptSize       = getPlutusScriptSizesSum tx
    , txTreasuryDonation = fromIntegral donation
    , txInvalidBefore    = invBefore
    , txInvalidHereafter = invAfter
    , txInputs           = if isValid then annotateInputRedeemers rIdxs (mkTxIn txBody) else collIns
    , txOutputs          = if isValid then outputs else maybeToList collOut
    , txCollateralInputs = collIns
    , txReferenceInputs  = refIns
    , txCollateralOutput = if isValid then collOut else Nothing
    , txCertificates     = certs
    , txWithdrawals      = annotateWithdrawalRedeemers rIdxs (mkTxWithdrawals txBody)
    , txMetadata         = Metadata.getMetadata <$> getTxAuxData tx
    , txMint             = getMint txBody
    , txCborRaw          = Just (getTxCborBytes tx)
      -- Plutus V1/V2/V3 scripts; redeemer purpose set gains
      -- 'Vote' and 'Propose'.
    , txScripts           = alonzoEraScripts conwayPlutusType tx
    , txDatums            = witnessDatums tx
    , txRedeemers         = witnessRedeemers conwayPurpose conwayItemScriptHash tx
    , txExtraKeyWitnesses = extraKeyHashes txBody
      -- Conway abandoned genesis-key parameter updates; parameter
      -- changes ride 'GovParameterChange' inside 'txProposals'.
    , txParamProposal     = []
    , txProposals         = props
    , txVotingProcedures  = votes
    , txVotingAnchors     = collectVotingAnchors certs props votes
    }
-- ---------------------------------------------------------------------------
-- * Dijkstra era (Conway extension)
-- ---------------------------------------------------------------------------

fromDijkstraTx :: (Word64, Core.Tx Core.TopTx DijkstraEra) -> GenericTx
fromDijkstraTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      Alonzo.IsValid isValid = tx ^. Alonzo.isValidTxL
      refScript = outputRefScript (fromDijkstraEraScript dijkstraPlutusType)
      outputs = mkTxOutMaryValue getBabbageDatum refScript txBody
      fee = txBody ^. Core.feeTxBodyL
      (invBefore, invAfter) = getInterval txBody
      collIns = mkCollTxIn txBody
      refIns = mkRefTxIn txBody
      collOut = getCollateralOutput refScript txBody
      Coin donation = dtbTreasuryDonation txBody
      rIdxs = redeemerIndexes dijkstraPurpose tx
  in GenericTx
    { txHash             = txHashId tx
    , txBlockIndex       = blkIndex
    , txSize             = getTxSize tx
    , txFee              = if isValid then fromIntegral (unCoin fee) else totalCollateral txBody
    , txOutSum           = if isValid then sumOutputValues outputs else sumOutputValues (maybeToList collOut)
    , txValidContract    = isValid
    , txScriptSize       = getPlutusScriptSizesSum tx
    , txTreasuryDonation = fromIntegral donation
    , txInvalidBefore    = invBefore
    , txInvalidHereafter = invAfter
    , txInputs           = if isValid then annotateInputRedeemers rIdxs (mkTxIn txBody) else collIns
    , txOutputs          = if isValid then outputs else maybeToList collOut
    , txCollateralInputs = collIns
    , txReferenceInputs  = refIns
    , txCollateralOutput = if isValid then collOut else Nothing
    , txCertificates     = annotateCertRedeemers rIdxs (mkTxCertificatesShelleyEra dijkstraCertToAction txBody)
    , txWithdrawals      = annotateWithdrawalRedeemers rIdxs (mkTxWithdrawals txBody)
    , txMetadata         = Metadata.getMetadata <$> getTxAuxData tx
    , txMint             = getMint txBody
    , txCborRaw          = Just (getTxCborBytes tx)
      -- Dijkstra native scripts have no JSON renderer; the row
      -- stores CBOR bytes. Required-signer extraction is a
      -- placeholder pending the Dijkstra body wiring.
    , txScripts           = dijkstraEraScripts dijkstraPlutusType tx
    , txDatums            = witnessDatums tx
    , txRedeemers         = witnessRedeemers dijkstraPurpose dijkstraItemScriptHash tx
    , txExtraKeyWitnesses = []  -- TODO(Dijkstra)
    , txParamProposal     = []
    , txProposals         = []  -- TODO(Dijkstra): dtbProposalProcedures lens not yet wired
    , txVotingProcedures  = []  -- TODO(Dijkstra): dtbVotingProcedures lens not yet wired
    , txVotingAnchors     = []  -- TODO(Dijkstra)
    }

-- ---------------------------------------------------------------------------
-- * Governance extraction (Conway+)
-- ---------------------------------------------------------------------------

-- | Build the list of 'GenericParamProposal' rows from a Shelley-Babbage
-- tx body's 'Shelley.updateTxBodyL' lens, using the supplied per-era
-- converter. Conway+ has no 'updateTxBodyL' so the field stays @[]@ for
-- those eras.
mkParamProposalsUpdate
  :: Shelley.ShelleyEraTxBody era
  => (EpochNo -> Shelley.ProposedPPUpdates era -> [PP.GenericParamProposal])
  -> Core.TxBody Core.TopTx era
  -> [PP.GenericParamProposal]
mkParamProposalsUpdate convert txBody =
  case strictMaybeToMaybe (txBody ^. Shelley.updateTxBodyL) of
    Nothing -> []
    Just (Shelley.Update pp epoch) -> convert epoch pp

-- | Extract the proposals from a Conway-era tx body. Dijkstra mirrors
-- this shape once the @DijkstraTxBody@ proposal lens lands; for now
-- 'fromDijkstraTx' emits @[]@ directly.
conwayProposals
  :: Core.TxBody Core.TopTx ConwayEra
  -> [GenericGovActionProposal]
conwayProposals txBody =
  zipWith mkProposal [0 ..] (toList (ctbProposalProcedures txBody))
  where
    mkProposal :: Word64 -> ProposalProcedure ConwayEra -> GenericGovActionProposal
    mkProposal idx pp = GenericGovActionProposal
      { ggapTxIndex         = idx
      , ggapReturnAddrCred  =
          rewardAddrCredHash (Ledger.serialiseAccountAddress (pProcReturnAddr pp))
      , ggapDeposit         = coinToWord64 (pProcDeposit pp)
      , ggapAnchor          = anchorData (pProcAnchor pp)
      , ggapAction          = convertGovAction (pProcGovAction pp)
      , ggapDescriptionJson = renderGovActionJson (pProcGovAction pp)
      }

    -- The description column is NOT NULL jsonb, and PostgreSQL
    -- rejects a Unicode NUL anywhere in a jsonb value. The only
    -- free-text the ledger encoding can carry is anchor URLs, which
    -- the ledger does not character-validate, so a hostile proposal
    -- could otherwise kill the sync. Substitute a placeholder; the
    -- action's structured form survives in 'ggapAction' and the tx
    -- CBOR keeps ground truth.
    renderGovActionJson :: GovAction ConwayEra -> Text
    renderGovActionJson ga
      | jsonValueContainsNul json = encodeText nulPlaceholder
      | otherwise                 = encodeText json
      where
        json = Aeson.toJSON ga
        encodeText = Text.decodeUtf8 . LBS.toStrict . Aeson.encode
        nulPlaceholder = Aeson.object
          [ ("error", Aeson.String "Gov action description contains a Unicode NUL (\\u0000), which PostgreSQL cannot store.") ]

-- | Project a Conway-era 'GovAction' into our 'GenericGovAction' ADT.
-- Conway's @ParameterChange@ embeds a 'PParamsUpdate ConwayEra' that
-- we flatten via 'PP.convertConwayParamProposal'.
convertGovAction :: GovAction ConwayEra -> GenericGovAction
convertGovAction = \case
  ParameterChange prev pparams scriptH ->
    GovParameterChange
      (govPurposeRef <$> strictMaybeToMaybe prev)
      (PP.convertConwayParamProposal pparams)
      (scriptHashBytes <$> strictMaybeToMaybe scriptH)
  HardForkInitiation prev pv ->
    GovHardForkInit
      (govPurposeRef <$> strictMaybeToMaybe prev)
      (fromIntegral @Word64 (Ledger.getVersion (Ledger.pvMajor pv)))
      (fromIntegral (Ledger.pvMinor pv))
  TreasuryWithdrawals mp guardrail ->
    GovTreasuryWithdraw
      [ (rewardAddrCredHash (Ledger.serialiseAccountAddress acct), coinToWord64 coin)
      | (acct, coin) <- Map.toList mp
      ]
      (scriptHashBytes <$> strictMaybeToMaybe guardrail)
  NoConfidence prev ->
    GovNoConfidence (govPurposeRef <$> strictMaybeToMaybe prev)
  UpdateCommittee prev remove add quorum ->
    GovUpdateCommittee
      (govPurposeRef <$> strictMaybeToMaybe prev)
      (Set.map credBytes remove)
      [ (credToCredHash cred, unEpochNo expiry)
      | (cred, expiry) <- Map.toList add
      ]
      (fromIntegral (numerator (Ledger.unboundRational quorum)))
      (fromIntegral (denominator (Ledger.unboundRational quorum)))
  NewConstitution prev constitution ->
    GovNewConstitution
      (govPurposeRef <$> strictMaybeToMaybe prev)
      (anchorData (constitutionAnchor constitution))
      (scriptHashBytes <$> strictMaybeToMaybe
        (constitution ^. constitutionGuardrailsScriptHashL))
  InfoAction -> GovInfoAction

-- | Unwrap a 'GovPurposeId' to our 'GovActionRef'.
govPurposeRef :: GovPurposeId p -> GovActionRef
govPurposeRef (GovPurposeId gaid) = govActionRef gaid

-- | Project a 'GovActionId' (the ledger's @(tx-id, index)@ pair) into our
-- 'GovActionRef'.
govActionRef :: GovActionId -> GovActionRef
govActionRef (GovActionId (Ledger.TxId txid) (GovActionIx ix)) =
  GovActionRef
    { garTxHash = Crypto.hashToBytes (extractHash txid)
    , garIndex  = fromIntegral ix
    }

-- | Extract the votes from a Conway-era tx body. The voter-by-voter
-- map is flattened into one 'GenericVotingProcedure' per
-- @(voter, gov-action)@ pair, with 'gvpTxIndex' counting from zero
-- within each voter's slice. Vote redeemers point at voters, so the
-- position from the vote-redeemer map lands on every row of the
-- voter's slice.
--
-- Dijkstra mirrors this shape once the @DijkstraTxBody@ voting lens
-- lands; for now 'fromDijkstraTx' emits @[]@ directly.
conwayVotingProcedures
  :: Core.TxBody Core.TopTx ConwayEra
  -> Map Word32 Word64
  -> [GenericVotingProcedure]
conwayVotingProcedures txBody voteRedeemers =
  [ mkVote idx voter gaId vp (Map.lookup voterIx voteRedeemers)
  | (voterIx, (voter, actions)) <-
      zip [0 ..] (Map.toList (unVotingProcedures (ctbVotingProcedures txBody)))
  , (idx, (gaId, vp)) <- zip [0 ..] (Map.toList actions)
  ]
  where
    mkVote
      :: Word16
      -> Voter
      -> GovActionId
      -> VotingProcedure ConwayEra
      -> Maybe Word64
      -> GenericVotingProcedure
    mkVote idx voter gaId vp redeemerIx =
      GenericVotingProcedure
        { gvpTxIndex     = idx
        , gvpVoter       = convertVoter voter
        , gvpGovActionId = govActionRef gaId
        , gvpVote        = convertVote (vProcVote vp)
        , gvpAnchor      = anchorData <$> strictMaybeToMaybe (vProcAnchor vp)
        , gvpRedeemerIx  = redeemerIx
        }

-- | Project a ledger 'Voter' into our 'GenericVoter' ADT. The
-- committee arm carries the @has_script@ flag so the dedup pass
-- writes a single @committee_hash@ row per @(raw, has_script)@.
convertVoter :: Voter -> GenericVoter
convertVoter = \case
  DRepVoter cred       -> VoterDRep (credToDRep cred)
  StakePoolVoter pkh   -> VoterStakePool (keyHashToBytes pkh)
  CommitteeVoter cred  ->
    let ch = credToCredHash cred
    in VoterCommittee (chHash ch) (chIsScript ch)
  where
    credToDRep :: Ledger.Credential r -> DRepIdent
    credToDRep = DRepCred . credToCredHash

-- | Project the ledger's three-valued 'Vote' into our enum.
convertVote :: Vote -> Db.Vote
convertVote = \case
  VoteYes -> Db.VoteYes
  VoteNo  -> Db.VoteNo
  Abstain -> Db.VoteAbstain

-- | Flatten the anchors referenced by certs, proposals, and votes
-- into a single list. Order matters only insofar as the dedup pass
-- collapses identical @(url, data_hash, type)@ triples downstream;
-- callers in the governance extractor walk the typed lists to
-- assign the correct 'AnchorType'.
collectVotingAnchors
  :: [GenericTxCertificate]
  -> [GenericGovActionProposal]
  -> [GenericVotingProcedure]
  -> [AnchorData]
collectVotingAnchors certs props votes =
       [ a | cert <- certs, a <- certAnchors (txCertAction cert) ]
    <> [ ggapAnchor p | p <- props ]
    <> [ a | p <- props, GovNewConstitution _ a _ <- [ggapAction p] ]
    <> catMaybes [ gvpAnchor v | v <- votes ]
  where
    certAnchors :: CertAction -> [AnchorData]
    certAnchors = \case
      CertDRepRegistration _ _ (Just a) -> [a]
      CertDRepUpdate       _   (Just a) -> [a]
      CertCommitteeResign  _   (Just a) -> [a]
      _                                 -> []
