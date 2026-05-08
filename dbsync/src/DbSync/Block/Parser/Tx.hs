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
--
-- __First pass:__ Redeemers, scripts, and governance fields are empty.
module DbSync.Block.Parser.Tx
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
  , dijkstraDelegAction
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
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxOut as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWits as Alonzo
import Cardano.Ledger.BaseTypes (Anchor (..), TxIx (..), strictMaybeToMaybe, unboundRational, portToWord16, dnsToText, urlToText)
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.TxBody (ctbTreasuryDonation)
import Cardano.Ledger.Conway.TxCert
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.DRep as Ledger
import Cardano.Ledger.Dijkstra.TxBody (dtbTreasuryDonation)
import Cardano.Ledger.Dijkstra.TxCert (DijkstraDelegCert (..), DijkstraTxCert (..))
import qualified Cardano.Ledger.Keys as Ledger
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), PolicyID (..), AssetName (..))
import qualified Cardano.Ledger.Plutus.Data as Plutus
import Cardano.Ledger.Shelley.TxCert
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.State as PoolP
import qualified Cardano.Ledger.TxIn as Ledger
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import qualified Data.Map.Strict as Map

import qualified Cardano.Chain.Common as Byron

import Data.Array.Byte (ByteArray (..))
import Data.ByteString.Short (ShortByteString (SBS))
import qualified Data.ByteString.Short as SBS
import qualified Data.Text.Encoding as Text
import Lens.Micro ((^.))

import qualified DbSync.Block.Metadata as Metadata
import DbSync.Util.Bech32 (serialiseShelleyAddrToBech32)

import Ouroboros.Consensus.Cardano.Block
  ( AllegraEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra
  , DijkstraEra
  , MaryEra
  , ShelleyEra
  )

import DbSync.Block.Types
  ( GenericTx (..)
  , GenericTxIn (..)
  , GenericTxOut (..)
  , GenericTxCertificate (..)
  , GenericTxWithdrawal (..)
  , CertAction (..)
  , DRepIdent (..)
  , AnchorData (..)
  , PoolRegistrationData (..)
  , PoolRelayData (..)
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
    { txInHash  = Crypto.hashToBytes (extractHash txid)
    , txInIndex = ix
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
      GenericTxOut
        { txOutIndex       = idx
        , txOutAddress     = addrToText (txOut ^. Core.addrTxOutL)
        , txOutAddressRaw  = Ledger.serialiseAddr (txOut ^. Core.addrTxOutL)
        , txOutValue       = fromIntegral (unCoin (txOut ^. Core.valueTxOutL))
        , txOutDataHash    = Nothing
        , txOutInlineDatum = Nothing
        , txOutRefScript   = Nothing
        , txOutMultiAssets  = []
        }

-- | Extract outputs from a Mary+ body (with multi-asset values).
--
-- The data-hash extractor varies by era: Mary uses @\\_ -> Nothing@,
-- Alonzo uses 'getAlonzoDatumHash', Babbage+ uses 'getBabbageDatumHash'.
-- 'txOutInlineDatum' and 'txOutRefScript' are owned by a different
-- extractor and are not populated from this path.
mkTxOutMaryValue ::
  forall l era.
  (Core.EraTxBody era, Core.Value era ~ MaryValue) =>
  (Core.TxOut era -> Maybe ByteString) ->
  Core.TxBody l era ->
  [GenericTxOut]
mkTxOutMaryValue dataHash txBody =
  zipWith (mkMaryTxOut dataHash) [0 ..] $ toList (txBody ^. Core.outputsTxBodyL)

-- | Build one Mary-shape 'GenericTxOut' from a ledger TxOut and a
-- data-hash extractor.  Reused by 'mkTxOutMaryValue' and by the
-- Babbage+ collateral-return path.
mkMaryTxOut ::
  forall era.
  (Core.EraTxOut era, Core.Value era ~ MaryValue) =>
  (Core.TxOut era -> Maybe ByteString) ->
  Word16 ->
  Core.TxOut era ->
  GenericTxOut
mkMaryTxOut dataHash idx txOut =
  let MaryValue ada multiAsset = txOut ^. Core.valueTxOutL
  in GenericTxOut
    { txOutIndex       = idx
    , txOutAddress     = addrToText (txOut ^. Core.addrTxOutL)
    , txOutAddressRaw  = Ledger.serialiseAddr (txOut ^. Core.addrTxOutL)
    , txOutValue       = fromIntegral (unCoin ada)
    , txOutDataHash    = dataHash txOut
    , txOutInlineDatum = Nothing
    , txOutRefScript   = Nothing
    , txOutMultiAssets = flattenMultiAsset multiAsset
    }

-- TODO: re-enable once a caller needs the per-tx withdrawal total.
-- Kept here (commented) because the LEDGER-PLAN expects this helper to
-- live alongside 'mkTxWithdrawals'.
--
-- -- | Sum of all withdrawal amounts.
-- calcWithdrawalSum :: Core.EraTxBody era => Core.TxBody l era -> Word64
-- calcWithdrawalSum bd =
--   fromIntegral $ sum $ map unCoin $ Map.elems $
--     Ledger.unWithdrawals (bd ^. Core.withdrawalsTxBodyL)

-- | Extract withdrawals. Uses 'EraTxBody' constraint which works across all eras.
mkTxWithdrawals :: Core.EraTxBody era => Core.TxBody l era -> [GenericTxWithdrawal]
mkTxWithdrawals bd =
  map fromWithdrawal $ Map.toList $ Ledger.unWithdrawals $ bd ^. Core.withdrawalsTxBodyL
  where
    fromWithdrawal (ra, Coin c) =
      GenericTxWithdrawal
        { txwRewardAddress = Ledger.serialiseAccountAddress ra
        , txwAmount        = fromIntegral c
        }

-- | Extract certificates from a Shelley-Babbage era tx body.
-- These eras share the ShelleyTxCert certificate type.
mkTxCertificatesShelleyEra :: Core.EraTxBody era => (Core.TxCert era -> CertAction) -> Core.TxBody l era -> [GenericTxCertificate]
mkTxCertificatesShelleyEra convert bd =
  zipWith toCert [0 ..] $ toList (bd ^. Core.certsTxBodyL)
  where
    toCert idx cert =
      GenericTxCertificate
        { txCertIndex = idx
        , txCertAction = convert cert
        }

-- | Convert a ShelleyTxCert (any Shelley-Babbage era) to CertAction.
--
-- The 'EraTxCert' constraint gives us 'EncCBOR' for the whole
-- 'ShelleyTxCert' so we can preserve the raw cert bytes for the
-- variants we don't structurally decode.
shelleyCertToAction
  :: forall era. EraTxCert era
  => ShelleyTxCert era -> CertAction
shelleyCertToAction = \case
  ShelleyTxCertDelegCert deleg -> shelleyDelegAction deleg
  ShelleyTxCertPool pool       -> poolCertAction pool
  cert@(ShelleyTxCertMir _)    -> CertMIR (LedgerCBOR.serialize' shelleyProtVer cert)
  cert@(ShelleyTxCertGenesisDeleg _) ->
    CertGenesisDelegation (LedgerCBOR.serialize' shelleyProtVer cert)

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
    CertStakeRegistration (credToBytes cred) (Just $ coinToWord64 deposit)
  DijkstraUnRegCert cred _deposit ->
    CertStakeDeregistration (credToBytes cred)
  DijkstraDelegCert cred delegatee -> case delegatee of
    DelegStake poolHash ->
      CertDelegation (credToBytes cred) (keyHashToBytes poolHash)
    DelegVote drep ->
      CertConwayDelegVote (credToBytes cred) (drepToIdent drep)
    DelegStakeVote poolHash drep ->
      CertConwayDelegStakeVote (credToBytes cred) (keyHashToBytes poolHash) (drepToIdent drep)
  DijkstraRegDelegCert cred delegatee deposit -> case delegatee of
    DelegStake poolHash ->
      CertConwayRegDeleg (credToBytes cred) (keyHashToBytes poolHash) (Just $ coinToWord64 deposit)
    DelegVote _drep ->
      CertStakeRegistration (credToBytes cred) (Just $ coinToWord64 deposit)
    DelegStakeVote poolHash drep ->
      CertConwayDelegStakeVote (credToBytes cred) (keyHashToBytes poolHash) (drepToIdent drep)

-- | Convert Shelley delegation cert subtypes.
shelleyDelegAction :: ShelleyDelegCert -> CertAction
shelleyDelegAction = \case
  ShelleyRegCert cred   -> CertStakeRegistration (credToBytes cred) Nothing
  ShelleyUnRegCert cred -> CertStakeDeregistration (credToBytes cred)
  ShelleyDelegCert cred poolHash ->
    CertDelegation (credToBytes cred) (keyHashToBytes poolHash)

-- | Convert Conway delegation cert subtypes.
conwayDelegAction :: ConwayDelegCert -> CertAction
conwayDelegAction = \case
  ConwayRegCert cred mDeposit ->
    CertStakeRegistration (credToBytes cred) (coinToWord64 <$> strictMaybeToMaybe mDeposit)
  ConwayUnRegCert cred _mDeposit ->
    CertStakeDeregistration (credToBytes cred)
  ConwayDelegCert cred delegatee -> case delegatee of
    DelegStake poolHash ->
      CertDelegation (credToBytes cred) (keyHashToBytes poolHash)
    DelegVote drep ->
      CertConwayDelegVote (credToBytes cred) (drepToIdent drep)
    DelegStakeVote poolHash drep ->
      CertConwayDelegStakeVote (credToBytes cred) (keyHashToBytes poolHash) (drepToIdent drep)
  ConwayRegDelegCert cred delegatee mDeposit -> case delegatee of
    DelegStake poolHash ->
      CertConwayRegDeleg (credToBytes cred) (keyHashToBytes poolHash) (Just $ coinToWord64 mDeposit)
    DelegVote _drep ->
      -- Combined register + vote-delegation. Only the stake-registration
      -- half is materialised here; the vote half is owned by the
      -- governance extractor and consumes the DRep separately.
      CertStakeRegistration (credToBytes cred) (Just $ coinToWord64 mDeposit)
    DelegStakeVote poolHash drep ->
      CertConwayDelegStakeVote (credToBytes cred) (keyHashToBytes poolHash) (drepToIdent drep)

-- | Convert Conway governance cert subtypes.
conwayGovAction :: ConwayGovCert -> CertAction
conwayGovAction = \case
  ConwayRegDRep cred coin mAnchor ->
    CertDRepRegistration (credToBytes cred) (coinToWord64 coin) (anchorData <$> strictMaybeToMaybe mAnchor)
  ConwayUnRegDRep cred coin ->
    CertDRepDeregistration (credToBytes cred) (coinToWord64 coin)
  ConwayAuthCommitteeHotKey coldKey hotKey ->
    CertCommitteeAuth (credToBytes coldKey) (credToBytes hotKey)
  ConwayResignCommitteeColdKey coldKey mAnchor ->
    CertCommitteeResign (credToBytes coldKey) (anchorData <$> strictMaybeToMaybe mAnchor)
  ConwayUpdateDRep cred mAnchor ->
    CertDRepUpdate (credToBytes cred) (anchorData <$> strictMaybeToMaybe mAnchor)

-- | Project a ledger 'Ledger.DRep' into our three-way 'DRepIdent'.
drepToIdent :: Ledger.DRep -> DRepIdent
drepToIdent = \case
  Ledger.DRepCredential cred    -> DRepCred (credToBytes cred)
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
--
-- In cardano-node 10.7.1 / cardano-ledger 1.13+ the @PoolParams@ type was
-- renamed to @StakePoolParams@, accessors gained the @spp*@ prefix, and
-- @ppRewardAccount@ was renamed to @sppAccountAddress@ alongside the
-- @RewardAccount → AccountAddress@ type rename. @PoolP.PoolParams@ still
-- exists as a pattern synonym but is no longer a type.
poolParamsToData :: PoolP.StakePoolParams -> PoolRegistrationData
poolParamsToData pp = PoolRegistrationData
  { prdPoolHash    = keyHashToBytes (PoolP.sppId pp)
  , prdVrfKeyHash  = Crypto.hashToBytes (Ledger.fromVRFVerKeyHash (PoolP.sppVrf pp))
  , prdPledge      = coinToWord64 (PoolP.sppPledge pp)
  , prdCost        = coinToWord64 (PoolP.sppCost pp)
  , prdMargin      = realToFrac $ unboundRational (PoolP.sppMargin pp)
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

-- | Serialise a stake credential to raw bytes.
credToBytes :: Ledger.Credential kr -> ByteString
credToBytes (Ledger.KeyHashObj (Ledger.KeyHash h))    = Crypto.hashToBytes h
credToBytes (Ledger.ScriptHashObj (Core.ScriptHash h)) = Crypto.hashToBytes h

-- | Serialise a KeyHash to raw bytes.
keyHashToBytes :: Ledger.KeyHash r -> ByteString
keyHashToBytes (Ledger.KeyHash h) = Crypto.hashToBytes h

-- | Coin to Word64.
coinToWord64 :: Coin -> Word64
coinToWord64 (Coin c) = fromIntegral c

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

-- | Datum-hash for an Alonzo TxOut. Alonzo outputs only ever carry
-- hashes, never inline datums.
getAlonzoDatumHash
  :: ( Alonzo.AlonzoEraTxOut era
     , Core.TxOut era ~ Alonzo.AlonzoTxOut era
     )
  => Alonzo.AlonzoTxOut era -> Maybe ByteString
getAlonzoDatumHash txOut =
  case strictMaybeToMaybe (txOut ^. Alonzo.dataHashTxOutL) of
    Nothing -> Nothing
    Just dh -> Just (Crypto.hashToBytes (extractHash dh))

-- | Datum-hash for a Babbage+ TxOut. Inline datums (the third 'Datum'
-- constructor) are not surfaced here — only the 'DatumHash' case
-- yields a value for @tx_out.data_hash@.
getBabbageDatumHash
  :: ( Core.BabbageEraTxOut era
     , Core.TxOut era ~ Babbage.BabbageTxOut era
     )
  => Babbage.BabbageTxOut era -> Maybe ByteString
getBabbageDatumHash txOut =
  case txOut ^. Core.datumTxOutL of
    Plutus.DatumHash dh -> Just (Crypto.hashToBytes (extractHash dh))
    Plutus.NoDatum      -> Nothing
    Plutus.Datum _      -> Nothing

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
  => Core.TxBody Core.TopTx era
  -> Maybe GenericTxOut
getCollateralOutput txBody =
  fmap (mkMaryTxOut getBabbageDatumHash collIdx) $
    strictMaybeToMaybe (txBody ^. Core.collateralReturnTxBodyL)
  where
    collIdx = fromIntegral (length (toList (txBody ^. Core.outputsTxBodyL)))

-- | Render a Shelley+ payment address.
--
-- Shelley/Allegra/… addresses go through Bech32 (HRP @addr@ on
-- mainnet, @addr_test@ on testnet); Byron-shaped bootstrap addresses
-- (which can still appear as outputs in Shelley+ blocks) round-trip
-- through Base58.
addrToText :: Ledger.Addr -> Text
addrToText (Ledger.AddrBootstrap (Ledger.BootstrapAddress byronAddr)) =
  Text.decodeUtf8 (Byron.addrToBase58 byronAddr)
addrToText addr@Ledger.Addr{} =
  serialiseShelleyAddrToBech32 (Ledger.serialiseAddr addr)

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
    }

-- ---------------------------------------------------------------------------
-- * Mary era (adds multi-assets)
-- ---------------------------------------------------------------------------

fromMaryTx :: (Word64, Core.Tx Core.TopTx MaryEra) -> GenericTx
fromMaryTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      outputs = mkTxOutMaryValue (\_ -> Nothing) txBody
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
    }

-- ---------------------------------------------------------------------------
-- * Alonzo era (adds Plutus, collateral, phase-2 validation)
-- ---------------------------------------------------------------------------

fromAlonzoTx :: (Word64, Core.Tx Core.TopTx AlonzoEra) -> GenericTx
fromAlonzoTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      Alonzo.IsValid isValid = tx ^. Alonzo.isValidTxL
      outputs = mkTxOutMaryValue getAlonzoDatumHash txBody
      fee = txBody ^. Core.feeTxBodyL
      (invBefore, invAfter) = getInterval txBody
      collIns = mkCollTxIn txBody
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
    , txInputs           = mkTxIn txBody
      -- Failed phase-2 produces no on-chain outputs — Alonzo has no
      -- collateral-return field, so collateral inputs are simply burnt.
    , txOutputs          = if isValid then outputs else []
    , txCollateralInputs = collIns
    , txReferenceInputs  = []
    , txCollateralOutput = Nothing
    , txCertificates     = mkTxCertificatesShelleyEra shelleyCertToAction txBody
    , txWithdrawals      = mkTxWithdrawals txBody
    , txMetadata         = Metadata.getMetadata <$> getTxAuxData tx
    , txMint             = getMint txBody
    , txCborRaw          = Just (getTxCborBytes tx)
    }

-- ---------------------------------------------------------------------------
-- * Babbage era (adds reference inputs, collateral outputs, inline datums)
-- ---------------------------------------------------------------------------

fromBabbageTx :: (Word64, Core.Tx Core.TopTx BabbageEra) -> GenericTx
fromBabbageTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      Alonzo.IsValid isValid = tx ^. Alonzo.isValidTxL
      outputs = mkTxOutMaryValue getBabbageDatumHash txBody
      fee = txBody ^. Core.feeTxBodyL
      (invBefore, invAfter) = getInterval txBody
      collIns = mkCollTxIn txBody
      refIns = mkRefTxIn txBody
  in GenericTx
    { txHash             = txHashId tx
    , txBlockIndex       = blkIndex
    , txSize             = getTxSize tx
    , txFee              = if isValid then fromIntegral (unCoin fee) else 0
    , txOutSum           = if isValid then sumOutputValues outputs else 0
    , txValidContract    = isValid
    , txScriptSize       = getPlutusScriptSizesSum tx
    , txTreasuryDonation = 0
    , txInvalidBefore    = invBefore
    , txInvalidHereafter = invAfter
    , txInputs           = mkTxIn txBody
    , txOutputs          = if isValid then outputs else []
    , txCollateralInputs = collIns
    , txReferenceInputs  = refIns
    , txCollateralOutput = getCollateralOutput txBody
    , txCertificates     = mkTxCertificatesShelleyEra shelleyCertToAction txBody
    , txWithdrawals      = mkTxWithdrawals txBody
    , txMetadata         = Metadata.getMetadata <$> getTxAuxData tx
    , txMint             = getMint txBody
    , txCborRaw          = Just (getTxCborBytes tx)
    }

-- ---------------------------------------------------------------------------
-- * Conway era (adds governance, treasury donations)
-- ---------------------------------------------------------------------------

fromConwayTx :: (Word64, Core.Tx Core.TopTx ConwayEra) -> GenericTx
fromConwayTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      Alonzo.IsValid isValid = tx ^. Alonzo.isValidTxL
      outputs = mkTxOutMaryValue getBabbageDatumHash txBody
      fee = txBody ^. Core.feeTxBodyL
      (invBefore, invAfter) = getInterval txBody
      collIns = mkCollTxIn txBody
      refIns = mkRefTxIn txBody
      Coin donation = ctbTreasuryDonation txBody
  in GenericTx
    { txHash             = txHashId tx
    , txBlockIndex       = blkIndex
    , txSize             = getTxSize tx
    , txFee              = if isValid then fromIntegral (unCoin fee) else 0
    , txOutSum           = if isValid then sumOutputValues outputs else 0
    , txValidContract    = isValid
    , txScriptSize       = getPlutusScriptSizesSum tx
    , txTreasuryDonation = fromIntegral donation
    , txInvalidBefore    = invBefore
    , txInvalidHereafter = invAfter
    , txInputs           = mkTxIn txBody
    , txOutputs          = if isValid then outputs else []
    , txCollateralInputs = collIns
    , txReferenceInputs  = refIns
    , txCollateralOutput = getCollateralOutput txBody
    , txCertificates     = mkTxCertificatesShelleyEra conwayCertToAction txBody
    , txWithdrawals      = mkTxWithdrawals txBody
    , txMetadata         = Metadata.getMetadata <$> getTxAuxData tx
    , txMint             = getMint txBody
    , txCborRaw          = Just (getTxCborBytes tx)
    }
-- ---------------------------------------------------------------------------
-- * Dijkstra era (Conway extension)
-- ---------------------------------------------------------------------------

fromDijkstraTx :: (Word64, Core.Tx Core.TopTx DijkstraEra) -> GenericTx
fromDijkstraTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      Alonzo.IsValid isValid = tx ^. Alonzo.isValidTxL
      outputs = mkTxOutMaryValue getBabbageDatumHash txBody
      fee = txBody ^. Core.feeTxBodyL
      (invBefore, invAfter) = getInterval txBody
      collIns = mkCollTxIn txBody
      refIns = mkRefTxIn txBody
      Coin donation = dtbTreasuryDonation txBody
  in GenericTx
    { txHash             = txHashId tx
    , txBlockIndex       = blkIndex
    , txSize             = getTxSize tx
    , txFee              = if isValid then fromIntegral (unCoin fee) else 0
    , txOutSum           = if isValid then sumOutputValues outputs else 0
    , txValidContract    = isValid
    , txScriptSize       = getPlutusScriptSizesSum tx
    , txTreasuryDonation = fromIntegral donation
    , txInvalidBefore    = invBefore
    , txInvalidHereafter = invAfter
    , txInputs           = mkTxIn txBody
    , txOutputs          = if isValid then outputs else []
    , txCollateralInputs = collIns
    , txReferenceInputs  = refIns
    , txCollateralOutput = getCollateralOutput txBody
    , txCertificates     = mkTxCertificatesShelleyEra dijkstraCertToAction txBody
    , txWithdrawals      = mkTxWithdrawals txBody
    , txMetadata         = Metadata.getMetadata <$> getTxAuxData tx
    , txMint             = getMint txBody
    , txCborRaw          = Just (getTxCborBytes tx)
    }
