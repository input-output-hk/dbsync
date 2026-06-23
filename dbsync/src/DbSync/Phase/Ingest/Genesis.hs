{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Byron genesis distribution insert.
--
-- The Byron genesis block, its synthetic slot leader, and one
-- transaction per genesis UTxO (AVVM redeem + non-AVVM balances) are
-- not delivered over chainsync, so they must be written explicitly at
-- the start of a fresh ingest. Recording the genesis outputs in the
-- UTxO cache also lets later Byron transactions resolve the inputs they
-- spend, which is what gives those transactions a correct fee.
module DbSync.Phase.Ingest.Genesis
  ( insertByronGenesisDist
  ) where

import Cardano.Prelude

import Cardano.Binary (serialize')
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Crypto as Crypto

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text.Encoding as Text

import DbSync.Db.Schema.Address (Address (..))
import DbSync.Db.Schema.Core (Block (..), SlotLeader (..), Tx (..))
import DbSync.Db.Schema.UTxO (TxOut (..))
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Phase.Ingest.UtxoStore (UtxoTxEntry (..))
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.Writer (HasWriter (..), Writer (..))

-- | Write the Byron genesis distribution: one slot leader, one block,
-- and one transaction + output + address per genesis UTxO.
insertByronGenesisDist
  :: (MonadReader env m, HasResolver env, HasWriter env, MonadIO m)
  => Byron.Config
  -> m ()
insertByronGenesisDist cfg = do
  resolver <- asks getResolver
  writer   <- asks getWriter

  let genesisHash    = configGenesisHash cfg
      slotLeaderHash = BS.take 28 genesisHash
      txos           = genesisTxos cfg
      slotLeader = SlotLeader
        { slotLeaderHash        = slotLeaderHash
        , slotLeaderPoolHashId  = Nothing
        , slotLeaderDescription = "Genesis slot leader"
        }

  (slId, isNew) <- liftIO $ resolveSlotLeader resolver slotLeaderHash slotLeader
  when isNew $ liftIO $ writeSlotLeader writer slId slotLeader

  blockId <- liftIO $ assignBlockId resolver
  liftIO $ writeBlock writer blockId Block
    { blockHash          = genesisHash
    , blockEpochNo       = Nothing
    , blockSlotNo        = Nothing
    , blockEpochSlotNo   = Nothing
    , blockBlockNo       = Nothing
    , blockPreviousId    = Nothing
    , blockSlotLeaderId  = slId
    , blockSize          = 0
    , blockTime          = Byron.configStartTime cfg
    , blockTxCount       = fromIntegral (length txos)
    , blockProtoMajor    = 0
    , blockProtoMinor    = 0
    , blockVrfKey        = Nothing
    , blockOpCert        = Nothing
    , blockOpCertCounter = Nothing
    }

  for_ txos $ \(address, value) -> do
    let val    = Byron.unsafeGetLovelace value
        txHash = Crypto.abstractHashToBytes (Crypto.serializeCborHash address)
        raw    = serialize' address
        addr = Address
          { addressAddress        = Text.decodeUtf8 (Byron.addrToBase58 address)
          , addressRaw            = raw
          , addressHasScript      = False
          , addressPaymentCred    = Nothing
          , addressStakeAddressId = Nothing
          }

    txId <- liftIO $ assignTxId resolver
    liftIO $ writeTx writer txId Tx
      { txHash             = txHash
      , txBlockId          = blockId
      , txBlockIndex       = 0
      , txOutSum           = DbLovelace val
      , txFee              = DbLovelace 0
      , txDeposit          = Just 0
      , txSize             = 0
      , txInvalidBefore    = Nothing
      , txInvalidHereafter = Nothing
      , txValidContract    = True
      , txScriptSize       = 0
      , txTreasuryDonation = DbLovelace 0
      }

    outId <- liftIO $ assignTxOutId resolver
    liftIO $ writeTxOut writer outId TxOut
      { txOutTxId              = txId
      , txOutIndex             = 0
      , txOutAddressId         = Nothing
      , txOutStakeAddressId    = Nothing
      , txOutValue             = DbLovelace val
      , txOutDataHash          = Nothing
      , txOutInlineDatumId     = Nothing
      , txOutReferenceScriptId = Nothing
      , txOutConsumedByTxId    = Nothing
      }

    liftIO $ recordTxOutAddress resolver outId raw addr
    liftIO $ recordTxOutputs resolver txHash UtxoTxEntry
      { uteTxId    = txId
      , uteOutputs = Seq.singleton (outId, DbLovelace val)
      }

-- ---------------------------------------------------------------------------
-- * Genesis distribution
-- ---------------------------------------------------------------------------

configGenesisHash :: Byron.Config -> ByteString
configGenesisHash =
  Crypto.abstractHashToBytes . Byron.unGenesisHash . Byron.configGenesisHash

-- | The full genesis UTxO set: AVVM redeem balances followed by the
-- non-AVVM balances.
genesisTxos :: Byron.Config -> [(Byron.Address, Byron.Lovelace)]
genesisTxos cfg = avvmBalances <> nonAvvmBalances
  where
    avvmBalances =
      first (Byron.makeRedeemAddress networkMagic . Crypto.fromCompactRedeemVerificationKey)
        <$> Map.toList (Byron.unGenesisAvvmBalances (Byron.configAvvmDistr cfg))
    networkMagic = Byron.makeNetworkMagic (Byron.configProtocolMagic cfg)
    nonAvvmBalances =
      Map.toList (Byron.unGenesisNonAvvmBalances (Byron.configNonAvvmBalances cfg))
