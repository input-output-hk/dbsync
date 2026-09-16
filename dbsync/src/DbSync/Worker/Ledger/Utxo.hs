{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Reads the live UTxO set out of the ledger's backing tables for
-- @utxo.strategy: "from_ledger"@.
--
-- The set lives only in the LSM ledger tables, so it comes back through
-- a paged cursor rather than off the in-memory state.
module DbSync.Worker.Ledger.Utxo
  ( PinnedUtxo (..)
  , UtxoEntry (..)
  , pinUtxoAtOrAfter
  , unpinUtxo
  , streamUtxoPages
  , utxoPageSize
  ) where

import Cardano.Prelude

import qualified Cardano.Crypto.Hash as Crypto
import qualified Control.Concurrent.Class.MonadSTM.Strict as STM
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Strict.Maybe as Strict
import Data.SOP.Index (Index (..))

import Cardano.Ledger.BaseTypes (TxIx (..))
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.TxIn as Ledger

import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.HardFork.Combinator.Ledger (HasCanonicalTxIn (..))
import qualified Ouroboros.Consensus.Ledger.Abstract as Consensus
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Ledger.Tables (LedgerTables (..), ValuesMK (..))
import Ouroboros.Consensus.Shelley.Ledger.Ledger (BigEndianTxIn (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq as Consensus
  ( LedgerTablesHandle (..)
  )

import DbSync.Parser.Tx (fromLedgerUtxoTxOut)
import DbSync.Parser.Types (GenericTxOut)
import DbSync.Worker.Ledger.Types
  ( CardanoLedgerState (..)
  , DbSyncStateRef (..)
  , LedgerDB (..)
  , LedgerEnv (..)
  )

-- | One unspent output: the hash of the producing tx plus the output
-- itself, in the same shape the block parsers produce.
data UtxoEntry = UtxoEntry
  { ueTxHash :: !ByteString
  , ueTxOut  :: !GenericTxOut
  }

-- | A ledger checkpoint held open for reading. Until 'unpinUtxo', the
-- checkpoint pruner blocks instead of closing the handle mid-read.
data PinnedUtxo = PinnedUtxo
  { puState  :: !(ExtLedgerState (CardanoBlock StandardCrypto) Consensus.EmptyMK)
  , puTables :: !(Consensus.LedgerTablesHandle IO (ExtLedgerState (CardanoBlock StandardCrypto)))
  , puRef    :: !DbSyncStateRef
  , puSlot   :: !SlotNo
    -- ^ Tip slot of the pinned checkpoint. The caller must process its
    -- own block queue up to exactly this slot before reading the set.
  }

-- | Rows per page, matching the consensus default query batch size.
utxoPageSize :: Int
utxoPageSize = 100000

-- | Block until the worker's newest checkpoint has applied at least
-- @slot@, then pin that checkpoint against pruning.
--
-- Wait and pin happen in one transaction, so the pruner cannot close
-- the handle in between. The pinned tip ('puSlot') may sit past the
-- requested slot; the caller catches up to it before reading.
pinUtxoAtOrAfter :: LedgerEnv -> SlotNo -> IO PinnedUtxo
pinUtxoAtOrAfter env slot = STM.atomically $ do
  mDb <- STM.readTVar (leStateVar env)
  case mDb of
    Strict.Just (LedgerDB (ref StrictSeq.:<| _))
      | At w <- tipSlot ref
      , w >= slot -> do
          STM.writeTVar (srCanClose ref) False
          pure PinnedUtxo
            { puState  = clsState (srState ref)
            , puTables = srTables ref
            , puRef    = ref
            , puSlot   = w
            }
    _ -> STM.retry
  where
    tipSlot = Consensus.ledgerTipSlot . ledgerState . clsState . srState

unpinUtxo :: PinnedUtxo -> IO ()
unpinUtxo pinned = STM.atomically $ STM.writeTVar (srCanClose (puRef pinned)) True

-- | Page the whole set through the callback.
--
-- The resume key is the last key of the previous page, which
-- @readRange@ treats as exclusive. An empty page marks the end; the
-- returned key alone is not a reliable terminator.
streamUtxoPages :: PinnedUtxo -> ([UtxoEntry] -> IO ()) -> IO (Either Text ())
streamUtxoPages pinned emit = go Nothing
  where
    go !resumeFrom = do
      (LedgerTables (ValuesMK values), _lastKey) <-
        Consensus.readRange (puTables pinned) (puState pinned) (resumeFrom, utxoPageSize)
      if Map.null values
        then pure (Right ())
        else case traverse decodeEntry (Map.toList values) of
          Left err   -> pure (Left err)
          Right page -> do
            emit page
            go (Just (fst (Map.findMax values)))

-- | Force the reads here: the decoded fields must not retain the LSM
-- page they came from once it is consumed.
decodeEntry
  :: ( Consensus.TxIn (ExtLedgerState (CardanoBlock StandardCrypto))
     , Consensus.TxOut (ExtLedgerState (CardanoBlock StandardCrypto))
     )
  -> Either Text UtxoEntry
decodeEntry (txIn, txOut) =
  case fromLedgerUtxoTxOut ix txOut of
    Left err   -> Left err
    Right gout -> Right (UtxoEntry hash gout)
  where
    -- Every Shelley-based era shares one 'TxIn', so the index the
    -- canonical key is ejected at does not matter.
    Ledger.TxIn (Ledger.TxId safeHash) (TxIx ix) =
      getOriginalTxIn (ejectCanonicalTxIn (IS IZ) txIn)
    !hash = Crypto.hashToBytes (Ledger.extractHash safeHash)
