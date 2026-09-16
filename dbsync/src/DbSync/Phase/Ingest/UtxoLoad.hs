{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Materialises @tx_out@ from the ledger's UTxO set, for
-- @utxo.strategy: "from_ledger"@.
--
-- Runs as the last step of Ingest, while the loader stream, dedup
-- stores and id counters are still open, so rows go out through the
-- ordinary writer path rather than a second insert route.
module DbSync.Phase.Ingest.UtxoLoad
  ( loadUtxoFromLedger
  ) where

import Cardano.Prelude

import Control.Monad.IO.Unlift (withRunInIO)
import Control.Tracer (traceWith)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as TE

import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import DbSync.App.Config.Types (Extractors (..), OptionFlag (..), SyncConfig (..))
import DbSync.App.Env (HasConfig (..), IngestEnv (..))
import DbSync.AppM (IngestM)
import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.Ids (TxId, TxOutId)
import DbSync.Db.Schema.MultiAsset (MaTxOut (..))
import DbSync.Db.Types (DbWord64 (..))
import DbSync.Extractor.SharedDedup
  ( resolveAndWriteDatum
  , resolveAndWriteMultiAsset
  , resolveAndWriteTxScript
  , resolveStakeCred
  )
import DbSync.Extractor.UTxO (extractStakeCred, mkDatum, mkTxOut)
import DbSync.Parser.Types (GenericTxDatum (..), GenericTxOut)
import qualified DbSync.Parser.Types as G
import qualified DbSync.Phase.Ingest.Writer as IngestWriter
import DbSync.Phase.Ingest.UtxoStore (UtxoStore, lookupInput)
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (LogMsg (..), Severity (..))
import DbSync.Worker.Ledger.Utxo (PinnedUtxo (..), UtxoEntry (..), streamUtxoPages)
import qualified DbSync.Worker.TxOut.AddressBuffer as AddressBuffer
import DbSync.Worker.TxOut.Worker (TxOutJob (..), awaitTxOutDrained, enqueueTxOutJob)
import DbSync.Writer (Writer (..))

-- | Commit cadence: pages per loader-stream commit + address-worker job.
commitEveryPages :: Int
commitEveryPages = 10

-- | Write one @tx_out@ row per unspent output in the pinned ledger
-- state, plus their @ma_tx_out@ rows when the multi-asset extractor is
-- enabled.
--
-- The pinned tip must be the last block committed to PostgreSQL: which
-- outputs are live is taken from the set at exactly that point, so any
-- other one silently disagrees with the committed rows. For the same
-- reason an output whose producing tx is missing from 'UtxoStore' is an
-- invariant violation, not a skip.
--
-- Rows go out through a writer without the from_ledger silencing, in
-- chunks: every 'commitEveryPages' pages the loader stream commits and
-- the accumulated address pairs go to the tx_out worker, whose bulk
-- @address_id@ UPDATE only hits committed rows.
loadUtxoFromLedger :: PinnedUtxo -> IngestM ()
loadUtxoFromLedger pinned = do
  ie        <- ask
  tracer    <- asks getTracer
  maEnabled <- asks (prEnabled . exMultiAsset . scExtractors . getConfig)
  let rawWriter = IngestWriter.mkWriter (ieLoaderStream ie)
  liftIO . traceWith tracer $ LogMsg Info "Ingest" $
    "from_ledger: loading UTxO set at slot " <> show (unSlotNo (puSlot pinned))
  pageCount <- liftIO $ newIORef (0 :: Int)
  rowCount  <- liftIO $ newIORef (0 :: Int)
  result <- withRunInIO $ \runInIO ->
    streamUtxoPages pinned $ \page -> do
      runInIO $ writePage rawWriter (ieUtxoStore ie) maEnabled page
      modifyIORef' rowCount (+ length page)
      pages <- atomicModifyIORef' pageCount (\n -> (n + 1, n + 1))
      when (pages `mod` commitEveryPages == 0) $ flushChunk ie
  case result of
    Left err -> panic ("utxo.strategy \"from_ledger\": " <> err)
    Right () -> do
      liftIO $ flushChunk ie
      liftIO $ awaitTxOutDrained (ieTxOutWorker ie)
      rows <- liftIO $ readIORef rowCount
      liftIO . traceWith tracer $ LogMsg Info "Ingest" $
        "from_ledger: loaded " <> show rows <> " tx_out rows"

-- | Commit the loader stream, then hand the address pairs of the
-- committed rows to the tx_out worker.
flushChunk :: IngestEnv -> IO ()
flushChunk ie = do
  lsCommit (ieLoaderStream ie)
  buf <- AddressBuffer.takeAndReset (ieAddressBuffer ie)
  enqueueTxOutJob (ieTxOutWorker ie) TxOutJob
    { tjEpoch      = EpochNo 0
    , tjAddress    = buf
    , tjConsumedBy = Nothing
    }
  lsReopen (ieLoaderStream ie)

writePage :: Writer IO -> UtxoStore -> Bool -> [UtxoEntry] -> IngestM ()
writePage rawWriter utxoStore maEnabled = traverse_ $ \entry -> do
  let gout = ueTxOut entry
  mProducer <- liftIO $ lookupInput utxoStore (ueTxHash entry) (G.txOutIndex gout)
  case mProducer of
    Nothing -> panic $ mconcat
      [ "utxo.strategy \"from_ledger\": ledger UTxO entry "
      , TE.decodeUtf8 (Base16.encode (ueTxHash entry))
      , "#", show (G.txOutIndex gout)
      , " has no producing tx in the UTxO store; the pinned ledger state "
      , "disagrees with the committed tx rows"
      ]
    Just (txId, outId, _value) -> writeOutput rawWriter maEnabled txId outId gout

-- | Mirrors the output arm of 'DbSync.Extractor.UTxO.processUTxO':
-- the row goes in with @address_id = NULL@ and the pair is recorded for
-- the worker's bulk UPDATE.
writeOutput :: Writer IO -> Bool -> TxId -> TxOutId -> GenericTxOut -> IngestM ()
writeOutput rawWriter maEnabled txId outId gout = do
  resolver  <- asks getResolver
  mStakeId  <- resolveStake
  mInlineId <- resolveInlineDatum
  mRefSid   <- resolveRefScript
  let raw = G.txOutAddressRaw gout
  liftIO $ writeTxOut rawWriter outId
    (mkTxOut txId Nothing mStakeId mInlineId mRefSid gout)
  liftIO $ recordTxOutAddress resolver outId raw mStakeId
  when maEnabled $
    for_ (G.txOutMultiAssets gout) $ \(policy, name, quantity) -> do
      maId <- resolveAndWriteMultiAsset policy name
      liftIO $ writeMaTxOut rawWriter MaTxOut
        { maTxOutQuantity = DbWord64 (fromIntegral quantity)
        , maTxOutTxOutId  = outId
        , maTxOutIdent    = maId
        }
  where
    resolveStake = case extractStakeCred (G.txOutAddressRaw gout) of
      Nothing   -> pure Nothing
      Just cred -> Just <$> resolveStakeCred cred

    resolveInlineDatum = case G.txOutInlineDatum gout of
      Nothing  -> pure Nothing
      Just gtd -> Just <$> resolveAndWriteDatum (gtdHash gtd) (mkDatum txId gtd)

    resolveRefScript = case G.txOutRefScript gout of
      Nothing  -> pure Nothing
      Just gts -> do
        !sid <- resolveAndWriteTxScript txId gts
        pure (Just sid)
