{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Periodic pipeline-backlog gauge for 'IngestChainHistory'.
--
-- A diagnostic thread that samples every inter-thread queue depth, the
-- per-epoch buffer sizes, and RTS live/in-use bytes on a fixed
-- interval and logs one Debug line under @"Gauge"@. Reads only, so it
-- does not perturb consumer throughput.
module DbSync.Phase.Ingest.Gauge
  ( withPipelineGauge
  ) where

import Cardano.Prelude

import Control.Concurrent.STM (lengthTBQueue)
import Control.Tracer (traceWith)
import Data.IORef (readIORef)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Text.Printf (printf)

import DbSync.App.Env (CoreEnv (..), IngestEnv (..))
import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Trace.Types (LogMsg (..), Severity (..))
import DbSync.Worker.Ledger.Types (HasLedgerEnv (..), LedgerEnv (..))
import DbSync.Worker.TxOut.AddressBuffer (addressBufferCounts)
import DbSync.Worker.TxOut.ConsumedByBuffer
  ( ConsumedByBufferRef
  , EpochConsumedByBuffer (..)
  )
import DbSync.Worker.TxOut.Worker (txOutWorkerDepths)

gaugeIntervalSeconds :: Int
gaugeIntervalSeconds = 5

-- | Run @act@ with a gauge thread alongside, only when severity admits
-- Debug. The gauge is cancelled when @act@ returns.
withPipelineGauge :: IngestEnv -> IO a -> IO a
withPipelineGauge ie act
  | ceMinSeverity (ieCore ie) <= Debug = withAsync (gaugeLoop ie) (const act)
  | otherwise                          = act

gaugeLoop :: IngestEnv -> IO ()
gaugeLoop ie = forever $ do
  threadDelay (gaugeIntervalSeconds * 1_000_000)
  sampleAndLog ie

sampleAndLog :: IngestEnv -> IO ()
sampleAndLog ie = do
  blkQ                <- atomically $ lengthTBQueue (ieBlockQueue ie)
  ledgerTxt           <- ledgerDepths (ieHasLedgerEnv ie)
  loaderTxt           <- loaderDepths (ieLoaderStream ie)
  (txInflight, txQ)   <- txOutWorkerDepths (ieTxOutWorker ie)
  (addrUniq, addrOut) <- addressBufferCounts <$> readIORef (ieAddressBuffer ie)
  cbCount             <- consumedByCount (ieConsumedByBuffer ie)
  rtsTxt              <- rtsDepths
  let line = Text.intercalate " | "
        [ "blkQ=" <> show blkQ
        , ledgerTxt
        , loaderTxt
        , "txout inflight=" <> show txInflight <> " q=" <> show txQ
        , "addrBuf out=" <> compact addrOut <> " uniq=" <> compact addrUniq
        , "cbBuf=" <> compact cbCount
        , rtsTxt
        ]
  traceWith (ceTracer (ieCore ie)) $ LogMsg Debug "Gauge" line

ledgerDepths :: HasLedgerEnv -> IO Text
ledgerDepths = \case
  LedgerDisabled _   -> pure "ledger=off"
  LedgerEnabled lenv -> atomically $ do
    lq <- lengthTBQueue (leLedgerQueue lenv)
    ba <- lengthTBQueue (leBlockApplyResults lenv)
    bd <- lengthTBQueue (leBoundaryApplyResults lenv)
    sq <- lengthTBQueue (leSnapshotQueue lenv)
    pure $ "ledgerQ=" <> show lq <> " blkApply=" <> show ba
        <> " bndApply=" <> show bd <> " snapQ=" <> show sq

loaderDepths :: LoaderStream -> IO Text
loaderDepths ls = do
  depths <- lsQueueDepths ls
  let totalChunks = sum [ cur | (_, cur, _) <- depths ]
      busy        = length [ () | (_, cur, _) <- depths, cur > 0 ]
      approxMb    = totalChunks `div` 16   -- 64KB chunks → /16 ≈ MB
  pure $ "loader=" <> show totalChunks <> "chk/~" <> show approxMb <> "MB busy="
      <> show busy <> "/" <> show (length depths)

consumedByCount :: Maybe ConsumedByBufferRef -> IO Int
consumedByCount = \case
  Nothing  -> pure 0
  Just ref -> Seq.length . ecbProducerTxOutIds <$> readIORef ref

rtsDepths :: IO Text
rtsDepths = do
  enabled <- getRTSStatsEnabled
  if not enabled
    then pure "rts=off"
    else do
      d <- gc <$> getRTSStats
      pure $ "live=" <> fmtGiB (gcdetails_live_bytes d)
          <> " inuse=" <> fmtGiB (gcdetails_mem_in_use_bytes d)

-- | Compact integer rendering: @4291714@ → @4.3M@.
compact :: Int -> Text
compact n
  | n >= 1_000_000 = Text.pack (printf "%.1fM" (fromIntegral n / 1e6 :: Double))
  | n >= 1_000     = Text.pack (printf "%.1fK" (fromIntegral n / 1e3 :: Double))
  | otherwise      = show n

fmtGiB :: Word64 -> Text
fmtGiB b = Text.pack (printf "%.1fG" (fromIntegral b / 1073741824 :: Double))
