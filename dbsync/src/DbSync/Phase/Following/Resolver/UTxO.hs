-- | Follow 'IdResolver' fragments for the @utxo@ extractor.
module DbSync.Phase.Following.Resolver.UTxO
  ( -- * Shared between both flavours
    ConsumedTracking (..)
  , recordTxOutAddressFollow
  , recordCollateralTxOutAddressFollow
  , deleteCachedUtxoFollow

    -- * Direct flavour
  , assignTxOutIdConn
  , assignCollateralTxOutIdConn
  , resolveAddressIdConn
  , resolveInputValuesConn
  , resolveInputUtxoConn
  , recordTxOutputsConn
  , recordConsumedConn

    -- * Buffered flavour
  , assignTxOutIdBuf
  , assignCollateralTxOutIdBuf
  , resolveAddressIdBuf
  , resolveInputValuesBuf
  , resolveInputUtxoBuf
  , recordTxOutputsBuf
  , recordConsumedBuf
  ) where

import Cardano.Prelude

import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import qualified Hasql.Connection as Conn
import qualified Hasql.Pipeline as Pipeline

import DbSync.Db.Schema.Address (Address)
import DbSync.Db.Schema.Ids (AddressId, CollateralTxOutId, StakeAddressId, TxId, TxOutId)
import DbSync.Db.Types (DbLovelace)
import DbSync.Db.Statement.UTxO
  ( insertAddressRowStmt
  , nextAddressIdStmt
  , queryAddressIdStmt
  )
import DbSync.Db.Statement.UTxO (nextCollateralTxOutIdStmt)
import DbSync.Db.Statement.UTxO
  ( nextTxOutIdStmt
  , queryInputUtxoStmt
  , queryOutputValueStmt
  )
import DbSync.Db.Statement.Worker.ConsumedBy (updateConsumedByTxIdStmt)
import DbSync.Phase.Following.IdAllocator (PreAllocatedIds (..), popHead)
import DbSync.Phase.Following.Resolver.Internal
  ( BlockDedupCache (..)
  , cacheInsert
  , runStmt
  )
import DbSync.Phase.Following.WriteBuffer (WriteBuffer, append)
import DbSync.Phase.Ingest.UtxoStore (UtxoTxEntry (..))

-- ---------------------------------------------------------------------------
-- * Shared (both flavours behave the same)
-- ---------------------------------------------------------------------------

-- | Whether the resolver mirrors spends into
-- @tx_out.consumed_by_tx_id@ (the @utxo.consumed_by_tx_id@ option).
data ConsumedTracking = TrackConsumedBy | SkipConsumedBy
  deriving stock (Eq, Show)

-- | Follow extractors must call 'resolveAddressId' synchronously
-- (and write the tx_out row with @address_id@ pre-populated). The
-- async-worker entry points panic if reached.
recordTxOutAddressFollow :: TxOutId -> ByteString -> Maybe StakeAddressId -> IO ()
recordTxOutAddressFollow _ _ _ =
  panic "Phase.Following.Resolver: recordTxOutAddress is Ingest-only"

recordCollateralTxOutAddressFollow :: CollateralTxOutId -> ByteString -> Maybe StakeAddressId -> IO ()
recordCollateralTxOutAddressFollow _ _ _ =
  panic "Phase.Following.Resolver: recordCollateralTxOutAddress is Ingest-only"

-- | No-op in Follow: the LSM-backed UtxoStore is Ingest-only, and
-- the buffered flavour's block-local map is dropped whole at COMMIT
-- (an output spent in this block cannot be spent again within it).
deleteCachedUtxoFollow :: ByteString -> Word16 -> IO ()
deleteCachedUtxoFollow _ _ = pure ()

-- ---------------------------------------------------------------------------
-- * Direct flavour
-- ---------------------------------------------------------------------------

assignTxOutIdConn :: Conn.Connection -> IO TxOutId
assignTxOutIdConn conn = runStmt conn () nextTxOutIdStmt

assignCollateralTxOutIdConn :: Conn.Connection -> IO CollateralTxOutId
assignCollateralTxOutIdConn conn = runStmt conn () nextCollateralTxOutIdStmt

-- | Per-pair SELECT against tx_out. Direct writes execute
-- immediately on this connection, so same-block producers are
-- already visible to the SELECT.
resolveInputValuesConn
  :: Conn.Connection -> [(ByteString, Word16)] -> IO [Maybe DbLovelace]
resolveInputValuesConn conn pairs =
  forM pairs $ \pair -> runStmt conn pair queryOutputValueStmt

resolveInputUtxoConn
  :: Conn.Connection -> ByteString -> Word16 -> IO (Maybe (TxId, TxOutId, DbLovelace))
resolveInputUtxoConn conn hash idx = runStmt conn (hash, idx) queryInputUtxoStmt

-- | No-op: the direct flavour's INSERTs are visible to its own
-- SELECTs, so 'resolveInputUtxoConn' needs no block-local map.
recordTxOutputsConn :: ByteString -> UtxoTxEntry -> IO ()
recordTxOutputsConn _ _ = pure ()

-- | Mark the producer row consumed with an inline UPDATE.
recordConsumedConn :: Conn.Connection -> ConsumedTracking -> TxOutId -> TxId -> IO ()
recordConsumedConn conn tracking !producerOutId !consumerTxId = case tracking of
  SkipConsumedBy  -> pure ()
  TrackConsumedBy ->
    runStmt conn (producerOutId, consumerTxId) updateConsumedByTxIdStmt

-- | SELECT-by-bytes; on miss, allocate from the sequence and run the
-- @address@ INSERT inline. Used by the direct (un-buffered) resolver.
resolveAddressIdConn :: Conn.Connection -> ByteString -> Address -> IO AddressId
resolveAddressIdConn conn rawBytes addr = do
  mId <- runStmt conn rawBytes queryAddressIdStmt
  case mId of
    Just aid -> pure aid
    Nothing  -> do
      aid <- runStmt conn () nextAddressIdStmt
      runStmt conn (aid, addr) insertAddressRowStmt
      pure aid

-- ---------------------------------------------------------------------------
-- * Buffered flavour
-- ---------------------------------------------------------------------------

assignTxOutIdBuf :: PreAllocatedIds -> IO TxOutId
assignTxOutIdBuf preAlloc = popHead "assignTxOutId" (paiTxOutIds preAlloc)

assignCollateralTxOutIdBuf :: PreAllocatedIds -> IO CollateralTxOutId
assignCollateralTxOutIdBuf preAlloc = popHead "assignCollateralTxOutId" (paiCollateralTxOutIds preAlloc)

-- | Same shape as 'resolveAddressIdConn' but the INSERT (when new)
-- is queued on the 'WriteBuffer' instead of running inline. The
-- per-block cache shadows the not-yet-flushed row so a sibling
-- resolve within the block finds the id without re-querying PG.
resolveAddressIdBuf
  :: Conn.Connection
  -> WriteBuffer
  -> BlockDedupCache
  -> ByteString
  -> Address
  -> IO AddressId
resolveAddressIdBuf conn buf cache rawBytes addr = do
  let mapRef = bdcAddress cache
  m <- readIORef mapRef
  case Map.lookup rawBytes m of
    Just aid -> pure aid
    Nothing -> do
      mId <- runStmt conn rawBytes queryAddressIdStmt
      case mId of
        Just aid -> do
          cacheInsert mapRef rawBytes aid
          pure aid
        Nothing -> do
          aid <- runStmt conn () nextAddressIdStmt
          cacheInsert mapRef rawBytes aid
          append buf (Pipeline.statement (aid, addr) insertAddressRowStmt)
          pure aid

-- | Per-pair lookup: block-local map first (the producing INSERT may
-- still be unflushed in the 'WriteBuffer'), then PG.
resolveInputValuesBuf
  :: Conn.Connection
  -> BlockDedupCache
  -> [(ByteString, Word16)]
  -> IO [Maybe DbLovelace]
resolveInputValuesBuf conn cache pairs = do
  m <- readIORef (bdcUtxo cache)
  forM pairs $ \pair@(hash, idx) ->
    case lookupBlockLocal m hash idx of
      Just (_, _, val) -> pure (Just val)
      Nothing          -> runStmt conn pair queryOutputValueStmt

resolveInputUtxoBuf
  :: Conn.Connection
  -> BlockDedupCache
  -> ByteString
  -> Word16
  -> IO (Maybe (TxId, TxOutId, DbLovelace))
resolveInputUtxoBuf conn cache hash idx = do
  m <- readIORef (bdcUtxo cache)
  case lookupBlockLocal m hash idx of
    Just hit -> pure (Just hit)
    Nothing  -> runStmt conn (hash, idx) queryInputUtxoStmt

-- | Record the tx's outputs in the block-local map so later inputs
-- in the same block resolve without a PG round-trip. The bangs keep
-- the map from retaining the pipeline's closures.
recordTxOutputsBuf :: BlockDedupCache -> ByteString -> UtxoTxEntry -> IO ()
recordTxOutputsBuf cache !hash !entry = cacheInsert (bdcUtxo cache) hash entry

-- | Queue the consumed-by UPDATE on the per-block pipeline; it runs
-- after the buffered INSERT of a same-block producer. The bangs keep
-- the ids out of the buffered closure as thunks.
recordConsumedBuf :: WriteBuffer -> ConsumedTracking -> TxOutId -> TxId -> IO ()
recordConsumedBuf buf tracking !producerOutId !consumerTxId = case tracking of
  SkipConsumedBy  -> pure ()
  TrackConsumedBy ->
    append buf (Pipeline.statement (producerOutId, consumerTxId) updateConsumedByTxIdStmt)

-- Block-local producer lookup: (tx hash, output index) into the
-- outputs the pipeline recorded for this block.
lookupBlockLocal
  :: Map ByteString UtxoTxEntry
  -> ByteString
  -> Word16
  -> Maybe (TxId, TxOutId, DbLovelace)
lookupBlockLocal m hash idx = do
  entry <- Map.lookup hash m
  (outId, val) <- Seq.lookup (fromIntegral idx) (uteOutputs entry)
  pure (uteTxId entry, outId, val)
