-- | Follow 'IdResolver' fragments for the @utxo@ extractor.
module DbSync.Phase.Following.Resolver.UTxO
  ( -- * Shared between both flavours
    recordTxOutAddressFollow
  , recordCollateralTxOutAddressFollow
  , resolveInputValuesFollow
  , resolveInputUtxoFollow
  , recordTxOutputsFollow
  , recordConsumedFollow
  , deleteCachedUtxoFollow

    -- * Direct flavour
  , assignTxOutIdConn
  , assignCollateralTxOutIdConn
  , resolveAddressIdConn

    -- * Buffered flavour
  , assignTxOutIdBuf
  , assignCollateralTxOutIdBuf
  , resolveAddressIdBuf
  ) where

import Cardano.Prelude

import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map

import qualified Hasql.Connection as Conn
import qualified Hasql.Pipeline as Pipeline

import DbSync.Db.Schema.Address (Address)
import DbSync.Db.Schema.Ids (AddressId, CollateralTxOutId, TxId, TxOutId)
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
import DbSync.Phase.Following.IdAllocator (PreAllocatedIds (..), popHead)
import DbSync.Phase.Following.Resolver.Internal
  ( BlockDedupCache (..)
  , cacheInsert
  , runStmt
  )
import DbSync.Phase.Following.WriteBuffer (WriteBuffer, append)
import DbSync.Phase.Ingest.UtxoStore (UtxoTxEntry)

-- ---------------------------------------------------------------------------
-- * Shared (both flavours behave the same)
-- ---------------------------------------------------------------------------

-- | Follow extractors must call 'resolveAddressId' synchronously
-- (and write the tx_out row with @address_id@ pre-populated). The
-- async-worker entry points panic if reached.
recordTxOutAddressFollow :: TxOutId -> ByteString -> Address -> IO ()
recordTxOutAddressFollow _ _ _ =
  panic "Phase.Following.Resolver: recordTxOutAddress is Ingest-only"

recordCollateralTxOutAddressFollow :: CollateralTxOutId -> ByteString -> Address -> IO ()
recordCollateralTxOutAddressFollow _ _ _ =
  panic "Phase.Following.Resolver: recordCollateralTxOutAddress is Ingest-only"

-- | Inline value resolution: per-pair SELECT against tx_out.
resolveInputValuesFollow
  :: Conn.Connection -> [(ByteString, Word16)] -> IO [Maybe DbLovelace]
resolveInputValuesFollow conn pairs =
  forM pairs $ \pair -> runStmt conn pair queryOutputValueStmt

resolveInputUtxoFollow
  :: Conn.Connection -> ByteString -> Word16 -> IO (Maybe (TxId, TxOutId, DbLovelace))
resolveInputUtxoFollow conn hash idx = runStmt conn (hash, idx) queryInputUtxoStmt

-- | No-op in Follow: the in-process UTxO cache is an Ingest-only
-- structure (UtxoStore lives in the LSM session). Each Follow input
-- consults PG directly via 'resolveInputUtxoFollow'.
recordTxOutputsFollow :: ByteString -> UtxoTxEntry -> IO ()
recordTxOutputsFollow _ _ = pure ()

-- | No-op in Follow: @consumed_by_tx_id@ is filled inline at INSERT
-- time, not through the async buffer.
recordConsumedFollow :: TxOutId -> TxId -> IO ()
recordConsumedFollow _ _ = pure ()

-- | No-op in Follow: the UtxoStore is Ingest-only.
deleteCachedUtxoFollow :: ByteString -> Word16 -> IO ()
deleteCachedUtxoFollow _ _ = pure ()

-- ---------------------------------------------------------------------------
-- * Direct flavour
-- ---------------------------------------------------------------------------

assignTxOutIdConn :: Conn.Connection -> IO TxOutId
assignTxOutIdConn conn = runStmt conn () nextTxOutIdStmt

assignCollateralTxOutIdConn :: Conn.Connection -> IO CollateralTxOutId
assignCollateralTxOutIdConn conn = runStmt conn () nextCollateralTxOutIdStmt

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
