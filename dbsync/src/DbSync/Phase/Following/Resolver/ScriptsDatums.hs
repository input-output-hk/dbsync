-- | Follow 'IdResolver' fragments for the @scripts_datums@ extractor.
--
-- Three dedup-keyed tables (@datum@, @script@, @redeemer_data@)
-- resolve by their 32-byte hash; one counter-managed table
-- (@redeemer@) gets a fresh id per row.
module DbSync.Phase.Following.Resolver.ScriptsDatums
  ( -- * Direct flavour
    resolveDatumConn
  , resolveScriptConn
  , resolveRedeemerDataConn
  , assignRedeemerIdConn
  , fillSpendScriptHashesConn

    -- * Buffered flavour
  , resolveDatumBuf
  , resolveScriptBuf
  , resolveRedeemerDataBuf
  , assignRedeemerIdBuf
  , fillSpendScriptHashesBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Pipeline as Pipeline

import DbSync.Db.Schema.Ids
  ( DatumId
  , RedeemerDataId
  , RedeemerId
  , ScriptId
  )
import DbSync.Db.Schema.ScriptsDatums (Datum, RedeemerData, Script)
import DbSync.Db.Statement.ScriptsDatums (nextDatumIdStmt, queryDatumIdStmt)
import DbSync.Db.Statement.ScriptsDatums (nextRedeemerIdStmt)
import DbSync.Db.Statement.ScriptsDatums
  ( nextRedeemerDataIdStmt
  , queryRedeemerDataIdStmt
  )
import DbSync.Db.Statement.ScriptsDatums (nextScriptIdStmt, queryScriptIdStmt)
import DbSync.Db.Statement.Worker.RedeemerScriptHash (fillSpendScriptHashesStmt)
import DbSync.Phase.Following.IdAllocator (PreAllocatedIds (..), popHead)
import DbSync.Phase.Following.Resolver.Internal
  ( BlockDedupCache (..)
  , resolveDedupSimple
  , runStmt
  )
import DbSync.Phase.Following.WriteBuffer (WriteBuffer, append)

-- ---------------------------------------------------------------------------
-- * Direct flavour
-- ---------------------------------------------------------------------------

resolveDatumConn :: Conn.Connection -> ByteString -> Datum -> IO (DatumId, Bool)
resolveDatumConn conn hash _row = do
  mId <- runStmt conn hash queryDatumIdStmt
  case mId of
    Just did -> pure (did, False)
    Nothing  -> do
      did <- runStmt conn () nextDatumIdStmt
      pure (did, True)

resolveScriptConn :: Conn.Connection -> ByteString -> Script -> IO (ScriptId, Bool)
resolveScriptConn conn hash _row = do
  mId <- runStmt conn hash queryScriptIdStmt
  case mId of
    Just sid -> pure (sid, False)
    Nothing  -> do
      sid <- runStmt conn () nextScriptIdStmt
      pure (sid, True)

resolveRedeemerDataConn
  :: Conn.Connection -> ByteString -> RedeemerData -> IO (RedeemerDataId, Bool)
resolveRedeemerDataConn conn hash _row = do
  mId <- runStmt conn hash queryRedeemerDataIdStmt
  case mId of
    Just rdid -> pure (rdid, False)
    Nothing   -> do
      rdid <- runStmt conn () nextRedeemerDataIdStmt
      pure (rdid, True)

assignRedeemerIdConn :: Conn.Connection -> IO RedeemerId
assignRedeemerIdConn conn = runStmt conn () nextRedeemerIdStmt

fillSpendScriptHashesConn :: Conn.Connection -> [RedeemerId] -> IO ()
fillSpendScriptHashesConn conn ids = runStmt conn ids fillSpendScriptHashesStmt

-- ---------------------------------------------------------------------------
-- * Buffered flavour
-- ---------------------------------------------------------------------------

resolveDatumBuf
  :: Conn.Connection -> BlockDedupCache -> ByteString -> Datum -> IO (DatumId, Bool)
resolveDatumBuf conn cache hash _row =
  resolveDedupSimple
    conn
    hash
    (bdcDatum cache)
    queryDatumIdStmt
    nextDatumIdStmt

resolveScriptBuf
  :: Conn.Connection -> BlockDedupCache -> ByteString -> Script -> IO (ScriptId, Bool)
resolveScriptBuf conn cache hash _row =
  resolveDedupSimple
    conn
    hash
    (bdcScript cache)
    queryScriptIdStmt
    nextScriptIdStmt

resolveRedeemerDataBuf
  :: Conn.Connection
  -> BlockDedupCache
  -> ByteString
  -> RedeemerData
  -> IO (RedeemerDataId, Bool)
resolveRedeemerDataBuf conn cache hash _row =
  resolveDedupSimple
    conn
    hash
    (bdcRedeemerData cache)
    queryRedeemerDataIdStmt
    nextRedeemerDataIdStmt

assignRedeemerIdBuf :: PreAllocatedIds -> IO RedeemerId
assignRedeemerIdBuf preAlloc = popHead "assignRedeemerId" (paiRedeemerIds preAlloc)

-- | Queued last in the block's pipeline, so it sees the @tx_in@,
-- @tx_out@ and @address@ INSERTs of a same-block script spend.
fillSpendScriptHashesBuf :: WriteBuffer -> [RedeemerId] -> IO ()
fillSpendScriptHashesBuf buf ids =
  append buf (Pipeline.statement ids fillSpendScriptHashesStmt)
