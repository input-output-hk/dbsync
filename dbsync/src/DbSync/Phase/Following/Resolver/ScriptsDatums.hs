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

    -- * Buffered flavour
  , resolveDatumBuf
  , resolveScriptBuf
  , resolveRedeemerDataBuf
  , assignRedeemerIdBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Ids
  ( DatumId
  , RedeemerDataId
  , RedeemerId
  , ScriptId
  )
import DbSync.Db.Schema.ScriptsDatums (Datum, RedeemerData, Script)
import DbSync.Db.Statement.Datum (nextDatumIdStmt, queryDatumIdStmt)
import DbSync.Db.Statement.Redeemer (nextRedeemerIdStmt)
import DbSync.Db.Statement.RedeemerData
  ( nextRedeemerDataIdStmt
  , queryRedeemerDataIdStmt
  )
import DbSync.Db.Statement.Script (nextScriptIdStmt, queryScriptIdStmt)
import DbSync.Phase.Following.IdAllocator (PreAllocatedIds (..), popHead)
import DbSync.Phase.Following.Resolver.Internal
  ( BlockDedupCache (..)
  , resolveDedupSimple
  , runStmt
  )

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
