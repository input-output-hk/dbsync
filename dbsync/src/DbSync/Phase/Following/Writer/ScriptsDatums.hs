-- | hasql writers for tables owned by the @scripts_datums@ extractor.
--
-- @datum@, @script@, @redeemer_data@ are dedup-keyed; @redeemer@ is
-- counter-managed; all four take a caller-allocated id. The
-- @extra_key_witness@ leaf is IDENTITY-managed so the writer takes
-- just the row.
module DbSync.Phase.Following.Writer.ScriptsDatums
  ( writeDatumConn
  , writeDatumBuf
  , writeScriptConn
  , writeScriptBuf
  , writeRedeemerConn
  , writeRedeemerBuf
  , writeRedeemerDataConn
  , writeRedeemerDataBuf
  , writeExtraKeyWitnessConn
  , writeExtraKeyWitnessBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Ids
  ( DatumId
  , RedeemerDataId
  , RedeemerId
  , ScriptId
  )
import DbSync.Db.Schema.ScriptsDatums
  ( Datum
  , ExtraKeyWitness
  , Redeemer
  , RedeemerData
  , Script
  )
import DbSync.Db.Statement.Datum (insertDatumRowStmt)
import DbSync.Db.Statement.ExtraKeyWitness (insertExtraKeyWitnessRowStmt)
import DbSync.Db.Statement.Redeemer (insertRedeemerRowStmt)
import DbSync.Db.Statement.RedeemerData (insertRedeemerDataRowStmt)
import DbSync.Db.Statement.Script (insertScriptRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

writeDatumConn :: Conn.Connection -> DatumId -> Datum -> IO ()
writeDatumConn conn did d = runConn conn (did, d) insertDatumRowStmt

writeDatumBuf :: WriteBuffer -> DatumId -> Datum -> IO ()
writeDatumBuf buf did d = queueBuf buf (did, d) insertDatumRowStmt

writeScriptConn :: Conn.Connection -> ScriptId -> Script -> IO ()
writeScriptConn conn sid s = runConn conn (sid, s) insertScriptRowStmt

writeScriptBuf :: WriteBuffer -> ScriptId -> Script -> IO ()
writeScriptBuf buf sid s = queueBuf buf (sid, s) insertScriptRowStmt

writeRedeemerConn :: Conn.Connection -> RedeemerId -> Redeemer -> IO ()
writeRedeemerConn conn rid r = runConn conn (rid, r) insertRedeemerRowStmt

writeRedeemerBuf :: WriteBuffer -> RedeemerId -> Redeemer -> IO ()
writeRedeemerBuf buf rid r = queueBuf buf (rid, r) insertRedeemerRowStmt

writeRedeemerDataConn :: Conn.Connection -> RedeemerDataId -> RedeemerData -> IO ()
writeRedeemerDataConn conn rdid rd = runConn conn (rdid, rd) insertRedeemerDataRowStmt

writeRedeemerDataBuf :: WriteBuffer -> RedeemerDataId -> RedeemerData -> IO ()
writeRedeemerDataBuf buf rdid rd = queueBuf buf (rdid, rd) insertRedeemerDataRowStmt

writeExtraKeyWitnessConn :: Conn.Connection -> ExtraKeyWitness -> IO ()
writeExtraKeyWitnessConn conn ekw = runConn conn ekw insertExtraKeyWitnessRowStmt

writeExtraKeyWitnessBuf :: WriteBuffer -> ExtraKeyWitness -> IO ()
writeExtraKeyWitnessBuf buf ekw = queueBuf buf ekw insertExtraKeyWitnessRowStmt
