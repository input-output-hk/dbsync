-- | hasql writers for tables owned by the @scripts_datums@
-- extractor. All flavours panic via 'todoWrite' until the insert
-- statements are wired.
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
  ( Datum, ExtraKeyWitness, Redeemer, RedeemerData, Script )
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (todoWrite, todoWriteLeaf)

writeDatumConn :: Conn.Connection -> DatumId -> Datum -> IO ()
writeDatumConn _ = todoWrite "writeDatum"

writeDatumBuf :: WriteBuffer -> DatumId -> Datum -> IO ()
writeDatumBuf _ = todoWrite "writeDatum"

writeScriptConn :: Conn.Connection -> ScriptId -> Script -> IO ()
writeScriptConn _ = todoWrite "writeScript"

writeScriptBuf :: WriteBuffer -> ScriptId -> Script -> IO ()
writeScriptBuf _ = todoWrite "writeScript"

writeRedeemerConn :: Conn.Connection -> RedeemerId -> Redeemer -> IO ()
writeRedeemerConn _ = todoWrite "writeRedeemer"

writeRedeemerBuf :: WriteBuffer -> RedeemerId -> Redeemer -> IO ()
writeRedeemerBuf _ = todoWrite "writeRedeemer"

writeRedeemerDataConn :: Conn.Connection -> RedeemerDataId -> RedeemerData -> IO ()
writeRedeemerDataConn _ = todoWrite "writeRedeemerData"

writeRedeemerDataBuf :: WriteBuffer -> RedeemerDataId -> RedeemerData -> IO ()
writeRedeemerDataBuf _ = todoWrite "writeRedeemerData"

writeExtraKeyWitnessConn :: Conn.Connection -> ExtraKeyWitness -> IO ()
writeExtraKeyWitnessConn _ = todoWriteLeaf "writeExtraKeyWitness"

writeExtraKeyWitnessBuf :: WriteBuffer -> ExtraKeyWitness -> IO ()
writeExtraKeyWitnessBuf _ = todoWriteLeaf "writeExtraKeyWitness"
