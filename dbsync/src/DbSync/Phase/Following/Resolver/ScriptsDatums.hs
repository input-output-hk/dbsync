-- | Follow 'IdResolver' fragments for the @scripts_datums@ extractor.
--
-- Follow-phase plumbing not landed for any of these fields; both
-- flavours use the same stubs.
module DbSync.Phase.Following.Resolver.ScriptsDatums
  ( resolveDatumStub
  , resolveScriptStub
  , resolveRedeemerDataStub
  , assignRedeemerIdStub
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.Ids
  ( DatumId
  , RedeemerDataId
  , RedeemerId
  , ScriptId
  )
import DbSync.Db.Schema.ScriptsDatums (Datum, RedeemerData, Script)
import DbSync.Phase.Following.Resolver.Internal (todoResolve)

resolveDatumStub :: ByteString -> Datum -> IO (DatumId, Bool)
resolveDatumStub _ _ = todoResolve "resolveDatum"

resolveScriptStub :: ByteString -> Script -> IO (ScriptId, Bool)
resolveScriptStub _ _ = todoResolve "resolveScript"

resolveRedeemerDataStub :: ByteString -> RedeemerData -> IO (RedeemerDataId, Bool)
resolveRedeemerDataStub _ _ = todoResolve "resolveRedeemerData"

assignRedeemerIdStub :: IO RedeemerId
assignRedeemerIdStub = todoResolve "assignRedeemerId"
