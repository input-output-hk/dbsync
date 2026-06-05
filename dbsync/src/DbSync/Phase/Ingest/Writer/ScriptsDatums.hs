-- | COPY writers for tables owned by the @scripts_datums@ extractor.
module DbSync.Phase.Ingest.Writer.ScriptsDatums
  ( writeDatumCopy
  , writeScriptCopy
  , writeRedeemerCopy
  , writeRedeemerDataCopy
  , writeExtraKeyWitnessCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
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
  , datumTableDef
  , encodeDatumCopy
  , encodeExtraKeyWitnessCopy
  , encodeRedeemerCopy
  , encodeRedeemerDataCopy
  , encodeScriptCopy
  , extraKeyWitnessTableDef
  , redeemerDataTableDef
  , redeemerTableDef
  , scriptTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))

writeDatumCopy :: LoaderStream -> DatumId -> Datum -> IO ()
writeDatumCopy ls did d =
  lsWriteRow ls (tdName datumTableDef) (encodeDatumCopy did d)

writeScriptCopy :: LoaderStream -> ScriptId -> Script -> IO ()
writeScriptCopy ls sid s =
  lsWriteRow ls (tdName scriptTableDef) (encodeScriptCopy sid s)

writeRedeemerCopy :: LoaderStream -> RedeemerId -> Redeemer -> IO ()
writeRedeemerCopy ls rid r =
  lsWriteRow ls (tdName redeemerTableDef) (encodeRedeemerCopy rid r)

writeRedeemerDataCopy :: LoaderStream -> RedeemerDataId -> RedeemerData -> IO ()
writeRedeemerDataCopy ls rdid rd =
  lsWriteRow ls (tdName redeemerDataTableDef) (encodeRedeemerDataCopy rdid rd)

writeExtraKeyWitnessCopy :: LoaderStream -> ExtraKeyWitness -> IO ()
writeExtraKeyWitnessCopy ls ek =
  lsWriteRow ls (tdName extraKeyWitnessTableDef) (encodeExtraKeyWitnessCopy ek)
