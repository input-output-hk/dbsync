{-# LANGUAGE OverloadedStrings #-}

-- | Plutus and native-script witness data: @datum@, @script@,
-- @redeemer@, @redeemer_data@ and @extra_key_witness@. Dedup on hash
-- gives @datum@, @script@ and @redeemer_data@ one row per payload,
-- however many transactions reference it.
--
-- A spend redeemer's @script_hash@ lives on the output the tx unlocks,
-- not in the tx itself, so 'DbSync.Resolver.fillSpendScriptHashes'
-- fills it later.
module DbSync.Extractor.ScriptsDatums
  ( scriptsDatumsExtractor
  , redeemerScriptFee
  ) where

import Cardano.Prelude

import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Prices, txscriptfee)

import DbSync.Db.Schema.Ids (RedeemerId, TxId)
import DbSync.Db.Schema.ScriptsDatums
import DbSync.Db.Types (DbLovelace)
import DbSync.Extractor
  ( BlockContext (..)
  , ExtractorDef (..)
  , ProcessBlockFn
  , TxContext (..)
  , blockPrices
  )
import DbSync.Extractor.SharedDedup
  ( resolveAndWriteDatum
  , resolveAndWriteRedeemerData
  , resolveAndWriteTxScript
  )
import DbSync.Parser.Types
  ( GenericTx (..)
  , GenericTxDatum (..)
  , GenericTxRedeemer (..)
  , GenericTxScript (..)
  )
import DbSync.Resolver (HasResolver)
import DbSync.Util (coinToDbLovelace)
import DbSync.Writer (HasWriter (..), Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

scriptsDatumsExtractor :: ExtractorDef
scriptsDatumsExtractor = ExtractorDef
  { pdName    = "scripts_datums"
  , pdTables  =
      [ datumTableDef
      , scriptTableDef
      , redeemerTableDef
      , redeemerDataTableDef
      , extraKeyWitnessTableDef
      ]
  , pdProcess = processScriptsDatums
  }

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

-- A phase-2 failure is skipped: the ledger never applies an invalid
-- tx's witnesses, so its scripts, datums and redeemers are not
-- on-chain data.
processScriptsDatums :: ProcessBlockFn
processScriptsDatums ctx = do
  let mPrices = blockPrices (bcLedgerData ctx)
  forM_ (bcTxs ctx) $ \tc -> when (txValidContract (tcGenTx tc)) $ do
    let txId = tcTxId tc
        gtx  = tcGenTx tc
    forM_ (txScripts gtx)           (writeScriptEntry txId)
    forM_ (txDatums gtx)            (writeDatumEntry txId)
    forM_ (txExtraKeyWitnesses gtx) (writeExtraKey txId)
    forM_ (zip (tcRedeemerIds tc) (txRedeemers gtx)) $
      uncurry (writeRedeemerEntry mPrices txId)

writeScriptEntry
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => TxId -> GenericTxScript -> m ()
writeScriptEntry txId gts = void $ resolveAndWriteTxScript txId gts

writeDatumEntry
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => TxId -> GenericTxDatum -> m ()
writeDatumEntry txId gtd = do
  let row = Datum
        { datumHash  = gtdHash gtd
        , datumTxId  = txId
        , datumValue = gtdValue gtd
        , datumBytes = gtdBytes gtd
        }
  _ <- resolveAndWriteDatum (gtdHash gtd) row
  pure ()

-- | @extra_key_witness.id@ is a PostgreSQL identity column, so the
-- row is dispatched without an in-process ID assignment.
writeExtraKey
  :: (HasWriter env, MonadReader env m, MonadIO m)
  => TxId -> ByteString -> m ()
writeExtraKey txId h = do
  writer <- asks getWriter
  liftIO $ writeExtraKeyWitness writer ExtraKeyWitness
    { extraKeyWitnessHash = h
    , extraKeyWitnessTxId = txId
    }

-- | @redeemer.fee@ needs the block's Plutus execution prices, so
-- 'Nothing' (ledger off) leaves that cell NULL. The pipeline
-- pre-assigns the row id, so FK writers in other extractors reference
-- it whatever the order.
writeRedeemerEntry
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => Maybe Prices -> TxId -> RedeemerId -> GenericTxRedeemer -> m ()
writeRedeemerEntry mPrices txId rid gtr = do
  writer <- asks getWriter
  rdId <- resolveAndWriteRedeemerData (gtrDataHash gtr) RedeemerData
    { redeemerDataHash  = gtrDataHash gtr
    , redeemerDataTxId  = txId
    , redeemerDataValue = gtrDataValue gtr
    , redeemerDataBytes = gtrDataBytes gtr
    }
  -- Forced now so the Follow write buffer stores a value, not a closure.
  let fee = case mPrices of
        Nothing -> Nothing
        Just p  -> Just $! redeemerScriptFee p (gtrUnitMem gtr) (gtrUnitSteps gtr)
  liftIO $ writeRedeemer writer rid Redeemer
    { redeemerTxId            = txId
    , redeemerUnitMem         = gtrUnitMem gtr
    , redeemerUnitSteps       = gtrUnitSteps gtr
    , redeemerFee             = fee
    , redeemerPurpose         = gtrPurpose gtr
    , redeemerIndex           = gtrIndex gtr
    , redeemerScriptHash      = gtrScriptHash gtr
    , redeemerRedeemerDataId  = rdId
    }

redeemerScriptFee :: Prices -> Word64 -> Word64 -> DbLovelace
redeemerScriptFee prices mem steps =
  coinToDbLovelace (txscriptfee prices (ExUnits (fromIntegral mem) (fromIntegral steps)))
