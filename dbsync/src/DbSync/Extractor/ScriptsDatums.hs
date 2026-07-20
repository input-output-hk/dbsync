{-# LANGUAGE OverloadedStrings #-}

-- | Extractor for Plutus and native-script witness data.
--
-- Populates five witness-derived tables: 'datum', 'script',
-- 'redeemer', 'redeemer_data', and 'extra_key_witness'. The
-- @datum@, @script@, and @redeemer_data@ rows are deduplicated on
-- their hash so the same payload referenced by multiple
-- transactions yields one row.
--
-- Failed Plutus scripts still have their witness sets recorded;
-- the @txValidContract@ gate is therefore not applied here.
--
-- == Known incomplete cells
--
-- * @redeemer.fee@ is always 'Nothing'. Populating it needs the
--   per-block 'apPrices' value multiplied by the redeemer's
--   exec units; that wiring is not yet in place.
-- * @redeemer.script_hash@ is always 'Nothing'. Resolving it means
--   following the redeemer pointer against the tx body's inputs /
--   certs / withdrawals / votes / proposals; that wiring is not yet
--   in place.
module DbSync.Extractor.ScriptsDatums
  ( scriptsDatumsExtractor
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.Ids (TxId)
import DbSync.Db.Schema.ScriptsDatums
import DbSync.Db.Types (DbLovelace)
import DbSync.Extractor
  ( BlockContext (..)
  , ExtractorDef (..)
  , ProcessBlockFn
  , TxContext (..)
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
import DbSync.Resolver (HasResolver (..), IdResolver (..))
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

-- Phase-2 failures are skipped: the ledger never applies an invalid
-- tx's witnesses, so its scripts, datums, and redeemers are not
-- on-chain data.
processScriptsDatums :: ProcessBlockFn
processScriptsDatums ctx =
  forM_ (bcTxs ctx) $ \tc -> when (txValidContract (tcGenTx tc)) $ do
    let txId = tcTxId tc
        gtx  = tcGenTx tc
    forM_ (txScripts gtx)           (writeScriptEntry txId)
    forM_ (txDatums gtx)            (writeDatumEntry txId)
    forM_ (txExtraKeyWitnesses gtx) (writeExtraKey txId)
    forM_ (txRedeemers gtx)         (writeRedeemerEntry txId)

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

writeRedeemerEntry
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => TxId -> GenericTxRedeemer -> m ()
writeRedeemerEntry txId gtr = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  rdId <- resolveAndWriteRedeemerData (gtrDataHash gtr) RedeemerData
    { redeemerDataHash  = gtrDataHash gtr
    , redeemerDataTxId  = txId
    , redeemerDataValue = gtrDataValue gtr
    , redeemerDataBytes = gtrDataBytes gtr
    }
  rid <- liftIO $ assignRedeemerId resolver
  liftIO $ writeRedeemer writer rid Redeemer
    { redeemerTxId            = txId
    , redeemerUnitMem         = gtrUnitMem gtr
    , redeemerUnitSteps       = gtrUnitSteps gtr
    , redeemerFee             = pendingFee
    , redeemerPurpose         = gtrPurpose gtr
    , redeemerIndex           = gtrIndex gtr
    , redeemerScriptHash      = gtrScriptHash gtr
    , redeemerRedeemerDataId  = rdId
    }

-- | Placeholder for @redeemer.fee@ until @apPrices@ flows through
-- the per-block ledger data.
pendingFee :: Maybe DbLovelace
pendingFee = Nothing
