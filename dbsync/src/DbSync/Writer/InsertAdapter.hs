{-# LANGUAGE OverloadedStrings #-}

-- | FollowingChainTip writer: typed records to hasql INSERTs against
-- a single connection. IDs come from the resolver; this layer only
-- writes.
--
-- Tables outside the slice-A surface (block, slot_leader) panic;
-- they're filled in as their extractors land in tests.
module DbSync.Writer.InsertAdapter
  ( mkInsertWriter
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Statement.Address (insertAddressRowStmt)
import DbSync.Db.Statement.Block (insertBlockRowStmt)
import DbSync.Db.Statement.CollateralTxIn (insertCollateralTxInRowStmt)
import DbSync.Db.Statement.Delegation (insertDelegationRowStmt)
import DbSync.Db.Statement.MaTxMint (insertMaTxMintRowStmt)
import DbSync.Db.Statement.MaTxOut (insertMaTxOutRowStmt)
import DbSync.Db.Statement.MultiAsset (insertMultiAssetRowStmt)
import DbSync.Db.Statement.PoolHash (insertPoolHashRowStmt)
import DbSync.Db.Statement.PoolMetadataRef (insertPoolMetadataRefRowStmt)
import DbSync.Db.Statement.PoolOwner (insertPoolOwnerRowStmt)
import DbSync.Db.Statement.PoolRelay (insertPoolRelayRowStmt)
import DbSync.Db.Statement.PoolRetire (insertPoolRetireRowStmt)
import DbSync.Db.Statement.PoolUpdate (insertPoolUpdateRowStmt)
import DbSync.Db.Statement.ReferenceTxIn (insertReferenceTxInRowStmt)
import DbSync.Db.Statement.SlotLeader (insertSlotLeaderRowStmt)
import DbSync.Db.Statement.StakeAddress (insertStakeAddressRowStmt)
import DbSync.Db.Statement.StakeDeregistration (insertStakeDeregistrationRowStmt)
import DbSync.Db.Statement.StakeRegistration (insertStakeRegistrationRowStmt)
import DbSync.Db.Statement.Tx (insertTxRowStmt)
import DbSync.Db.Statement.TxCbor (insertTxCborRowStmt)
import DbSync.Db.Statement.TxIn (insertTxInRowStmt)
import DbSync.Db.Statement.TxMetadata (insertTxMetadataRowStmt)
import DbSync.Db.Statement.TxOut (insertTxOutRowStmt)
import DbSync.Db.Statement.Withdrawal (insertWithdrawalRowStmt)
import DbSync.Writer (Writer (..))

mkInsertWriter :: Conn.Connection -> Writer IO
mkInsertWriter conn = Writer
  { writeBlock      = \bid blk -> run conn (bid, blk) insertBlockRowStmt
  , writeSlotLeader = \sid sl  -> run conn (sid, sl)  insertSlotLeaderRowStmt
  , writeTx         = \tid tx  -> run conn (tid, tx)  insertTxRowStmt
  , commit          = pure ()

    -- UTxO writers
  , writeAddress        = \aid addr -> run conn (aid, addr) insertAddressRowStmt
  , writeTxOut          = \oid txo -> run conn (oid, txo) insertTxOutRowStmt
  , writeTxIn           = \iid ti  -> run conn (iid, ti)  insertTxInRowStmt
  , writeCollateralTxIn = \iid ci  -> run conn (iid, ci)  insertCollateralTxInRowStmt
  , writeReferenceTxIn  = \iid ri  -> run conn (iid, ri)  insertReferenceTxInRowStmt

    -- Metadata writers
  , writeTxMetadata     = \mid md  -> run conn (mid, md)  insertTxMetadataRowStmt

    -- MultiAsset writers
  , writeMultiAsset     = \mid ma  -> run conn (mid, ma)  insertMultiAssetRowStmt
  , writeMaTxMint       = \mid m   -> run conn (mid, m)   insertMaTxMintRowStmt
  , writeMaTxOut        = \mid m   -> run conn (mid, m)   insertMaTxOutRowStmt

    -- StakeDelegation writers
  , writeStakeAddress        = \sid sa -> run conn (sid, sa) insertStakeAddressRowStmt
  , writeStakeRegistration   = \sid sr -> run conn (sid, sr) insertStakeRegistrationRowStmt
  , writeStakeDeregistration = \sid sd -> run conn (sid, sd) insertStakeDeregistrationRowStmt
  , writeDelegation          = \did d  -> run conn (did, d)  insertDelegationRowStmt
  , writeWithdrawal          = \wid w  -> run conn (wid, w)  insertWithdrawalRowStmt

    -- Pool writers
  , writePoolHash         = \pid ph  -> run conn (pid, ph)  insertPoolHashRowStmt
  , writePoolUpdate       = \pid pu  -> run conn (pid, pu)  insertPoolUpdateRowStmt
  , writePoolMetadataRef  = \pid pm  -> run conn (pid, pm)  insertPoolMetadataRefRowStmt
  , writePoolOwner        = \pid po  -> run conn (pid, po)  insertPoolOwnerRowStmt
  , writePoolRetire       = \pid pr  -> run conn (pid, pr)  insertPoolRetireRowStmt
  , writePoolRelay        = \pid pr  -> run conn (pid, pr)  insertPoolRelayRowStmt

    -- CBOR writers
  , writeTxCbor          = \tcid tc -> run conn (tcid, tc) insertTxCborRowStmt
  , writeEpochSyncStats      = \_ _ -> todo "writeEpochSyncStats"
  , writeAdaPots             = \_ _ -> todo "writeAdaPots"
  }

run :: Conn.Connection -> a -> Stmt.Statement a b -> IO b
run conn p stmt = do
  result <- Conn.use conn (Sess.statement p stmt)
  case result of
    Right b -> pure b
    Left e  -> panic $ "Insert writer session failed: " <> show e

todo :: Text -> IO a
todo name = pure $ panic $ "Writer.InsertAdapter." <> name <> " not yet implemented"
