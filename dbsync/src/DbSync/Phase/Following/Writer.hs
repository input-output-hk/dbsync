{-# LANGUAGE OverloadedStrings #-}

-- | FollowingChainTip writer: typed records to hasql INSERTs against
-- a single connection. IDs come from the resolver; this layer only
-- writes.
--
-- Two implementations:
--
-- * 'mkWriter' — every @write*@ goes straight to PG via
--   @Conn.use Sess.statement@. One round-trip per row; used by the
--   integration test suite where deterministic per-row behaviour
--   makes assertions easier to author.
--
-- * 'mkBufferedWriter' — every @write*@ appends a
--   'Pipeline.statement' to a per-block 'WriteBuffer'. The
--   orchestrator drains the buffer in one round-trip at end of
--   block. Used in production; the per-row cost drops from one
--   round-trip to one append.
--
-- The within-block dedup pattern (a SELECT seeing a just-inserted
-- row) is preserved because the buffered resolver maintains an
-- in-process map for the duration of the block.
--
-- 'commit' is a no-op in both implementations; the per-block
-- @BEGIN@\/@COMMIT@ envelope owned by @Phase.Following.Run@ is what
-- actually closes the transaction.
module DbSync.Phase.Following.Writer
  ( mkWriter
  , mkBufferedWriter
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Pipeline as Pipeline
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Phase.Following.WriteBuffer (WriteBuffer, append)

import DbSync.Db.Statement.Address (insertAddressRowStmt)
import DbSync.Db.Statement.Block (insertBlockRowStmt)
import DbSync.Db.Statement.CollateralTxIn (insertCollateralTxInRowStmt)
import DbSync.Db.Statement.CollateralTxOut (insertCollateralTxOutRowStmt)
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

mkWriter :: Conn.Connection -> Writer IO
mkWriter conn = Writer
  { writeBlock      = \bid blk -> run conn (bid, blk) insertBlockRowStmt
  , writeSlotLeader = \sid sl  -> run conn (sid, sl)  insertSlotLeaderRowStmt
  , writeTx         = \tid tx  -> run conn (tid, tx)  insertTxRowStmt

    -- UTxO writers
  , writeAddress         = \aid addr -> run conn (aid, addr) insertAddressRowStmt
  , writeTxOut           = \oid txo  -> run conn (oid, txo)  insertTxOutRowStmt
  , writeTxIn            = \iid ti   -> run conn (iid, ti)   insertTxInRowStmt
  , writeCollateralTxIn  = \iid ci   -> run conn (iid, ci)   insertCollateralTxInRowStmt
  , writeCollateralTxOut = \oid co   -> run conn (oid, co)   insertCollateralTxOutRowStmt
  , writeReferenceTxIn   = \iid ri   -> run conn (iid, ri)   insertReferenceTxInRowStmt

    -- Metadata writers
  , writeTxMetadata = \mid md -> run conn (mid, md) insertTxMetadataRowStmt

    -- MultiAsset writers
  , writeMultiAsset = \mid ma -> run conn (mid, ma) insertMultiAssetRowStmt
  , writeMaTxMint   = \mid m  -> run conn (mid, m)  insertMaTxMintRowStmt
  , writeMaTxOut    = \mid m  -> run conn (mid, m)  insertMaTxOutRowStmt

    -- StakeDelegation writers
  , writeStakeAddress        = \sid sa -> run conn (sid, sa) insertStakeAddressRowStmt
  , writeStakeRegistration   = \sid sr -> run conn (sid, sr) insertStakeRegistrationRowStmt
  , writeStakeDeregistration = \sid sd -> run conn (sid, sd) insertStakeDeregistrationRowStmt
  , writeDelegation          = \did d  -> run conn (did, d)  insertDelegationRowStmt
  , writeWithdrawal          = \wid w  -> run conn (wid, w)  insertWithdrawalRowStmt

    -- Pool writers
  , writePoolHash         = \pid ph -> run conn (pid, ph) insertPoolHashRowStmt
  , writePoolUpdate       = \pid pu -> run conn (pid, pu) insertPoolUpdateRowStmt
  , writePoolMetadataRef  = \pid pm -> run conn (pid, pm) insertPoolMetadataRefRowStmt
  , writePoolOwner        = \pid po -> run conn (pid, po) insertPoolOwnerRowStmt
  , writePoolRetire       = \pid pr -> run conn (pid, pr) insertPoolRetireRowStmt
  , writePoolRelay        = \pid pr -> run conn (pid, pr) insertPoolRelayRowStmt

    -- CBOR writers
  , writeTxCbor = \tcid tc -> run conn (tcid, tc) insertTxCborRowStmt

  , writeEpochSyncStats = \_ _ -> todo "writeEpochSyncStats"
  , writeAdaPots        = \_ _ -> todo "writeAdaPots"

    -- No-op: the per-block transaction envelope is owned by
    -- @Phase.Following.Run@, not the Writer.
  , commit = pure ()
  }

run :: Conn.Connection -> a -> Stmt.Statement a b -> IO b
run conn p stmt = do
  result <- Conn.use conn (Sess.statement p stmt)
  case result of
    Right b -> pure b
    Left  e -> panic $ "Insert writer session failed: " <> show e

-- | Buffered writer: every @write*@ appends a 'Pipeline.statement'
-- to the supplied 'WriteBuffer' instead of running the statement
-- immediately. The caller flushes the buffer once at end of block.
--
-- Same row shapes, same encoders, same SQL — only the network
-- timing differs. Each append is a 'modifyIORef' (microseconds);
-- each immediate-mode call would be a libpq round-trip
-- (microseconds-to-milliseconds, depending on PG distance).
mkBufferedWriter :: WriteBuffer -> Writer IO
mkBufferedWriter buf = Writer
  { writeBlock      = \bid blk -> queue (bid, blk) insertBlockRowStmt
  , writeSlotLeader = \sid sl  -> queue (sid, sl)  insertSlotLeaderRowStmt
  , writeTx         = \tid tx  -> queue (tid, tx)  insertTxRowStmt

  , writeAddress         = \aid addr -> queue (aid, addr) insertAddressRowStmt
  , writeTxOut           = \oid txo  -> queue (oid, txo)  insertTxOutRowStmt
  , writeTxIn            = \iid ti   -> queue (iid, ti)   insertTxInRowStmt
  , writeCollateralTxIn  = \iid ci   -> queue (iid, ci)   insertCollateralTxInRowStmt
  , writeCollateralTxOut = \oid co   -> queue (oid, co)   insertCollateralTxOutRowStmt
  , writeReferenceTxIn   = \iid ri   -> queue (iid, ri)   insertReferenceTxInRowStmt

  , writeTxMetadata = \mid md -> queue (mid, md) insertTxMetadataRowStmt

  , writeMultiAsset = \mid ma -> queue (mid, ma) insertMultiAssetRowStmt
  , writeMaTxMint   = \mid m  -> queue (mid, m)  insertMaTxMintRowStmt
  , writeMaTxOut    = \mid m  -> queue (mid, m)  insertMaTxOutRowStmt

  , writeStakeAddress        = \sid sa -> queue (sid, sa) insertStakeAddressRowStmt
  , writeStakeRegistration   = \sid sr -> queue (sid, sr) insertStakeRegistrationRowStmt
  , writeStakeDeregistration = \sid sd -> queue (sid, sd) insertStakeDeregistrationRowStmt
  , writeDelegation          = \did d  -> queue (did, d)  insertDelegationRowStmt
  , writeWithdrawal          = \wid w  -> queue (wid, w)  insertWithdrawalRowStmt

  , writePoolHash         = \pid ph -> queue (pid, ph) insertPoolHashRowStmt
  , writePoolUpdate       = \pid pu -> queue (pid, pu) insertPoolUpdateRowStmt
  , writePoolMetadataRef  = \pid pm -> queue (pid, pm) insertPoolMetadataRefRowStmt
  , writePoolOwner        = \pid po -> queue (pid, po) insertPoolOwnerRowStmt
  , writePoolRetire       = \pid pr -> queue (pid, pr) insertPoolRetireRowStmt
  , writePoolRelay        = \pid pr -> queue (pid, pr) insertPoolRelayRowStmt

  , writeTxCbor = \tcid tc -> queue (tcid, tc) insertTxCborRowStmt

  , writeEpochSyncStats = \_ _ -> todo "writeEpochSyncStats"
  , writeAdaPots        = \_ _ -> todo "writeAdaPots"

  , commit = pure ()
  }
  where
    queue :: a -> Stmt.Statement a () -> IO ()
    queue params stmt = append buf (Pipeline.statement params stmt)

todo :: Text -> IO a
todo name = pure $ panic $ "Phase.Following.Writer." <> name <> " not yet implemented"
