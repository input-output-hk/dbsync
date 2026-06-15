{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @core@ extractor tables:
-- @block@, @tx@, @slot_leader@, @stake_address@, @pool_hash@.
--
-- @stake_address@ is dedup-keyed on @hash_raw@ (the 29-byte serialised
-- reward address); @pool_hash@ is dedup-keyed on @hash_raw@ (the
-- 28-byte pool key hash).
--
-- Used during 'FollowingChainTip' and 'PreparingForVolatileTail';
-- 'IngestChainHistory' writes via COPY instead. The @Row@-suffixed
-- inserts take a caller-chosen id allocated via the matching
-- @next…IdStmt@; the unsuffixed inserts let PostgreSQL pick the id.
module DbSync.Db.Statement.Core
  ( -- * block
    insertBlockStmt
  , insertBlockRowStmt
  , nextBlockIdStmt
  , queryBlockIdByHashStmt
  , queryBlockCountStmt
  , queryLatestBlockNoStmt
  , queryLatestEpochNoStmt
  , queryLatestSlotNoStmt
  , queryLatestBlockIdStmt

    -- * tx
  , insertTxStmt
  , insertTxRowStmt
  , nextTxIdStmt
  , queryTxIdByHashStmt
  , queryTxCountStmt

    -- * slot_leader
  , insertSlotLeaderStmt
  , insertSlotLeaderRowStmt
  , nextSlotLeaderIdStmt
  , querySlotLeaderIdStmt
  , querySlotLeaderCountStmt

    -- * stake_address
  , insertStakeAddressRowStmt
  , nextStakeAddressIdStmt
  , queryStakeAddressIdStmt

    -- * pool_hash
  , insertPoolHashRowStmt
  , nextPoolHashIdStmt
  , queryPoolHashIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Core
  ( Block
  , BlockCols (..)
  , PoolHash
  , SlotLeader
  , StakeAddress
  , Tx
  , blockCols
  , blockEncoder
  , blockTableDef
  , poolHashEncoder
  , poolHashTableDef
  , slotLeaderEncoder
  , slotLeaderTableDef
  , stakeAddressEncoder
  , stakeAddressTableDef
  , txEncoder
  , txTableDef
  )
import DbSync.Db.Schema.Ids
  ( BlockId (..)
  , PoolHashId (..)
  , SlotLeaderId (..)
  , StakeAddressId (..)
  , TxId (..)
  , idDecoder
  , idEncoder
  )
import DbSync.Db.Schema.Types (TableColumn (..), TableDef (..))
import DbSync.Db.Statement.Common
  ( LookupColumn (..)
  , countRowsStmt
  , insertReturningIdSql
  , insertRowSql
  , nextIdStmt
  , queryIdByColumnStmt
  )

-- ---------------------------------------------------------------------------
-- * block
-- ---------------------------------------------------------------------------

insertBlockStmt :: Stmt.Statement Block BlockId
insertBlockStmt =
  Stmt.preparable
    (insertReturningIdSql blockTableDef)
    blockEncoder
    (D.singleRow $ idDecoder BlockId)

insertBlockRowStmt :: Stmt.Statement (BlockId, Block) ()
insertBlockRowStmt =
  Stmt.preparable (insertRowSql blockTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getBlockId)
           <> (snd >$< blockEncoder)

nextBlockIdStmt :: Stmt.Statement () BlockId
nextBlockIdStmt = nextIdStmt blockTableDef BlockId

queryBlockIdByHashStmt :: Stmt.Statement ByteString (Maybe BlockId)
queryBlockIdByHashStmt = queryIdByColumnStmt blockTableDef ByHash BlockId

queryBlockCountStmt :: Stmt.Statement () Int64
queryBlockCountStmt = countRowsStmt blockTableDef

-- | 'Nothing' when the table is empty or every row has a NULL
-- @block_no@ (Byron EBBs).
queryLatestBlockNoStmt :: Stmt.Statement () (Maybe Word64)
queryLatestBlockNoStmt =
  Stmt.preparable sql E.noParams
    (D.singleRow $ D.column (D.nullable $ fromIntegral <$> D.int8))
  where
    col = blockCols.bcBlockNo.tcName
    sql = "SELECT MAX(" <> col <> ") FROM " <> tdName blockTableDef
       <> " WHERE " <> col <> " IS NOT NULL"

-- | 'Nothing' on a Byron-only table or when the table is empty.
queryLatestEpochNoStmt :: Stmt.Statement () (Maybe Word64)
queryLatestEpochNoStmt =
  Stmt.preparable sql E.noParams
    (D.singleRow $ D.column (D.nullable $ fromIntegral <$> D.int8))
  where
    col = blockCols.bcEpochNo.tcName
    sql = "SELECT MAX(" <> col <> ") FROM " <> tdName blockTableDef
       <> " WHERE " <> col <> " IS NOT NULL"

-- | Returns @0@ when no slot-bearing block exists. Used at boot to
-- find the ChainSync intersection point.
queryLatestSlotNoStmt :: Stmt.Statement () Word64
queryLatestSlotNoStmt =
  Stmt.preparable sql E.noParams
    (D.singleRow $ fromIntegral <$> D.column (D.nonNullable D.int8))
  where
    col = blockCols.bcSlotNo.tcName
    sql = "SELECT COALESCE(MAX(" <> col <> "), 0)::bigint FROM "
       <> tdName blockTableDef <> " WHERE " <> col <> " IS NOT NULL"

-- | The id of the block with the largest @slot_no@ (not necessarily
-- the largest @id@). 'Nothing' on an empty table.
queryLatestBlockIdStmt :: Stmt.Statement () (Maybe BlockId)
queryLatestBlockIdStmt =
  Stmt.preparable sql E.noParams (D.rowMaybe (idDecoder BlockId))
  where
    idCol   = blockCols.bcId.tcName
    slotCol = blockCols.bcSlotNo.tcName
    sql = "SELECT " <> idCol <> " FROM " <> tdName blockTableDef
       <> " WHERE " <> slotCol <> " IS NOT NULL"
       <> " ORDER BY " <> slotCol <> " DESC LIMIT 1"

-- ---------------------------------------------------------------------------
-- * tx
-- ---------------------------------------------------------------------------

insertTxStmt :: Stmt.Statement Tx TxId
insertTxStmt =
  Stmt.preparable
    (insertReturningIdSql txTableDef)
    txEncoder
    (D.singleRow $ idDecoder TxId)

insertTxRowStmt :: Stmt.Statement (TxId, Tx) ()
insertTxRowStmt =
  Stmt.preparable (insertRowSql txTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getTxId)
           <> (snd >$< txEncoder)

nextTxIdStmt :: Stmt.Statement () TxId
nextTxIdStmt = nextIdStmt txTableDef TxId

queryTxIdByHashStmt :: Stmt.Statement ByteString (Maybe TxId)
queryTxIdByHashStmt = queryIdByColumnStmt txTableDef ByHash TxId

queryTxCountStmt :: Stmt.Statement () Int64
queryTxCountStmt = countRowsStmt txTableDef

-- ---------------------------------------------------------------------------
-- * slot_leader
-- ---------------------------------------------------------------------------

insertSlotLeaderStmt :: Stmt.Statement SlotLeader SlotLeaderId
insertSlotLeaderStmt =
  Stmt.preparable
    (insertReturningIdSql slotLeaderTableDef)
    slotLeaderEncoder
    (D.singleRow $ idDecoder SlotLeaderId)

insertSlotLeaderRowStmt :: Stmt.Statement (SlotLeaderId, SlotLeader) ()
insertSlotLeaderRowStmt =
  Stmt.preparable (insertRowSql slotLeaderTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getSlotLeaderId)
           <> (snd >$< slotLeaderEncoder)

nextSlotLeaderIdStmt :: Stmt.Statement () SlotLeaderId
nextSlotLeaderIdStmt = nextIdStmt slotLeaderTableDef SlotLeaderId

querySlotLeaderIdStmt :: Stmt.Statement ByteString (Maybe SlotLeaderId)
querySlotLeaderIdStmt = queryIdByColumnStmt slotLeaderTableDef ByHash SlotLeaderId

querySlotLeaderCountStmt :: Stmt.Statement () Int64
querySlotLeaderCountStmt = countRowsStmt slotLeaderTableDef

-- ---------------------------------------------------------------------------
-- * stake_address
-- ---------------------------------------------------------------------------

insertStakeAddressRowStmt :: Stmt.Statement (StakeAddressId, StakeAddress) ()
insertStakeAddressRowStmt =
  Stmt.preparable (insertRowSql stakeAddressTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getStakeAddressId)
           <> (snd >$< stakeAddressEncoder)

nextStakeAddressIdStmt :: Stmt.Statement () StakeAddressId
nextStakeAddressIdStmt = nextIdStmt stakeAddressTableDef StakeAddressId

queryStakeAddressIdStmt :: Stmt.Statement ByteString (Maybe StakeAddressId)
queryStakeAddressIdStmt =
  queryIdByColumnStmt stakeAddressTableDef ByHashRaw StakeAddressId

-- ---------------------------------------------------------------------------
-- * pool_hash
-- ---------------------------------------------------------------------------

insertPoolHashRowStmt :: Stmt.Statement (PoolHashId, PoolHash) ()
insertPoolHashRowStmt =
  Stmt.preparable (insertRowSql poolHashTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getPoolHashId)
           <> (snd >$< poolHashEncoder)

nextPoolHashIdStmt :: Stmt.Statement () PoolHashId
nextPoolHashIdStmt = nextIdStmt poolHashTableDef PoolHashId

queryPoolHashIdStmt :: Stmt.Statement ByteString (Maybe PoolHashId)
queryPoolHashIdStmt = queryIdByColumnStmt poolHashTableDef ByHashRaw PoolHashId
