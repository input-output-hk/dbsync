-- | Hasql 'Statement' bindings for the @pool@ extractor tables:
-- @pool_update@, @pool_metadata_ref@, @pool_owner@, @pool_retire@,
-- @pool_relay@.
--
-- @pool_update@ and @pool_metadata_ref@ are counter-managed (each row
-- allocates a fresh id). @pool_owner@, @pool_retire@ and @pool_relay@
-- are IDENTITY leaves.
module DbSync.Db.Statement.Pool
  ( -- * pool_update
    insertPoolUpdateRowStmt
  , nextPoolUpdateIdStmt

    -- * pool_metadata_ref
  , insertPoolMetadataRefRowStmt
  , nextPoolMetadataRefIdStmt

    -- * pool_owner
  , insertPoolOwnerRowStmt

    -- * pool_retire
  , insertPoolRetireRowStmt

    -- * pool_relay
  , insertPoolRelayRowStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids
  ( PoolMetadataRefId (..)
  , PoolUpdateId (..)
  , idEncoder
  )
import DbSync.Db.Schema.Pool
  ( PoolMetadataRef
  , PoolOwner
  , PoolRelay
  , PoolRetire
  , PoolUpdate
  , poolMetadataRefEncoder
  , poolMetadataRefTableDef
  , poolOwnerEncoder
  , poolOwnerTableDef
  , poolRelayEncoder
  , poolRelayTableDef
  , poolRetireEncoder
  , poolRetireTableDef
  , poolUpdateEncoder
  , poolUpdateTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql, nextIdStmt)

-- ---------------------------------------------------------------------------
-- * pool_update
-- ---------------------------------------------------------------------------

insertPoolUpdateRowStmt :: Stmt.Statement (PoolUpdateId, PoolUpdate) ()
insertPoolUpdateRowStmt =
  Stmt.preparable (insertRowSql poolUpdateTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getPoolUpdateId)
           <> (snd >$< poolUpdateEncoder)

nextPoolUpdateIdStmt :: Stmt.Statement () PoolUpdateId
nextPoolUpdateIdStmt = nextIdStmt poolUpdateTableDef PoolUpdateId

-- ---------------------------------------------------------------------------
-- * pool_metadata_ref
-- ---------------------------------------------------------------------------

insertPoolMetadataRefRowStmt :: Stmt.Statement (PoolMetadataRefId, PoolMetadataRef) ()
insertPoolMetadataRefRowStmt =
  Stmt.preparable (insertRowSql poolMetadataRefTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getPoolMetadataRefId)
           <> (snd >$< poolMetadataRefEncoder)

nextPoolMetadataRefIdStmt :: Stmt.Statement () PoolMetadataRefId
nextPoolMetadataRefIdStmt = nextIdStmt poolMetadataRefTableDef PoolMetadataRefId

-- ---------------------------------------------------------------------------
-- * pool_owner
-- ---------------------------------------------------------------------------

insertPoolOwnerRowStmt :: Stmt.Statement PoolOwner ()
insertPoolOwnerRowStmt =
  Stmt.preparable (insertRowSql poolOwnerTableDef) poolOwnerEncoder D.noResult

-- ---------------------------------------------------------------------------
-- * pool_retire
-- ---------------------------------------------------------------------------

insertPoolRetireRowStmt :: Stmt.Statement PoolRetire ()
insertPoolRetireRowStmt =
  Stmt.preparable (insertRowSql poolRetireTableDef) poolRetireEncoder D.noResult

-- ---------------------------------------------------------------------------
-- * pool_relay
-- ---------------------------------------------------------------------------

insertPoolRelayRowStmt :: Stmt.Statement PoolRelay ()
insertPoolRelayRowStmt =
  Stmt.preparable (insertRowSql poolRelayTableDef) poolRelayEncoder D.noResult
