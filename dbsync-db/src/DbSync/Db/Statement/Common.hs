{-# LANGUAGE OverloadedStrings #-}

-- | Shared 'Hasql' helpers used by every per-table @DbSync.Db.Statement.*@
-- module. Every helper is parameterised by a 'TableDef' so the table
-- name lives in one place (the schema module) and never has to be
-- hand-typed in a statement module.
module DbSync.Db.Statement.Common
  ( -- * ID allocation
    nextIdStmt

    -- * Lookups
  , LookupColumn (..)
  , queryIdByColumnStmt
  , countRowsStmt

    -- * Reusable codecs
  , int8RowDecoder
  , word64Param

    -- * Array parameter helpers
  , arrayParam
  , nullArrayParam
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (idDecoder)
import DbSync.Db.Schema.Types (TableDef (..))

-- ---------------------------------------------------------------------------
-- * ID allocation
-- ---------------------------------------------------------------------------

-- | @SELECT nextval(\'\<table\>_id_seq\')@ returning a typed id.
--
-- Replaces the per-module @nextXxxIdStmt@ pattern that hand-wrote
-- the same 4-line body 25+ times.
nextIdStmt :: TableDef -> (Int64 -> a) -> Stmt.Statement () a
nextIdStmt td ctor =
  Stmt.preparable
    ("SELECT nextval('" <> tdName td <> "_id_seq')")
    E.noParams
    (D.singleRow (idDecoder ctor))

-- ---------------------------------------------------------------------------
-- * Lookups
-- ---------------------------------------------------------------------------

-- | The column used by 'queryIdByColumnStmt' to look up a row's
-- primary key from its natural-key bytes.
--
-- Closed set of the three column-naming conventions our hash-keyed
-- lookup tables use today. Add a constructor when a new convention
-- lands; that single point of edit makes the convention visible at
-- compile time and rules out a stringly-typed column-name typo.
data LookupColumn
  = ByHash      -- ^ The @hash@ column (block, tx, slot_leader).
  | ByHashRaw   -- ^ The @hash_raw@ column (pool_hash, stake_address).
  deriving stock (Eq, Show)

lookupColumnName :: LookupColumn -> Text
lookupColumnName = \case
  ByHash    -> "hash"
  ByHashRaw -> "hash_raw"

-- | @SELECT id FROM \<table\> WHERE \<column\> = $1@ for a 'ByteString'
-- key. Used by every \"look up the id of a previously-inserted row\"
-- statement that keys on a single bytea column.
queryIdByColumnStmt
  :: TableDef
  -> LookupColumn
  -> (Int64 -> a)        -- ^ id constructor
  -> Stmt.Statement ByteString (Maybe a)
queryIdByColumnStmt td col ctor =
  Stmt.preparable
    ("SELECT id FROM " <> tdName td <> " WHERE " <> lookupColumnName col <> " = $1")
    (E.param (E.nonNullable E.bytea))
    (D.rowMaybe (idDecoder ctor))

-- | @SELECT COUNT(*) FROM \<table\>@ as an 'Int64'.
countRowsStmt :: TableDef -> Stmt.Statement () Int64
countRowsStmt td =
  Stmt.preparable
    ("SELECT COUNT(*) FROM " <> tdName td)
    E.noParams
    int8RowDecoder

-- ---------------------------------------------------------------------------
-- * Reusable codecs
-- ---------------------------------------------------------------------------

-- | A single-column 'Int64' row decoder. Shared by @COUNT(*)@,
-- @MAX(id)@, and similar aggregate-shape statements.
int8RowDecoder :: D.Result Int64
int8RowDecoder = D.singleRow (D.column (D.nonNullable D.int8))

-- | Encode a 'Word64' through a PostgreSQL @int8@ column. Cardano
-- slot numbers and similar are widened from 'Word64' to 'Int64' at
-- the boundary — this codifies that decision in one place.
word64Param :: E.Params Word64
word64Param = fromIntegral >$< E.param (E.nonNullable E.int8)

-- ---------------------------------------------------------------------------
-- * Array parameter helpers
-- ---------------------------------------------------------------------------

-- | A @\<type\>[]@ array of non-null values. Wraps the
-- @nonNullable . foldableArray . nonNullable@ triple that callers
-- otherwise spell out by hand.
arrayParam :: E.Value a -> E.Params [a]
arrayParam v = E.param (E.nonNullable (E.foldableArray (E.nonNullable v)))

-- | A @\<type\>[]@ array of nullable values.
nullArrayParam :: E.Value a -> E.Params [Maybe a]
nullArrayParam v = E.param (E.nonNullable (E.foldableArray (E.nullable v)))
