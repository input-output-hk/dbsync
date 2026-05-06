{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @pool_relay@ table.
module DbSync.Db.Statement.PoolRelay
  ( -- * Inserts
    insertPoolRelayRowStmt

    -- * ID allocation
  , nextPoolRelayIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (PoolRelayId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.Pool (PoolRelay, poolRelayEncoder, poolRelayTableDef)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName poolRelayTableDef

insertPoolRelayRowStmt :: Stmt.Statement (PoolRelayId, PoolRelay) ()
insertPoolRelayRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getPoolRelayId)
           <> (snd >$< poolRelayEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, update_id, ipv4, ipv6, dns_name, dns_srv_name, port)"
      , " VALUES ($1, $2, $3, $4, $5, $6, $7)"
      ]

nextPoolRelayIdStmt :: Stmt.Statement () PoolRelayId
nextPoolRelayIdStmt =
  Stmt.preparable sql E.noParams (D.singleRow $ idDecoder PoolRelayId)
  where
    sql = "SELECT nextval('" <> table <> "_id_seq')"
