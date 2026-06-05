-- | Hasql 'Statement' bindings for the @redeemer@ table.
--
-- @redeemer@ is counter-managed (other tables would FK into it, and
-- there is no natural dedup key); the resolver allocates a fresh id
-- per row via 'nextRedeemerIdStmt'.
module DbSync.Db.Statement.Redeemer
  ( -- * Inserts
    insertRedeemerRowStmt

    -- * ID allocation
  , nextRedeemerIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (RedeemerId (..), idEncoder)
import DbSync.Db.Schema.ScriptsDatums
  ( Redeemer
  , redeemerEncoder
  , redeemerTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql, nextIdStmt)

insertRedeemerRowStmt :: Stmt.Statement (RedeemerId, Redeemer) ()
insertRedeemerRowStmt =
  Stmt.preparable (insertRowSql redeemerTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getRedeemerId)
           <> (snd >$< redeemerEncoder)

nextRedeemerIdStmt :: Stmt.Statement () RedeemerId
nextRedeemerIdStmt = nextIdStmt redeemerTableDef RedeemerId
