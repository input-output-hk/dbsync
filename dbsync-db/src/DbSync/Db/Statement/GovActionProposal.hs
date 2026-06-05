{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @gov_action_proposal@ table.
--
-- Counter-managed FK target: the resolver allocates the id via
-- 'nextGovActionProposalIdStmt' and threads it into the row. The
-- cross-block proposal cache is keyed on @(tx_hash, index)@;
-- 'queryGovActionProposalByTxHashStmt' is the SELECT-on-PG fallback
-- when the per-block cache misses.
module DbSync.Db.Statement.GovActionProposal
  ( insertGovActionProposalRowStmt
  , nextGovActionProposalIdStmt
  , queryGovActionProposalByTxHashStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Governance
  ( GovActionProposal
  , govActionProposalEncoder
  , govActionProposalTableDef
  )
import DbSync.Db.Schema.Ids (GovActionProposalId (..), idDecoder, idEncoder)
import DbSync.Db.Statement.Common (insertRowSql, nextIdStmt)

insertGovActionProposalRowStmt
  :: Stmt.Statement (GovActionProposalId, GovActionProposal) ()
insertGovActionProposalRowStmt =
  Stmt.preparable (insertRowSql govActionProposalTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getGovActionProposalId)
           <> (snd >$< govActionProposalEncoder)

nextGovActionProposalIdStmt :: Stmt.Statement () GovActionProposalId
nextGovActionProposalIdStmt =
  nextIdStmt govActionProposalTableDef GovActionProposalId

-- | Look up @gov_action_proposal.id@ by the proposing @tx.hash@ and
-- the proposal's @index@. Used by the Follow cross-block proposal
-- cache to resolve a vote's @GovActionId@ when the in-process cache
-- misses.
queryGovActionProposalByTxHashStmt
  :: Stmt.Statement (ByteString, Word64) (Maybe GovActionProposalId)
queryGovActionProposalByTxHashStmt =
  Stmt.preparable sql encoder (D.rowMaybe (idDecoder GovActionProposalId))
  where
    sql =
      "SELECT g.id \
      \FROM gov_action_proposal g \
      \JOIN tx t ON t.id = g.tx_id \
      \WHERE t.hash = $1 AND g.index = $2"
    encoder =
         (fst >$< E.param (E.nonNullable E.bytea))
      <> ((fromIntegral . snd :: (ByteString, Word64) -> Int64)
           >$< E.param (E.nonNullable E.int8))
