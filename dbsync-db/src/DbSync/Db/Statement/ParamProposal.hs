-- | Hasql 'Statement' bindings for the @param_proposal@ table.
--
-- Counter-managed FK target: @gov_action_proposal.param_proposal@
-- references it for @ParameterChange@ proposals.
module DbSync.Db.Statement.ParamProposal
  ( insertParamProposalRowStmt
  , nextParamProposalIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Governance
  ( ParamProposal
  , paramProposalEncoder
  , paramProposalTableDef
  )
import DbSync.Db.Schema.Ids (ParamProposalId (..), idEncoder)
import DbSync.Db.Statement.Common (insertRowSql, nextIdStmt)

insertParamProposalRowStmt :: Stmt.Statement (ParamProposalId, ParamProposal) ()
insertParamProposalRowStmt =
  Stmt.preparable (insertRowSql paramProposalTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getParamProposalId)
           <> (snd >$< paramProposalEncoder)

nextParamProposalIdStmt :: Stmt.Statement () ParamProposalId
nextParamProposalIdStmt = nextIdStmt paramProposalTableDef ParamProposalId
