-- | Hasql 'Statement' bindings for the @stake_delegation@ extractor
-- tables: @stake_address@, @stake_registration@,
-- @stake_deregistration@, @delegation@, @withdrawal@, @pot_transfer@,
-- @reserve@, @treasury@.
--
-- @stake_address@ is dedup-keyed on @hash_raw@ (the 29-byte serialised
-- reward address). @pot_transfer@, @reserve@ and @treasury@ are
-- defined in 'DbSync.Db.Schema.EpochBoundary' but populated by this
-- extractor's MIR-cert handling. Everything else is an IDENTITY leaf.
module DbSync.Db.Statement.StakeDelegation
  ( -- * stake_address
    insertStakeAddressRowStmt
  , nextStakeAddressIdStmt
  , queryStakeAddressIdStmt

    -- * stake_registration
  , insertStakeRegistrationRowStmt

    -- * stake_deregistration
  , insertStakeDeregistrationRowStmt

    -- * delegation
  , insertDelegationRowStmt

    -- * withdrawal
  , insertWithdrawalRowStmt

    -- * pot_transfer
  , insertPotTransferRowStmt

    -- * reserve
  , insertReserveRowStmt

    -- * treasury
  , insertTreasuryRowStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.EpochBoundary
  ( PotTransfer
  , Reserve
  , Treasury
  , potTransferEncoder
  , potTransferTableDef
  , reserveEncoder
  , reserveTableDef
  , treasuryEncoder
  , treasuryTableDef
  )
import DbSync.Db.Schema.Ids (StakeAddressId (..), idEncoder)
import DbSync.Db.Schema.StakeDelegation
  ( Delegation
  , StakeAddress
  , StakeDeregistration
  , StakeRegistration
  , Withdrawal
  , delegationEncoder
  , delegationTableDef
  , stakeAddressEncoder
  , stakeAddressTableDef
  , stakeDeregistrationEncoder
  , stakeDeregistrationTableDef
  , stakeRegistrationEncoder
  , stakeRegistrationTableDef
  , withdrawalEncoder
  , withdrawalTableDef
  )
import DbSync.Db.Statement.Common
  ( LookupColumn (..)
  , insertRowSql
  , nextIdStmt
  , queryIdByColumnStmt
  )

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
-- * stake_registration
-- ---------------------------------------------------------------------------

insertStakeRegistrationRowStmt :: Stmt.Statement StakeRegistration ()
insertStakeRegistrationRowStmt =
  Stmt.preparable
    (insertRowSql stakeRegistrationTableDef)
    stakeRegistrationEncoder
    D.noResult

-- ---------------------------------------------------------------------------
-- * stake_deregistration
-- ---------------------------------------------------------------------------

insertStakeDeregistrationRowStmt :: Stmt.Statement StakeDeregistration ()
insertStakeDeregistrationRowStmt =
  Stmt.preparable
    (insertRowSql stakeDeregistrationTableDef)
    stakeDeregistrationEncoder
    D.noResult

-- ---------------------------------------------------------------------------
-- * delegation
-- ---------------------------------------------------------------------------

insertDelegationRowStmt :: Stmt.Statement Delegation ()
insertDelegationRowStmt =
  Stmt.preparable (insertRowSql delegationTableDef) delegationEncoder D.noResult

-- ---------------------------------------------------------------------------
-- * withdrawal
-- ---------------------------------------------------------------------------

insertWithdrawalRowStmt :: Stmt.Statement Withdrawal ()
insertWithdrawalRowStmt =
  Stmt.preparable (insertRowSql withdrawalTableDef) withdrawalEncoder D.noResult

-- ---------------------------------------------------------------------------
-- * pot_transfer
-- ---------------------------------------------------------------------------

insertPotTransferRowStmt :: Stmt.Statement PotTransfer ()
insertPotTransferRowStmt =
  Stmt.preparable (insertRowSql potTransferTableDef) potTransferEncoder D.noResult

-- ---------------------------------------------------------------------------
-- * reserve
-- ---------------------------------------------------------------------------

insertReserveRowStmt :: Stmt.Statement Reserve ()
insertReserveRowStmt =
  Stmt.preparable (insertRowSql reserveTableDef) reserveEncoder D.noResult

-- ---------------------------------------------------------------------------
-- * treasury
-- ---------------------------------------------------------------------------

insertTreasuryRowStmt :: Stmt.Statement Treasury ()
insertTreasuryRowStmt =
  Stmt.preparable (insertRowSql treasuryTableDef) treasuryEncoder D.noResult
