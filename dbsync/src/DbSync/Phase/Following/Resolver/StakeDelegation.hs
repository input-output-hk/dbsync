-- | Follow 'IdResolver' fragments for the @stake_delegation@ extractor.
--
-- The three pot-rebalancing ID assigners are emitted by the
-- stake-delegation extractor; their Follow-side insert plumbing has
-- not landed yet, so they fall through to 'todoResolve' stubs.
module DbSync.Phase.Following.Resolver.StakeDelegation
  ( -- * Direct flavour
    resolveStakeAddressConn
  , assignStakeRegistrationIdConn
  , assignStakeDeregistrationIdConn
  , assignDelegationIdConn
  , assignWithdrawalIdConn

    -- * Buffered flavour
  , resolveStakeAddressBuf
  , assignStakeRegistrationIdBuf
  , assignStakeDeregistrationIdBuf
  , assignDelegationIdBuf
  , assignWithdrawalIdBuf

    -- * Stubs (both flavours)
  , assignPotTransferIdStub
  , assignTreasuryIdStub
  , assignReserveIdStub
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Ids
  ( DelegationId
  , PotTransferId
  , ReserveId
  , StakeAddressId
  , StakeDeregistrationId
  , StakeRegistrationId
  , TreasuryId
  , WithdrawalId
  )
import DbSync.Db.Schema.StakeDelegation (StakeAddress)
import DbSync.Db.Statement.Delegation (nextDelegationIdStmt)
import DbSync.Db.Statement.StakeAddress (nextStakeAddressIdStmt, queryStakeAddressIdStmt)
import DbSync.Db.Statement.StakeDeregistration (nextStakeDeregistrationIdStmt)
import DbSync.Db.Statement.StakeRegistration (nextStakeRegistrationIdStmt)
import DbSync.Db.Statement.Withdrawal (nextWithdrawalIdStmt)
import DbSync.Phase.Following.IdAllocator (PreAllocatedIds (..), popHead)
import DbSync.Phase.Following.Resolver.Internal
  ( BlockDedupCache (..)
  , resolveDedupSimple
  , runStmt
  , todoResolve
  )

-- ---------------------------------------------------------------------------
-- * Direct flavour
-- ---------------------------------------------------------------------------

resolveStakeAddressConn
  :: Conn.Connection -> ByteString -> StakeAddress -> IO (StakeAddressId, Bool)
resolveStakeAddressConn conn hash _sa = do
  mId <- runStmt conn hash queryStakeAddressIdStmt
  case mId of
    Just saId -> pure (saId, False)
    Nothing   -> do
      saId <- runStmt conn () nextStakeAddressIdStmt
      pure (saId, True)

assignStakeRegistrationIdConn :: Conn.Connection -> IO StakeRegistrationId
assignStakeRegistrationIdConn conn = runStmt conn () nextStakeRegistrationIdStmt

assignStakeDeregistrationIdConn :: Conn.Connection -> IO StakeDeregistrationId
assignStakeDeregistrationIdConn conn = runStmt conn () nextStakeDeregistrationIdStmt

assignDelegationIdConn :: Conn.Connection -> IO DelegationId
assignDelegationIdConn conn = runStmt conn () nextDelegationIdStmt

assignWithdrawalIdConn :: Conn.Connection -> IO WithdrawalId
assignWithdrawalIdConn conn = runStmt conn () nextWithdrawalIdStmt

-- ---------------------------------------------------------------------------
-- * Buffered flavour
-- ---------------------------------------------------------------------------

resolveStakeAddressBuf
  :: Conn.Connection -> BlockDedupCache -> ByteString -> StakeAddress -> IO (StakeAddressId, Bool)
resolveStakeAddressBuf conn cache hash _sa =
  resolveDedupSimple
    conn
    hash
    (bdcStakeAddress cache)
    queryStakeAddressIdStmt
    nextStakeAddressIdStmt

assignStakeRegistrationIdBuf :: PreAllocatedIds -> IO StakeRegistrationId
assignStakeRegistrationIdBuf preAlloc = popHead "assignStakeRegistrationId" (paiStakeRegistrationIds preAlloc)

assignStakeDeregistrationIdBuf :: PreAllocatedIds -> IO StakeDeregistrationId
assignStakeDeregistrationIdBuf preAlloc = popHead "assignStakeDeregistrationId" (paiStakeDeregistrationIds preAlloc)

assignDelegationIdBuf :: PreAllocatedIds -> IO DelegationId
assignDelegationIdBuf preAlloc = popHead "assignDelegationId" (paiDelegationIds preAlloc)

assignWithdrawalIdBuf :: PreAllocatedIds -> IO WithdrawalId
assignWithdrawalIdBuf preAlloc = popHead "assignWithdrawalId" (paiWithdrawalIds preAlloc)

-- ---------------------------------------------------------------------------
-- * Stubs (both flavours)
-- ---------------------------------------------------------------------------

assignPotTransferIdStub :: IO PotTransferId
assignPotTransferIdStub = todoResolve "assignPotTransferId"

assignTreasuryIdStub :: IO TreasuryId
assignTreasuryIdStub = todoResolve "assignTreasuryId"

assignReserveIdStub :: IO ReserveId
assignReserveIdStub = todoResolve "assignReserveId"
