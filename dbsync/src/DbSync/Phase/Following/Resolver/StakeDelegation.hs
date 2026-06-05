-- | Follow 'IdResolver' fragments for the @stake_delegation@ extractor.
module DbSync.Phase.Following.Resolver.StakeDelegation
  ( -- * Direct flavour
    resolveStakeAddressConn

    -- * Buffered flavour
  , resolveStakeAddressBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Ids (StakeAddressId)
import DbSync.Db.Schema.StakeDelegation (StakeAddress)
import DbSync.Db.Statement.StakeAddress (nextStakeAddressIdStmt, queryStakeAddressIdStmt)
import DbSync.Phase.Following.Resolver.Internal
  ( BlockDedupCache (..)
  , resolveDedupSimple
  , runStmt
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
