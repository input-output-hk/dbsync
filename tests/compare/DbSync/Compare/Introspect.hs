module DbSync.Compare.Introspect
  ( DbFacts (..)
  , AddressShape (..)
  , gatherFacts
  , columnExists
  , tableNonEmpty
  , approxRowCount
  , computeCeiling
  , computeBlockCeiling
  , quoteIdent
  ) where

import Cardano.Prelude
import qualified Data.Text as T
import DbSync.Compare.Connect

-- ---------------------------------------------------------------------------
-- * Database facts
-- ---------------------------------------------------------------------------

-- | How @tx_out@ stores its address columns on one side. 'AddressNormalised'
-- carries the raw-bytes column name on @address@ (@address@ in the rewrite,
-- @raw@ in legacy).
data AddressShape
  = AddressInline
  | AddressNormalised !Text
  deriving stock (Eq, Show)

data DbFacts = DbFacts
  { dfRole :: !DbRole
  , dfDbName :: !Text
  , dfServerVersion :: !Text
  , dfDbSize :: !Text
  , dfMaxEpoch :: !(Maybe Int64)
  , dfMaxBlockNo :: !(Maybe Int64)
  , dfPresentTables :: ![Text]
  , dfAddressShape :: !AddressShape
  }
  deriving stock (Eq, Show)

gatherFacts :: DbConn -> IO DbFacts
gatherFacts conn =
  DbFacts (dbcRole conn) (dbcName conn)
    <$> queryScalarText conn "SELECT current_setting('server_version')"
    <*> queryScalarText conn "SELECT pg_size_pretty(pg_database_size(current_database()))"
    <*> queryMaybeInt conn "SELECT max(epoch_no)::bigint FROM block"
    <*> queryMaybeInt conn "SELECT max(block_no)::bigint FROM block"
    <*> queryTextList
      conn
      "SELECT table_name::text FROM information_schema.tables WHERE table_schema = 'public' AND table_type = 'BASE TABLE'"
    <*> detectAddressShape conn

columnExists :: DbConn -> Text -> Text -> IO Bool
columnExists conn table col =
  queryBool conn $
    T.concat
      [ "SELECT EXISTS (SELECT 1 FROM information_schema.columns "
      , "WHERE table_schema = 'public' AND table_name = '"
      , table
      , "' AND column_name = '"
      , col
      , "')"
      ]

detectAddressShape :: DbConn -> IO AddressShape
detectAddressShape conn = do
  inline <- columnExists conn "tx_out" "address"
  if inline
    then pure AddressInline
    else do
      hasAddr <- columnExists conn "address" "address"
      if hasAddr
        then pure (AddressNormalised "address")
        else do
          hasRaw <- columnExists conn "address" "raw"
          pure $ if hasRaw then AddressNormalised "raw" else AddressInline

-- ---------------------------------------------------------------------------
-- * Per-table probes
-- ---------------------------------------------------------------------------

tableNonEmpty :: DbConn -> Text -> IO Bool
tableNonEmpty conn table =
  queryBool conn ("SELECT EXISTS (SELECT 1 FROM " <> quoteIdent table <> ")")

approxRowCount :: DbConn -> Text -> IO Int64
approxRowCount conn table =
  fromMaybe 0
    <$> queryMaybeInt
      conn
      ( "SELECT reltuples::bigint FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace "
          <> "WHERE n.nspname = 'public' AND c.relname = "
          <> quoteLiteral table
      )

-- ---------------------------------------------------------------------------
-- * Comparison ceilings
-- ---------------------------------------------------------------------------

-- The epoch ceiling drives the spot-check era ranges; one epoch back from the
-- lower tip keeps both databases inside a fully-synced epoch.
computeCeiling :: Maybe Int64 -> DbFacts -> DbFacts -> Int64
computeCeiling override oldFacts newFacts =
  case override of
    Just e -> e
    Nothing -> max 0 (min oldMax newMax - 1)
  where
    oldMax = fromMaybe 0 (dfMaxEpoch oldFacts)
    newMax = fromMaybe 0 (dfMaxEpoch newFacts)

-- The row counts bound on block_no rather than epoch: the two databases sit at
-- different tips even within the same epoch, so the lower shared block_no is
-- the only cut that lets the counts line up.
computeBlockCeiling :: Maybe Int64 -> DbFacts -> DbFacts -> Int64
computeBlockCeiling override oldFacts newFacts =
  case override of
    Just b -> b
    Nothing -> min oldMax newMax
  where
    oldMax = fromMaybe 0 (dfMaxBlockNo oldFacts)
    newMax = fromMaybe 0 (dfMaxBlockNo newFacts)

-- ---------------------------------------------------------------------------
-- * SQL quoting
-- ---------------------------------------------------------------------------

quoteIdent :: Text -> Text
quoteIdent t = "\"" <> T.replace "\"" "\"\"" t <> "\""

quoteLiteral :: Text -> Text
quoteLiteral t = "'" <> T.replace "'" "''" t <> "'"
