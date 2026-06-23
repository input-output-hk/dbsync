module DbSync.Compare.Schema
  ( StaticClass (..)
  , staticClassify
  , renamedOldName
  , newTableNames
  , tableToExtractor
  , oldOnlyTables
  ) where

import Cardano.Prelude
import Data.List (lookup)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Extractor.Registry (allDeclaredTables, allKnownExtractors)

-- ---------------------------------------------------------------------------
-- * New-schema tables
-- ---------------------------------------------------------------------------

newTableNames :: [Text]
newTableNames = map tdName allDeclaredTables

tableToExtractor :: [(Text, Text)]
tableToExtractor =
  [(tdName t, pdName e) | e <- allKnownExtractors, t <- pdTables e]

-- ---------------------------------------------------------------------------
-- * Old vs new divergences
-- ---------------------------------------------------------------------------

-- Map a new table name to its old-schema name. Only renamed tables differ.
renamedOldName :: Text -> Text
renamedOldName = \case
  "pot_reward" -> "reward_rest"
  "epoch_finalized" -> "epoch"
  other -> other

data StaticClass
  = StaticSkip Text -- ^ Definite skip with reason; never probed.
  | StaticCandidate (Maybe Text) -- ^ Worth probing; optional comparability note.

staticClassify :: Text -> StaticClass
staticClassify name
  | Just reason <- lookup name newOnlySkips = StaticSkip reason
  | Just reason <- lookup name unpopulatedInNew = StaticSkip reason
  | name `elem` addressNormalised = StaticCandidate (Just "address-normalised: compared via address join")
  | otherwise = StaticCandidate Nothing

-- New-schema tables that are bookkeeping or normalisation helpers.
newOnlySkips :: [(Text, Text)]
newOnlySkips =
  [ ("address", "new-only: address-normalisation helper (verified through tx_out)")
  , ("dbsync_sync_state", "new-only: sync bookkeeping")
  , ("epoch_param_pending", "new-only: internal pending buffer")
  ]

-- Declared in the new schema but never populated by any extractor yet.
unpopulatedInNew :: [(Text, Text)]
unpopulatedInNew =
  [ ("event_info", "declared but never populated in either")
  , ("delisted_pool", "SMASH not wired in new")
  , ("reserved_pool_ticker", "SMASH not wired in new")
  ]

-- Tables whose row comparison needs the address-normalisation mapping.
addressNormalised :: [Text]
addressNormalised = ["tx_out", "collateral_tx_out"]

-- Tables present only in the old schema, with no new counterpart.
oldOnlyTables :: [(Text, Text)]
oldOnlyTables =
  [ ("meta", "old-only bookkeeping")
  , ("reverse_index", "old-only")
  , ("epoch_sync_time", "old-only")
  , ("extra_migrations", "old-only migration bookkeeping")
  , ("schema_version", "old-only schema bookkeeping")
  , ("new_committee", "old-only; represented differently in new")
  ]
