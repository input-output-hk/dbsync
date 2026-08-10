-- | Configuration validation.
--
-- Validates a parsed 'SyncConfig' for internal consistency.
-- Collects ALL errors (not just the first) so the user can fix them in one pass.
module DbSync.App.Config.Validation
  ( validateConfig
  ) where

import Cardano.Prelude

import DbSync.App.Config.Types
  ( ConfigError (..)
  , LedgerConfig (..)
  , OptionFlag (..)
  , Extractors (..)
  , SyncConfig (..)
  , UtxoOption (..)
  )

-- | Validate a parsed config; return every error found, or the valid config.
validateConfig :: SyncConfig -> Either [ConfigError] SyncConfig
validateConfig cfg =
  case errors of
    [] -> Right cfg
    es -> Left es
  where
    errors = concat
      [ checkEpochBoundaryRequiresLedger cfg
      , checkPoolStatsRequiresLedger cfg
      , checkStakeDelegationLedgerRequiresLedger cfg
      , checkCurrentStateRequiresLedger cfg
      , checkMultiAssetRequiresUtxo cfg
      , checkOffChainPoolsRequiresPool cfg
      , checkOffChainVotesRequiresGovernance cfg
      ]

-- ---------------------------------------------------------------------------
-- * Validation rules
-- ---------------------------------------------------------------------------

checkEpochBoundaryRequiresLedger :: SyncConfig -> [ConfigError]
checkEpochBoundaryRequiresLedger cfg
  | prEnabled (exEpochBoundary extractors) && not (lcEnabled ledger) =
      [ ConfigValidationError
          "epoch_boundary extractor requires ledger.enabled = true. \
          \epoch_boundary produces rewards, epoch_stake, and ada_pots which \
          \are computed from the ledger state."
      ]
  | otherwise = []
  where
    extractors = scExtractors cfg
    ledger = scLedger cfg

checkPoolStatsRequiresLedger :: SyncConfig -> [ConfigError]
checkPoolStatsRequiresLedger cfg
  | prEnabled (exPoolStats extractors) && not (lcEnabled ledger) =
      [ ConfigValidationError
          "pool_stats extractor requires ledger.enabled = true. \
          \pool_stat rows are derived from the per-epoch pool \
          \distribution carried on the worker's ApplyResult."
      ]
  | otherwise = []
  where
    extractors = scExtractors cfg
    ledger = scLedger cfg

checkStakeDelegationLedgerRequiresLedger :: SyncConfig -> [ConfigError]
checkStakeDelegationLedgerRequiresLedger cfg
  | prEnabled (exStakeDelegationLedger extractors) && not (lcEnabled ledger) =
      [ ConfigValidationError
          "stake_delegation_ledger extractor requires ledger.enabled = true. \
          \reward / pot_reward / epoch_stake / epoch_stake_progress rows are \
          \derived from the worker's per-block stake slice and per-boundary \
          \ledger events."
      ]
  | otherwise = []
  where
    extractors = scExtractors cfg
    ledger = scLedger cfg

checkCurrentStateRequiresLedger :: SyncConfig -> [ConfigError]
checkCurrentStateRequiresLedger cfg
  | prEnabled (exCurrentState extractors) && not (lcEnabled ledger) =
      [ ConfigValidationError
          "current_state extractor requires ledger.enabled = true. \
          \current_state computes live UTxO set and delegation state from \
          \the ledger."
      ]
  | otherwise = []
  where
    extractors = scExtractors cfg
    ledger = scLedger cfg

checkMultiAssetRequiresUtxo :: SyncConfig -> [ConfigError]
checkMultiAssetRequiresUtxo cfg
  | prEnabled (exMultiAsset extractors) && not (uoEnabled (exUtxo extractors)) =
      [ ConfigValidationError
          "multi_asset extractor requires utxo extractor to be enabled. \
          \multi_asset data (ma_tx_mint, ma_tx_out) references tx_out rows \
          \from the utxo extractor."
      ]
  | otherwise = []
  where
    extractors = scExtractors cfg

checkOffChainPoolsRequiresPool :: SyncConfig -> [ConfigError]
checkOffChainPoolsRequiresPool cfg
  | prEnabled (exOffChainPools extractors) && not (prEnabled (exPool extractors)) =
      [ ConfigValidationError
          "off_chain_pools extractor requires pool extractor to be enabled. \
          \off_chain_pool_data and off_chain_pool_fetch_error reference \
          \pool_metadata_ref rows written by the pool extractor."
      ]
  | otherwise = []
  where
    extractors = scExtractors cfg

checkOffChainVotesRequiresGovernance :: SyncConfig -> [ConfigError]
checkOffChainVotesRequiresGovernance cfg
  | prEnabled (exOffChainVotes extractors) && not (prEnabled (exGovernance extractors)) =
      [ ConfigValidationError
          "off_chain_votes extractor requires governance extractor to be enabled. \
          \off_chain_vote_data fetches metadata for voting_anchor rows written \
          \by the governance extractor."
      ]
  | otherwise = []
  where
    extractors = scExtractors cfg
