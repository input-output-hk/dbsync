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
  , SyncOption (..)
  , SyncOptions (..)
  , SyncConfig (..)
  , UtxoOption (..)
  )

-- | Validate a parsed config, returning accumulated errors or the valid config.
-- Checks extractor dependencies and ledger requirements.
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
      , checkPoolRequiresStakeDelegation cfg
      , checkOffChainPoolsRequiresPool cfg
      , checkOffChainVotesRequiresGovernance cfg
      ]

-- ---------------------------------------------------------------------------
-- * Validation rules
-- ---------------------------------------------------------------------------

-- | epoch_boundary produces rewards, epoch_stake, ada_pots — all from ledger state.
-- If ledger is disabled, epoch_boundary must also be disabled.
checkEpochBoundaryRequiresLedger :: SyncConfig -> [ConfigError]
checkEpochBoundaryRequiresLedger cfg
  | prEnabled (pcEpochBoundary extractors) && not (lcEnabled ledger) =
      [ ConfigValidationError
          "epoch_boundary extractor requires ledger.enabled = true. \
          \epoch_boundary produces rewards, epoch_stake, and ada_pots which \
          \are computed from the ledger state."
      ]
  | otherwise = []
  where
    extractors = scOptions cfg
    ledger = scLedger cfg

-- | pool_stats sources its rows from the worker's per-epoch pool
-- distribution. If ledger is disabled, pool_stats must also be.
checkPoolStatsRequiresLedger :: SyncConfig -> [ConfigError]
checkPoolStatsRequiresLedger cfg
  | prEnabled (pcPoolStats extractors) && not (lcEnabled ledger) =
      [ ConfigValidationError
          "pool_stats extractor requires ledger.enabled = true. \
          \pool_stat rows are derived from the per-epoch pool \
          \distribution carried on the worker's ApplyResult."
      ]
  | otherwise = []
  where
    extractors = scOptions cfg
    ledger = scLedger cfg

-- | stake_delegation_ledger sources reward / pot_reward / epoch_stake /
-- epoch_stake_progress rows from the worker's per-block stake slice
-- and per-boundary apEvents. Requires ledger.enabled = true.
checkStakeDelegationLedgerRequiresLedger :: SyncConfig -> [ConfigError]
checkStakeDelegationLedgerRequiresLedger cfg
  | prEnabled (pcStakeDelegationLedger extractors) && not (lcEnabled ledger) =
      [ ConfigValidationError
          "stake_delegation_ledger extractor requires ledger.enabled = true. \
          \reward / pot_reward / epoch_stake / epoch_stake_progress rows are \
          \derived from the worker's per-block stake slice and per-boundary \
          \ledger events."
      ]
  | otherwise = []
  where
    extractors = scOptions cfg
    ledger = scLedger cfg

-- | current_state (current_utxo, current_delegation, etc.) requires ledger state.
checkCurrentStateRequiresLedger :: SyncConfig -> [ConfigError]
checkCurrentStateRequiresLedger cfg
  | prEnabled (pcCurrentState extractors) && not (lcEnabled ledger) =
      [ ConfigValidationError
          "current_state extractor requires ledger.enabled = true. \
          \current_state computes live UTxO set and delegation state from \
          \the ledger."
      ]
  | otherwise = []
  where
    extractors = scOptions cfg
    ledger = scLedger cfg

-- | multi_asset (ma_tx_mint, ma_tx_out) references tx_out rows from the UTxO extractor.
-- If UTxO is disabled, multi_asset data has no parent rows to reference.
checkMultiAssetRequiresUtxo :: SyncConfig -> [ConfigError]
checkMultiAssetRequiresUtxo cfg
  | prEnabled (pcMultiAsset extractors) && not (uoEnabled (pcUtxo extractors)) =
      [ ConfigValidationError
          "multi_asset extractor requires utxo extractor to be enabled. \
          \multi_asset data (ma_tx_mint, ma_tx_out) references tx_out rows \
          \from the utxo extractor."
      ]
  | otherwise = []
  where
    extractors = scOptions cfg

-- | pool (pool_update, pool_owner, etc.) references stake_address rows from the
-- StakeDelegation extractor (for reward addresses and owner stake keys).
-- Both extractors also share the pool_hash dedup table.
checkPoolRequiresStakeDelegation :: SyncConfig -> [ConfigError]
checkPoolRequiresStakeDelegation cfg
  | prEnabled (pcPool extractors) && not (prEnabled (pcStakeDelegation extractors)) =
      [ ConfigValidationError
          "pool extractor requires stake_delegation extractor to be enabled. \
          \pool_update and pool_owner reference stake_address rows from the \
          \stake_delegation extractor, and both share the pool_hash dedup table."
      ]
  | otherwise = []
  where
    extractors = scOptions cfg

-- | off_chain_pools rows reference pool_hash and pool_metadata_ref
-- rows written by the pool extractor.
checkOffChainPoolsRequiresPool :: SyncConfig -> [ConfigError]
checkOffChainPoolsRequiresPool cfg
  | prEnabled (pcOffChainPools extractors) && not (prEnabled (pcPool extractors)) =
      [ ConfigValidationError
          "off_chain_pools extractor requires pool extractor to be enabled. \
          \off_chain_pool_data and off_chain_pool_fetch_error reference \
          \pool_hash and pool_metadata_ref rows written by the pool extractor."
      ]
  | otherwise = []
  where
    extractors = scOptions cfg

-- | off_chain_votes anchor metadata only has meaning when the
-- governance extractor is enabled — the voting_anchor rows it fetches
-- are produced by the governance pass.
checkOffChainVotesRequiresGovernance :: SyncConfig -> [ConfigError]
checkOffChainVotesRequiresGovernance cfg
  | prEnabled (pcOffChainVotes extractors) && not (prEnabled (pcGovernance extractors)) =
      [ ConfigValidationError
          "off_chain_votes extractor requires governance extractor to be enabled. \
          \off_chain_vote_data fetches metadata for voting_anchor rows written \
          \by the governance extractor."
      ]
  | otherwise = []
  where
    extractors = scOptions cfg
