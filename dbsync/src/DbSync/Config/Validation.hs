-- | Configuration validation.
--
-- Validates a parsed 'SyncConfig' for internal consistency.
-- Collects ALL errors (not just the first) so the user can fix them in one pass.
-- The original project had no validation here — this is new.
module DbSync.Config.Validation
  ( validateConfig
  ) where

import Cardano.Prelude

import DbSync.Config.Types
  ( ConfigError (..)
  , LedgerConfig (..)
  , ProjectionConfig (..)
  , ProjectionConfigs (..)
  , SyncConfig (..)
  )

-- | Validate a parsed config, returning accumulated errors or the valid config.
-- Checks projection dependencies and ledger requirements.
validateConfig :: SyncConfig -> Either [ConfigError] SyncConfig
validateConfig cfg =
  case errors of
    [] -> Right cfg
    es -> Left es
  where
    errors = concat
      [ checkEpochBoundaryRequiresLedger cfg
      , checkCurrentStateRequiresLedger cfg
      , checkMultiAssetRequiresUtxo cfg
      ]

-- ---------------------------------------------------------------------------
-- * Validation rules
-- ---------------------------------------------------------------------------

-- | epoch_boundary produces rewards, epoch_stake, ada_pots — all from ledger state.
-- If ledger is disabled, epoch_boundary must also be disabled.
checkEpochBoundaryRequiresLedger :: SyncConfig -> [ConfigError]
checkEpochBoundaryRequiresLedger cfg
  | prEnabled (pcEpochBoundary projections) && not (lcEnabled ledger) =
      [ ConfigValidationError
          "epoch_boundary projection requires ledger.enabled = true. \
          \epoch_boundary produces rewards, epoch_stake, and ada_pots which \
          \are computed from the ledger state."
      ]
  | otherwise = []
  where
    projections = scProjections cfg
    ledger = scLedger cfg

-- | current_state (current_utxo, current_delegation, etc.) requires ledger state.
checkCurrentStateRequiresLedger :: SyncConfig -> [ConfigError]
checkCurrentStateRequiresLedger cfg
  | prEnabled (pcCurrentState projections) && not (lcEnabled ledger) =
      [ ConfigValidationError
          "current_state projection requires ledger.enabled = true. \
          \current_state computes live UTxO set and delegation state from \
          \the ledger."
      ]
  | otherwise = []
  where
    projections = scProjections cfg
    ledger = scLedger cfg

-- | multi_asset (ma_tx_mint, ma_tx_out) references tx_out rows from the UTxO projection.
-- If UTxO is disabled, multi_asset data has no parent rows to reference.
checkMultiAssetRequiresUtxo :: SyncConfig -> [ConfigError]
checkMultiAssetRequiresUtxo cfg
  | prEnabled (pcMultiAsset projections) && not (prEnabled (pcUtxo projections)) =
      [ ConfigValidationError
          "multi_asset projection requires utxo projection to be enabled. \
          \multi_asset data (ma_tx_mint, ma_tx_out) references tx_out rows \
          \from the utxo projection."
      ]
  | otherwise = []
  where
    projections = scProjections cfg
