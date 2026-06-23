module DbSync.Compare.Normalize
  ( bytea
  , numericText
  , floatText
  , timestampEpoch
  , jsonbCanonical
  , asText
  ) where

import Cardano.Prelude

-- ---------------------------------------------------------------------------
-- * Canonical SQL-side normalisation
-- ---------------------------------------------------------------------------

-- Both databases must render a given column to the same text so that a textual
-- diff reflects a real data difference, not a formatting one. These helpers
-- wrap a SQL expression in the canonical form for its type.

-- Bytea as lower-case hex without the leading @\\x@.
bytea :: Text -> Text
bytea expr = "encode(" <> expr <> ", 'hex')"

-- Numeric / word128 to a plain decimal string with no trailing-zero noise.
numericText :: Text -> Text
numericText expr = "trim_scale(" <> expr <> ")::text"

-- A protocol-parameter ratio stored as @double precision@ in the old schema and
-- as @text@ in the new one. Cast both through numeric and trim trailing zeros so
-- the float and text encodings collapse to the same canonical decimal. NULL is
-- preserved (the cast chain yields NULL, never the string \"NULL\").
floatText :: Text -> Text
floatText expr = "trim_scale((" <> expr <> ")::double precision::numeric)::text"

-- Timestamp to whole seconds since the epoch, dodging timezone and sub-second
-- rendering differences between the two schemas.
timestampEpoch :: Text -> Text
timestampEpoch expr = "extract(epoch from " <> expr <> ")::bigint::text"

-- jsonb re-parsed so key order and whitespace are canonical on both sides.
jsonbCanonical :: Text -> Text
jsonbCanonical expr = "(" <> expr <> ")::jsonb::text"

-- A column with no normalisation hazard, rendered as text.
asText :: Text -> Text
asText expr = expr <> "::text"
