---
id: following-chain-tip
title: FollowingChainTip
sidebar_position: 5
---

# FollowingChainTip

Same code path as [`FollowingVolatileTail`](following-volatile-tail) — the
phase tag just distinguishes that the consumer has caught up with the
receiver and is now idling between blocks. There's no behavioural
difference at the PG level.

The tag still matters for:

- The log component prefix and the `epoch_sync_stats.phase` column.
- The Follow loop's **idle heartbeat** — every quiet period the loop
  emits a `"still at tip"` line so a quiet chain doesn't look like a
  stalled app. Only fires in `FollowingChainTip` (in
  `FollowingVolatileTail` the windowed summary covers visibility).
- The per-block progress log — more verbose in `FollowingChainTip`
  because there's no throughput-style summary to fall back on between
  blocks.

## The flip

The Follow loop checks every block whether the consumer is at the
receiver's tip. The flip is automatic and cheap: read the latest tip
ref, compare against the current slot, set the phase via
`setCurrentPhase` if changed.

The opposite flip — back to `FollowingVolatileTail` — happens the same
way if the consumer falls behind the receiver (e.g. a brief node-side
hiccup that backs the queue up).

Neither transition involves any PG work or resource handoff. They're
log distinctions only.
