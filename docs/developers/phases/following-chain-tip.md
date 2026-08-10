---
id: following-chain-tip
title: FollowingChainTip
sidebar_position: 5
---

# FollowingChainTip

Same code path as [`FollowingVolatileTail`](following-volatile-tail). The
phase tag records that the consumer has caught up with the receiver and now
idles between blocks. Nothing differs at the PG level.

The tag still matters for:

- The log component prefix and the `epoch_sync_stats.phase` column.
- The Follow loop's **idle heartbeat**. Every 30 seconds without a block,
  the loop emits a `"still at tip"` line so a quiet chain does not look
  like a stalled app. It fires only in `FollowingChainTip`; in
  `FollowingVolatileTail` the windowed summary covers visibility.
- The per-block progress log, which is more verbose here because there is
  no throughput summary to fall back on between blocks.

## The flip is one-way

After each applied block, `maybeFlipToTip` reads the receiver's latest tip
and compares **block numbers** — not slots:

```haskell
applied + tipFollowMargin >= tip   -- tipFollowMargin = 1
```

If the predicate holds, `setCurrentPhase` sets `FollowingChainTip`. The flip
is cheap: one `readTVarIO` and a comparison.

:::warning There is no automatic flip back
`FollowingVolatileTail → FollowingChainTip` happens once and does not
reverse on its own. Falling behind the receiver does **not** move the phase
back.

The only path back is `MsgRollback`. `processRollback` sets
`FollowingVolatileTail`, and it is not a label change: it runs the full
multi-table DELETE cascade in one transaction. See
[Rollback](following-volatile-tail#rollback).
:::

The predicate is also suppressed inside the replay window, where the
skip-only consumer outpaces the receiver and would fire it spuriously.
