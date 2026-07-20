---
id: memory-and-strictness
title: Memory & Strictness
---

# Memory & Strictness

How laziness interacts with the long-lived structures in this codebase,
what a laziness leak looks like from the outside, and the discipline that
keeps the class of bug out. The conventions here are normative: this page
is the reference for them, and explains *why* they exist and how to
recognise a violation in a running sync.

## Why this class of bug is expensive

A thunk retains everything its closure can reach, and the retainer is
anonymous: heap profiles show the *retained* data (often unattributed
`ARR_WORDS` byte arrays) while no cost centre names the code that created
the thunk. Worse, profiling builds perturb the optimizer enough that the
leak can shrink or vanish under instrumentation — the one build you can
inspect is the one build that doesn't have the problem. Finding one of
these after the fact costs weeks; preventing them costs a bang.

## A worked failure

Consider decoding a fixed-layout value fetched from the LSM store:

```haskell
decodeOutput (UtxoOutputBytes sbs)
  | SBS.length sbs /= 24 = Nothing
  | otherwise            = Just
      ( TxId       (readInt64BE  sbs 0)
      , TxOutId    (readInt64BE  sbs 8)     -- the read that never happens
      , DbLovelace (readWord64BE sbs 16)
      )
```

This looks strict — it "constructs" three ids. It constructs nothing:

1. **Newtype constructors are erased at runtime.** `TxOutId (readInt64BE
   sbs 8)` is heap-identical to the bare thunk `readInt64BE sbs 8`.
   There is no constructor cell, so wrapping evaluates nothing, and the
   thunk's closure captures `sbs` — the whole backing byte array.
2. **Containers store elements as given.** The id flows into a per-epoch
   buffer via `Seq`'s `(|>)`, which is spine-strict but element-lazy: the
   finger tree is forced, the element is not. A `!` on the record field
   holding the `Seq` forces the tree root — one layer — and no more.
3. **Lifetime does the damage.** The buffer is not drained per block; it
   is handed to a worker at the epoch boundary. One unforced thunk per
   spent input, at one to two million inputs per epoch, is a multi-GB
   live-heap ramp that collapses the instant the boundary force runs.

The fix closes both ends — force at construction, and force at the entry
point of the long-lived structure so the invariant holds regardless of
caller:

```haskell
  | otherwise =
      let !tid = readInt64BE  sbs 0
          !oid = readInt64BE  sbs 8
          !val = readWord64BE sbs 16
      in Just (TxId tid, TxOutId oid, DbLovelace val)

recordConsumedBy ref !producerOutId !consumerTxId = ...
```

## Recognising a laziness leak in a running sync

The built-in instrumentation makes the signature visible without a
profiler. With `logging.level: debug`:

- **`Gauge` lines** sample every queue depth, per-epoch buffer fill and
  RTS live/in-use bytes on an interval. The leak signature is live bytes
  ramping linearly with a buffer counter across the epoch, then
  collapsing at the boundary — a sawtooth whose period is exactly one
  epoch.
- **`MajorGcProbe`** forces a major GC mid-epoch and logs live
  before/after. `liveAfter` far below `liveBefore` means the growth was
  collectible garbage; `liveAfter` tracking `liveBefore` means a
  reachable structure — a real retention, this class of bug.
- **`TxOutProbe`** brackets the boundary buffer-force and each bulk
  statement with allocation and live-heap readings.
- The second boot log line (`App: binary … (linked …)`) identifies the
  running executable by link time. Trust no measurement until it matches
  the build you think you are testing.

When the sawtooth appears, audit the entry points of epoch-lived
structures (buffers, queues, caches) for unforced values *before*
reaching for heap profiling. Two measurement caveats if you do profile:
`live` reported after a minor GC counts promoted-but-dead data as live
(only a major GC gives a truthful figure), and a `-prof` build may not
reproduce the leak at all.

## The discipline

| Layer | Rule |
| --- | --- |
| Construction | Decode strictly at every deserialisation boundary (LSM values, CBOR, wire formats). The decoder is the last place that knows how large the backing buffer is. |
| Wrapping | A newtype constructor forces nothing. `!` fields, `Seq` snoc, `atomicModifyIORef'` each force exactly one WHNF layer. `!(Maybe a)` stops at the `Just` cell. Strictness composes shallowly. |
| Storage | Anything entering a structure that outlives the call — per-epoch buffers, inter-thread queues, caches — is forced at the entry point, with bangs on the entry point's own arguments. Block-lifetime values may stay lazy; epoch-lifetime values may not. |
| Payloads | Records that cross thread boundaries are forced to normal form before enqueue. Hand-write `rnf` over any field that can hide a thunk; do not trust a derived instance. |
| Exceptions | Deliberate laziness (e.g. datum payloads that a dedup hit drops unread) is documented at the field with who forces it or why nothing ever does. |

## Lock the contract with a bomb test

Every force-on-entry point and every `NFData` instance relied on by a
force-at-enqueue gets a test that feeds it a bottom and expects a throw:

```haskell
it "recordConsumedBy forces both ids on entry (bomb)" $ do
  ref <- newConsumedByBufferRef
  recordConsumedBy ref (panic "unforced producer id") (TxId 10)
    `shouldThrow` anyException
```

`Data.Map.Lazy` builds the same `Map` type as the strict module without
forcing values, which makes it useful in tests as a stand-in for any
lazily-produced entry. See `DbSync.Worker.TxOut.WorkerSpec` and
`DbSync.Worker.Ledger.TypesSpec` for the existing suite. The test, not
the deriving or the bang you remember adding, is the guarantee.
