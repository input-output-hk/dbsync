---
id: error-handling
title: Error handling
sidebar_position: 10
---

# Error handling

How errors are represented, thrown, propagated, and rendered in dbsync,
and the discipline that keeps a crash log readable. The conventions here
are normative: this page is the reference for them. If you are adding a
throw site, catching at a boundary, or wondering why a crash log looks
the way it does, start here.

## The model

dbsync has no `ExceptT` in its stack. Errors are exceptions: an
`AppError` is thrown with `throwIO` and propagates through `AppM` until
something catches it or a thread dies. Error information lives in **two
layers**, and they have different lifetimes:

| Layer | Carried by | Survives `try`/rethrow? | Answers |
| --- | --- | --- | --- |
| Identity | `SrcInfo` field inside `AppError` | Yes — it's a record field | *What* failed, and *where* it was thrown |
| Depth | Exception-context annotations on `SomeException` | Only if you use the right combinators | The backtrace, the block in flight, the causal chain |

The identity layer is the robust one and it is the primary output. The
depth layer is supplementary and *fragile* — a single careless rethrow
drops it (see [the one rule](#the-one-rule-never-rethrow-a-bare-someexception)).
Design so the identity layer alone is enough to act on; treat annotations
as a bonus that makes triage faster.

## Throwing an error

Use a per-kind thrower from `DbSync.Error`. Each takes a single `Text`
message and captures the call site into `SrcInfo` automatically — you do
not construct `SrcInfo` or pass a callstack:

```haskell
when (rowsAffected /= 1) $
  throwSyncState ("expected exactly one row, got " <> show rowsAffected)
```

Pick the kind that matches the failure. The kind becomes the label in the
log and is what a catch site pattern-matches on:

| Thrower | `AppError` kind | Use for |
| --- | --- | --- |
| `throwDb` | `AppDatabaseError` | PostgreSQL connection or query failure (usually raised for you by `useConn`) |
| `throwSyncState` | `AppSyncStateError` | sync-state row read/write failure |
| `throwLedger` | `AppLedgerError` | ledger-state application failure |
| `throwBlock` | `AppBlockError` | block parse failure |
| `throwSchema` | `AppSchemaError` | schema generation or migration failure |
| `throwNetwork` | `AppNetworkError` | ChainSync / n2c connection failure |
| `throwInternal` | `AppInternalError` | programmer error — an unreachable branch, or a call made in the wrong phase |

Prefer these over the generic `throwAppError`; reach for `throwAppError`
only if you are writing a helper that is itself parameterised over the
constructor.

### Database failures

Do not hand-roll hasql error handling. Run sessions through `useConn` or
`usePoolSession` (`DbSync.Db.Run`); they turn a hasql `Left` into an
`AppDatabaseError` whose message is `<label>: <driver error>` and whose
`SrcInfo` points at *your* call site, not at the runner:

```haskell
useConn "advanceSyncState" conn $
  Sess.statement params writeSyncStateSlotStmt
```

The `label` is the first thing an operator reads in the log, so make it
name the operation.

### Wrapping third-party exceptions

At a boundary with a library that throws its own exception type, wrap the
whole action with `rethrowAs` to convert any *synchronous* failure into a
chosen `AppError` kind, prefixing a context string:

```haskell
rethrowAs AppNetworkError "connecting to node" $
  connectToNode iomgr topLevelCfg networkMagic socketPath req
```

Asynchronous exceptions (cancellation, timeouts) pass through `rethrowAs`
untouched, keeping their context — you never want to convert a
`ThreadKilled` into an `AppNetworkError`. The original exception is nested
as the cause, so the crash log shows both the wrapper and what it wrapped.

## The one rule: never rethrow a bare `SomeException`

:::danger
`throwIO (e :: SomeException)` **clears the exception context**. Every
annotation — the backtrace, the block-in-flight, the cause chain —
is silently lost. This is the single most common way to turn a
diagnosable crash into an anonymous one.
:::

The failure mode comes straight from GHC's exception machinery: the
`Exception` instance for `SomeException` resets the context on
`toException`. So the moment you catch as `SomeException` and throw it
again, you have thrown the annotations away.

When you must catch and rethrow the *same* exception (a retry loop, a
cleanup-then-continue), use the context-preserving combinators instead of
`catch` + `throwIO`:

- `catchNoPropagate` hands the handler an `ExceptionWithContext e` — the
  exception *together with* its annotations — and, unlike `catch`, does
  not itself add a `WhileHandling` layer.
- `rethrowIO` throws an `ExceptionWithContext` back without collecting a
  fresh backtrace or dropping the carried context.

The rollback retry loop in `DbSync.Phase.Following.Run` is the worked
example — retry transient database errors, rethrow everything else with
its context intact:

```haskell
outcome <-
  withRunInIO $ \runInIO ->
    (Right <$> runInIO (Rollback.rollbackToPoint tableDefs point))
      `Exception.catchNoPropagate`
        \(ewc :: Exception.ExceptionWithContext AppError) -> pure (Left ewc)

case outcome of
  Right () -> pure ()
  Left ewc@(Exception.ExceptionWithContext _ appErr)
    | AppDatabaseError _ msg <- appErr
    , attempt < rollbackMaxAttempts -> retryAfterDelay msg
    | otherwise                     -> liftIO (Exception.rethrowIO ewc)
```

Match on the payload (`appErr`) to decide; rethrow the wrapper (`ewc`) to
preserve. If instead you are catching one exception and throwing a
*different* one, that is not a rethrow — a plain `throwIO newError` inside
a `catch` handler is correct, and GHC records the caught exception as the
`WhileHandling` cause automatically.

## Backtraces come from IPE, not `HasCallStack`

On GHC 9.14 every `throwIO` collects a backtrace at the throw site and
attaches it as an annotation. dbsync uses the **IPE** (info-table
provenance) mechanism for this, chosen once at boot in
`DbSync.App.Run`:

```haskell
setBacktraceMechanismState IPEBacktrace True
setBacktraceMechanismState HasCallStackBacktrace False
```

IPE gives a real multi-frame execution stack with **zero cost on the
happy path** — nothing is collected unless an exception is actually
thrown. It requires `-finfo-table-map` (set per local package in
`cabal.project`) and full optimisation to produce meaningful frames;
`optimization: 2` is already on. The only cost is binary size (about
+6 MB / ~2%). The `HasCallStack` annotation is turned *off* because the
throwers already capture the exact site into `SrcInfo`, so it would add
nothing but a duplicate single frame.

:::note
IPE frames are empty in an unoptimised (`-O0`) build. If you are testing
backtrace output locally, build the way the executable ships (`-O2`), or
the backtrace section will be blank.
:::

### Do not add `HasCallStack`

`captureCallSite` reads only the **top** frame of the callstack, and the
throwers freeze it with `withFrozenCallStack` so that frame is the
caller. Adding `HasCallStack` to your own functions therefore changes
*nothing* in the output — it only pushes deeper frames that are never
read, while paying a hidden-argument and allocation cost at every call.
GHC never flags these as redundant, so they accumulate silently.

The constraint is deliberately confined to two places that genuinely feed
`SrcInfo`: the throwers in `DbSync.Error` and the session runners in
`DbSync.Db.Run`. Everywhere else, the deep stack comes from IPE. If you
find yourself reaching for `HasCallStack`, you want an IPE backtrace
instead — and you already have one.

## Naming the block in flight

An `AppError` says where in the *code* it was thrown; it does not know
which block was being processed. That context is attached with
`annotateIO` and a `BlockAnnotation`, wrapping the per-block work in both
the ingest consumer (`DbSync.Phase.Ingest.Consumer`) and the follow path
(`DbSync.Phase.Following.Run`):

```haskell
Exception.annotateIO blockAnn $ do
  -- extract, resolve ids, flush to PostgreSQL …
```

Anything thrown inside that scope — from any depth — carries the
annotation, and the renderer prints it as
`while processing block <n> (slot <s>, hash <h>…)`. This is the pattern
for **any** domain context worth having in a crash log: define a small
type with an `ExceptionAnnotation` instance (see `BlockAnnotation` in
`DbSync.Error`), wrap the scope with `annotateIO`, and it renders
automatically with no change to the throw sites or the renderer.

## Where errors surface

There are exactly two places an error becomes operator-visible, and both
route through `DbSync.Error.Render`:

- **A background thread dies.** Worker threads (tx-out, ledger, off-chain
  fetcher, …) exit through `logThreadExit`. A normal shutdown
  (`AsyncCancelled`) logs at `Info` as `stopped (cancelled during
  shutdown)`; anything else logs at `Error` as `crashed: <rendered
  crash>`.
- **The main thread escapes `runApp`.** `handleFatalError` in `Main`
  catches it, renders it into the app log, and exits non-zero. An
  `ExitCode` passes through untouched so an intentional exit keeps its
  status.

`renderCrash` produces a one-line summary followed by indented depth. It
first unwraps async's `ExceptionInLinkedThread` so the real error
surfaces rather than the propagation wrapper, then lays out the block
context, the IPE backtrace, and any `WhileHandling` cause:

```text
[2026-06-24 10:15:03.842 UTC] [Error] TxOutWorker: crashed: database error at dbsync/src/DbSync/SyncState/Row.hs:199 (writeSyncState): connection reset by peer
    while processing block 11542891 (slot 134092310, hash a1b2c3d4e5f60718…)
    IPE backtrace:
      DbSync.SyncState.Row.writeSyncState (dbsync/src/DbSync/SyncState/Row.hs:(197,1)-(199,44))
      DbSync.Phase.Following.Run.processForward (dbsync/src/DbSync/Phase/Following/Run.hs:(277,1)-(350,30))
```

The one-line summary (`renderAppError`) is `<kind> at <file>:<line>
(<function>): <message>`. Render errors this way — never with the derived
`Show` on `AppError`, which dumps the whole `SrcInfo` record and is noise.

## Log, don't throw — and when to swallow

Not every failure is an exception. Two distinctions:

- **Structured logging is not error handling.** `LogMsg` and the
  `logInfoIO` / `logWarnIO` / `logErrorIO` helpers (`DbSync.Trace.Types`)
  are for progress and diagnostics. Do not encode a fatal condition as a
  logged line and carry on; throw so it propagates.
- **Cleanup handlers may swallow.** Resource teardown on a bracket's
  release path (closing a connection, an LSM session) legitimately
  catches `SomeException`, logs it at `Warning`/`Error`, and continues —
  a failure while cleaning up must not mask the error that triggered the
  cleanup. This is the one sanctioned place to catch-and-discard, and it
  only ever *logs*; it never rethrows a bare `SomeException`.

## Testing errors

Behaviour that the renderer and throwers guarantee is locked with a spec —
`DbSync.Error.RenderSpec` is the model. It throws each kind and asserts
the one-line format, throws inside an `annotateIO` scope and asserts the
block context appears, and asserts the `WhileHandling` cause renders. When
you add an `AppError` kind, a domain annotation, or a new surfacing point,
extend that spec rather than trusting the shape by eye.

For the separate discipline of forcing thunks before they cross a thread
or epoch boundary — including the bomb tests that lock those contracts —
see [Memory & Strictness](memory-and-strictness).
