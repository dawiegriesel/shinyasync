# shinyasync 0.2.0

* `reactiveValAsync()` now gives each value a distinct default label, so
  multiple values can be distinguished in reactlog. Explicit labels are
  unchanged (#21).
* New vignette, `vignette("testing")`, on testing async reactives with
  `shiny::testServer()`: a few lines pump the event loop and the mock session
  until a task settles, using only the mock session's public methods (#7).
* `print()` on a `reactiveAsync()` or `observeAsync()` shows what dependency
  capture resolved: the calls evaluated on the session, the objects and
  packages shipped to the worker, and the rewritten worker expression, or the
  error that stops the expression being sent. Nothing is evaluated and the
  task is not started, so it is safe anywhere in a server function. In
  `shiny::devmode()` each async reactive prints itself on its first run, and
  each `mirai_async()` call site the first time it runs in a session;
  `options(shinyasync.print = FALSE)` turns that off (#5).
* `pool_async()` is now optional. The first task that runs while no daemons
  are set starts a pool with the defaults, with a message, held by the
  session it runs in. Previously each task warned and ran in a fresh process.
* New `mirai_async()` starts a `mirai` with the same dependency capture as
  `reactiveAsync()`: helper functions, the functions they call, objects and
  packages are shipped automatically, and reactive reads, `req()` and
  `invalidateLater()` are evaluated on the session thread. It returns the
  plain `mirai`, so a `reactiveValAsync()` can be fed without hand-shipping
  every helper (#4).
* New `has_value_async()` is `TRUE` when reading an async reactive would
  return a value rather than halt or throw. With `status_async()` it tells a
  first run, which wants a spinner, from a refresh, which wants a "showing
  the previous result" note (#3).
* A chained async reactive now waits for its upstream. Each `reactiveAsync()`
  run records the reactive reads it was computed from; a downstream that
  reads it keeps its previous value while the upstream is catching up with
  its own inputs, so it never combines a new input with an upstream value
  computed from the old one, and re-runs once the upstream has settled.
  Staleness follows a chain and upstream errors propagate down it, as in a
  chain of `reactive()` expressions. No guard is needed in the expression (#2).
* A reactive in a module's returned list, read as `mod$wind()` or
  `mod[["wind"]]()`, is now hoisted like `wind()`. Previously the list was
  shipped to the worker and the read failed there.
* `status_async()` and `has_value_async()` calls inside an async expression
  are evaluated on the session thread, like reactive reads, when their
  argument is a name or a literal list field such as `mod$r`.
* `status_async()` now starts a `reactiveAsync()` that has not run yet, the
  same way reading its value does. Previously a UI that read the value only
  once the status was `"success"` never started the task (#1).

# shinyasync 0.1.0

First release.

* `reactiveAsync()`, `observeAsync()` and `reactiveValAsync()`: async
  counterparts of `reactive()`, `observe()` and `reactiveVal()` that evaluate
  on `mirai` daemons, track reactive dependencies, cancel superseded work and
  return the previous value while a new one is computed.
* `status_async()` and `cancel_async()` for spinners, disabled buttons and
  cancel buttons.
* `pool_async()` starts a daemon pool shared by every session in the process,
  reference-counted and shut down with the last session.
* `bindEvent()` methods so button-triggered async reactives update outputs.
* Dependency capture respects `isolate()`, keeps `req()`/`validate()` in the
  worker when they may not run, hoists reads inside formulas, ships helper
  closures with their own environments, and reports assignment to reactive
  values as an error.
