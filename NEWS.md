# shinyasync (development version)

* New `has_value_async()` is `TRUE` when reading an async reactive would
  return a value rather than halt or throw. With `status_async()` it tells a
  first run, which wants a spinner, from a refresh, which wants a "showing
  the previous result" note (#3).
* New `stale_async()` and `inputs_async()` expose the provenance of a
  `reactiveAsync()` result: the reactive reads it was computed from. A chained
  async reactive can guard an upstream read with `req(!stale_async(up))` so
  it never combines a new input with an upstream value computed from the old
  one, and re-runs once the upstream catches up. Staleness follows a chain of
  async reactives and upstream errors propagate through the guard (#2).
* `status_async()`, `stale_async()` and `inputs_async()` calls inside an async
  expression are evaluated on the session thread, like reactive reads, when
  their argument is a name or a literal list field such as `mod$r`.
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
