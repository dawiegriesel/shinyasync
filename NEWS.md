# shinyasync (development version)

* `vignette("testing")` now covers every primitive: sections on
  `observeAsync()`, `reactiveValAsync()` with `mirai_async()`,
  `mode = "queue"` and a chained `reactiveAsync()`, plus the `peek()` helper
  for reading a reactive that may halt. Its `wait_for()` matches the
  package's own test helper again, flushing once more after the condition
  holds so observers see the value, and the article notes that a task which
  finishes within the flush that dispatched it is never seen `"running"`.
* `vignette("extendedtask")` describes queued invocations as sequential on
  one worker, mentions `globals = list(...)` for reads capture cannot
  resolve, and says the fan-out table gains a row as each task returns. The
  pkgdown site lists the articles in that order.

# shinyasync 0.3.0

* The default pool size respects the CPU limits of the process's cgroup. It
  was one less than `parallel::detectCores()`, which reports the host's
  cores, so in a container or on Posit Connect with a CPU quota or cpuset
  the default pool started a daemon per host core. It is now one less than
  the host's cores capped by the tightest cgroups v1 or v2 quota or cpuset
  on the process or any ancestor cgroup (#17).
* New vignette, `vignette("extendedtask")`, writes the surf forecast app with
  `ExtendedTask` and with shinyasync side by side, one piece at a time: the
  pool, a fetch from an input, status and the previous value, a button with
  cancel, superseded inputs, a chained task, side effects and fan-out, and
  what the layer costs against `ExtendedTask`.
* A running task no longer polls `mirai::unresolved()` every 100 ms. The poll
  covered a promise notification that mirai can lose: its `as.promise()`
  method checks `unresolved()` and then registers the notification, and a
  task that completes in between never notifies. One check after
  registration closes that window, so a task settles as soon as its result
  arrives, with no timer. In `shiny::testServer()`, whose flush ends by
  running due `later` callbacks, a task that finishes within the flush that
  dispatched it now settles in that flush, where the poll left 100 ms of
  grace; a test that reads the status or value of a quick task straight
  after `setInputs()` should give the task some work. A test reproduces the
  lost notification by widening the window. The race was fixed upstream in
  nanonext 1.10.3.9000 (r-lib/mirai#656, r-lib/nanonext#363); the check is
  skipped on that version and later, and goes once it is required (#16).
* A connection opened in `pool_async(everywhere = )` can be closed by a
  `.Last` function assigned in the same expression. R runs it when the
  worker exits, on pool shutdown and when the app process dies, and the pool
  does not wait for it. This is base R, so no teardown argument was added;
  it is documented and tested (#25).
* `pool_async()` gains `everywhere`, an expression evaluated once on every
  worker when the pool starts, for data frames, models and connections that
  every task needs. Objects it refers to are sent once and packages it uses
  are attached; the names it creates are resident, so tasks no longer ship
  them, and `print()` lists them on a `resident` line. The README documents
  that shipped objects are otherwise serialized on every run (#15).
* `reactiveValAsync()` is labelled with the name it is assigned to, as
  `shiny::reactiveVal()` is, so values can be told apart in reactlog (#21).
* A task on a named mirai compute profile (`mirai_async(.compute = "x")`) no
  longer starts the default pool (#20).
* `pool_async()` documents what holds the pool outside an app: a script or
  console call releases it when the next app in the process stops, or at
  process exit (#19).
* `pool_async()` called in `server()` and the first task in that session now
  hold the pool once rather than twice; the counts balanced at session end
  before, so this is an accounting change only (#18).
* A `reactiveAsync()` waiting for an upstream async reactive now reports
  `"running"` while that upstream runs, so a status-driven spinner shows
  during the whole refresh, and in `mode = "latest"` a run already computing
  on the old upstream value is dropped instead of occupying a worker for a
  result that would be discarded (#14).
* A downstream gated with `bindEvent()` whose event fires while its upstream
  async reactive is busy, or before it has started, now runs once the upstream
  settles; `cancel_async()` drops the pending event, and so does an upstream
  that stops without producing. Previously the event was always lost: the
  halt inside `bindEvent()`'s `isolate()` registered no dependency, so
  nothing re-ran the task (#11).
* A name assigned inside an async expression now shadows the session object
  only after its assignment, so `data <- data()` reads the reactive and ships
  nothing. Previously the whole expression treated `data` as local, the
  worker called `utils::data()` and the result was silently wrong, or failed
  with "could not find function" for other names (#12).
* `mirai (>= 2.5.0)` is now required. The code has used `mirai::daemons_set()`
  (mirai 2.3.0) and the `condition.class` attribute on worker errors (mirai
  2.5.0) since 0.2.0; with an older mirai a worker-side `req()` became a real
  error instead of a silent halt (#13).

# shinyasync 0.2.0

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
