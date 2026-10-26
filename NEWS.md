# shinyasync (development version)

# shinyasync 0.5.0

## Breaking changes

* `status_async()` returns `"initial"` instead of `"idle"` before the first
  task is sent to a worker, the value `ExtendedTask$status()` returns, so
  status code moves over unchanged. `"idle"` remains the status after
  `cancel_async()` or a worker-side `req()` halt with no value (@hypebright).

## New features

* A function the session cannot resolve warns on the first run, naming the
  function and the fix (attach the package in the session or use
  `pkg::fn()`), instead of failing on the worker after the task has run.
  `print()` lists such functions on a `missing` line (@hypebright).
* `req()`, `validate()`, `invalidateLater()`, `isolate()`, `status_async()`
  and `has_value_async()` are recognised when called through a module
  object, as `shiny$req()` or `shinyasync$status_async()`, which is how
  `box::use(shiny)` binds them in box and rhino apps. Before, such calls ran
  on the worker, and `shiny$isolate()` did not isolate its read.

## Bug fixes

* `bslib::bind_task_button()` returns the async object visibly, as it
  returns an `ExtendedTask` and as `bindEvent()` returns it.
* The message printed when the default pool starts points to the top level of
  `app.R`, where `pool_async()` belongs, not to the server function
  (@hypebright).

# shinyasync 0.4.0

## New features

* `bslib::bind_task_button()` works on `reactiveAsync()`, `observeAsync()`
  and `reactiveValAsync()`: an `input_task_button()` is busy while the task
  runs and ready when it settles or is cancelled (#26).

## Bug fixes

* A resident name used only inside a shipped helper is no longer sent with
  every run; it is checked for on the worker and listed on the `resident`
  line of `print()` (#27).
* A `reactiveValAsync()` created without an assignment, as in `lapply()`,
  gets a label that is unique in the process. An assigned name is kept as
  the label also when the call is piped (#21, #24, @LeonidasZhak).

## Documentation

* `vignette("testing")` covers every primitive and its `wait_for()` matches
  the package's test helper. `vignette("extendedtask")` uses the example
  app's names (#28).

# shinyasync 0.3.0

## New features

* `pool_async()` gains `everywhere`, an expression run once on every worker
  when the pool starts, for data, models and connections every task needs.
  The names it creates are resident: tasks do not ship them and `print()`
  lists them (#15). A connection opened there is closed by a `.Last`
  function assigned in the same expression (#25).
* New `vignette("extendedtask")` writes the example app with `ExtendedTask`
  and with shinyasync side by side.
* The default pool size respects a container's CPU quota or cpuset (#17).

## Bug fixes

* A task settles as soon as its result arrives instead of being polled every
  100 ms. In `shiny::testServer()` a task that finishes within the flush that
  dispatched it now settles in that flush, so a test that expects to see it
  `"running"` should give it some work (#16).
* A `reactiveAsync()` waiting for an upstream async reactive reports
  `"running"` while the upstream runs, and in `mode = "latest"` a run on the
  old upstream value is dropped (#14).
* A `bindEvent()` whose event fires while the upstream async reactive is busy
  or unstarted now runs once the upstream settles; `cancel_async()` drops the
  pending event (#11).
* A name assigned inside an async expression, as in `data <- data()`,
  shadows the session object only after the assignment, so the reactive is
  read rather than shipped or mistaken for a function (#12).
* A task on a named mirai compute profile no longer starts the default pool
  (#20).
* `pool_async()` in `server()` and the first task in that session hold the
  pool once (#18).
* `reactiveValAsync()` is labelled with the name it is assigned to, as
  `shiny::reactiveVal()` is (#21).
* `mirai (>= 2.5.0)` is required; with an older mirai a worker-side `req()`
  became an error (#13).

# shinyasync 0.2.0

## New features

* `pool_async()` is optional: the first task that runs while no daemons are
  set starts a pool with the defaults.
* New `mirai_async()` starts a `mirai` with the same dependency capture as
  `reactiveAsync()`, for feeding a `reactiveValAsync()` (#4).
* New `has_value_async()` tells a first run, which wants a spinner, from a
  refresh, which has a previous value to show (#3).
* `print()` on an async reactive shows what dependency capture resolved
  without running it. In `shiny::devmode()` each task prints on its first
  run; `options(shinyasync.print = FALSE)` turns that off (#5).
* New `vignette("testing")` on testing async reactives with
  `shiny::testServer()` (#7).

## Bug fixes

* A chained async reactive keeps its previous value until its upstream has
  caught up with its inputs, then re-runs, as a chain of `reactive()`
  expressions would (#2).
* A reactive in a module's returned list, read as `mod$wind()`, is hoisted
  like `wind()` instead of shipped.
* `status_async()` and `has_value_async()` inside an async expression are
  evaluated on the session thread.
* `status_async()` starts a `reactiveAsync()` that has not run yet, as
  reading its value does (#1).

# shinyasync 0.1.0

First release.

* `reactiveAsync()`, `observeAsync()` and `reactiveValAsync()`: async
  counterparts of `reactive()`, `observe()` and `reactiveVal()` that evaluate
  on `mirai` daemons, track reactive dependencies, cancel superseded work and
  return the previous value while a new one is computed.
* `status_async()` and `cancel_async()` for spinners and cancel buttons.
* `pool_async()` starts a daemon pool shared by every session in the process.
* `bindEvent()` methods so button-triggered async reactives update outputs.
