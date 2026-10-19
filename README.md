# shinyasync

<!-- badges: start -->
[![R-CMD-check](https://github.com/dawiegriesel/shinyasync/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dawiegriesel/shinyasync/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![GitHub release](https://img.shields.io/github/v/release/dawiegriesel/shinyasync?label=version)](https://github.com/dawiegriesel/shinyasync/releases)
[![pkgdown](https://github.com/dawiegriesel/shinyasync/actions/workflows/pkgdown.yaml/badge.svg)](https://dawiegriesel.github.io/shinyasync/)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE.md)
<!-- badges: end -->

> Async Shiny reactives on `mirai`, with the standard Shiny API.

> **Warning:** use with caution. This package is not on CRAN yet and its API
> can still change, including renamed or removed functions, until it is
> published there. Pin a commit if you depend on it.

`shinyasync` lets you move slow work off the Shiny session thread without changing how you write Shiny. Add `Async` to the reactive primitives you already use and the rest of your app stays the same.

| Shiny            | shinyasync            |
|------------------|-----------------------|
| `reactive()`     | `reactiveAsync()`     |
| `observe()`      | `observeAsync()`      |
| `reactiveVal()`  | `reactiveValAsync()`  |

Under the hood each one is a cancellable task, modelled on `ExtendedTask`, backed by a `mirai` daemon pool. You never see either.

## Why

Shiny's `ExtendedTask` makes an app responsive within a session and across concurrent users, but using it today means wiring three things by hand: an `ExtendedTask$new()` with a `mirai()` inside, argument capture via `environment()`, and a reference to the running mirai if you ever want to cancel. The result is boilerplate that looks nothing like ordinary Shiny code.

`shinyasync` collapses that into one call and keeps the reactive contract:

- dependencies are tracked like a normal reactive;
- when a dependency changes the in-flight task is cancelled and re-run;
- while a task runs, reading the reactive returns the previous value (stale-while-revalidate) or silently halts like `req()` when there is none;
- errors surface on the next read, as they would from any reactive.

Because they are real reactives, `bindEvent()`, `debounce()`, `req()`, `observe()` and modules all work unchanged. See [How dependency capture works](#how-dependency-capture-works) for what can and cannot appear inside an async expression. `vignette("extendedtask")` writes one app both ways, with `ExtendedTask` and with shinyasync, piece by piece.

Documentation: <https://dawiegriesel.github.io/shinyasync/>

## Installation

```r
# install.packages("pak")
pak::pak("dawiegriesel/shinyasync")
```

Requires R ≥ 4.1, shiny ≥ 1.8.1 and mirai ≥ 2.5.0.

## Quick start

```r
library(shiny)
library(shinyasync)

ui <- fluidPage(
  sliderInput("n", "Rows", 1e3, 1e7, 1e5),
  plotOutput("p"),
  textOutput("status")
)

server <- function(input, output, session) {
  r <- reactiveAsync({
    x <- rnorm(input$n)               # runs in a worker, not the session
    density(x)
  })

  output$p      <- renderPlot(plot(r()))
  output$status <- renderText(status_async(r))
}

shinyApp(ui, server)
```

Drag the slider: the previous density stays on screen while the new one computes, and other users of the app are never blocked. The first task starts a daemon pool shared by every session in the process; call `pool_async()` to size it.

### Spinner or previous result

While a task runs the status is `"running"` whether or not there is a previous
value on screen. `has_value_async()` tells the two apart:

```r
output$note <- renderUI({
  running <- status_async(r) == "running"
  if (running && !has_value_async(r)) icon("spinner", class = "fa-spin")
  else if (running) span(class = "text-muted", "Showing the previous result while the new one computes")
})
```

### Button-triggered

Use `bindEvent()` exactly as you would with `reactive()`:

```r
r <- reactiveAsync({ fit_model(input$formula, data()) }) |>
  bindEvent(input$go)
```

A cancel button calls `cancel_async(r)`. Afterwards the status is `"idle"`
while `has_value_async(r)` stays `TRUE`, and the previous result remains on
screen until the next click starts a new task.

### Hidden outputs

A task starts the first time its reactive or status is read. Shiny suspends
outputs in hidden tabs, so a task that only hidden outputs read does not start
until the tab is opened. To start it on load, as in plain Shiny:

```r
outputOptions(output, "forecast", suspendWhenHidden = FALSE)
```

### Debounced input

```r
q <- debounce(reactive(input$query), 400)

r <- reactiveAsync({ search_db(q()) })
```

### Side effects

```r
observeAsync({
  write_audit_log(input$user, results())
})
```

### Background refresh

```r
r <- reactiveAsync({
  invalidateLater(60e3)
  fetch_dashboard_data()
})
```

### Manual control and fan-out

`reactiveValAsync()` is the imperative counterpart: a value you set from an
observer rather than derive, as with `reactiveVal()`. `mirai_async()` starts a
task with the same capture as `reactiveAsync()` and returns the plain `mirai`
its setter accepts, so one observer can start a task per item:

```r
forecasts <- lapply(spots, function(spot) reactiveValAsync())

observe({
  for (i in seq_along(spots)) {
    # fetch_marine() and its helpers are shipped; input$days is a dependency.
    forecasts[[i]](mirai_async(fetch_marine(spots[[i]], days = input$days)))
  }
}) |> bindEvent(input$refresh)

output$table <- renderTable(do.call(rbind, lapply(forecasts, function(f) f())))
```

For a single button-triggered task, `reactiveAsync() |> bindEvent()` above is
all you need.

### Chained async reactives

An async reactive can read another one, exactly as a `reactive()` would:

```r
wind   <- reactiveAsync(fetch_wind(input$spot))
scored <- reactiveAsync(score(wind(), input$spot))
```

While `wind` is catching up with a new `input$spot`, `scored` keeps its previous
value rather than combining the new spot with wind fetched for the old one, and
re-runs once `wind` has settled. That is what a chain of `reactive()` expressions
does, so nothing in the expression changes. Staleness follows a chain and an
error in `wind` propagates down it.

### Modules

Nothing changes inside `moduleServer()`: the namespaced `input` and the module's local objects are captured like any others, and the async reactive can be returned to the caller.

```r
densityServer <- function(id, sd = 1) {
  moduleServer(id, function(input, output, session) {
    simulate <- function(n) density(rnorm(n, sd = sd))   # shipped to the worker

    d <- reactiveAsync(simulate(input$n))

    output$p      <- renderPlot(plot(d()))
    output$status <- renderText(status_async(d))
    d
  })
}

server <- function(input, output, session) {
  left  <- densityServer("left", sd = 1)
  right <- densityServer("right", sd = 3)

  output$both <- renderText(sprintf("%.3f / %.3f", left()$bw, right()$bw))
}
```

The full app is in `inst/examples/modules/app.R`.

### A complete app

[shinyasync-example](https://github.com/dawiegriesel/shinyasync-example) is a
surf forecast dashboard that uses every primitive in the package: parallel API
fetches with stale-while-revalidate, a chained async score, a button-triggered
simulation with cancel, a background refresh, an `observeAsync()` cache writer,
a raw mirai feeding `reactiveValAsync()`, a module mounted twice, and a pool
preloaded with a request template through `pool_async(everywhere = )`. A task
monitor in the sidebar shows the status of each one.

## API

### `reactiveAsync(expr, mode = c("latest", "queue"), globals = NULL)`

Returns a reactive. `expr` is evaluated in a worker. Reactive dependencies are captured on the session thread and their resolved values shipped to the worker.

- `mode = "latest"` (default): a new invalidation cancels the running task.
- `mode = "queue"`: tasks run in order, matching `ExtendedTask`'s default.
- `globals`: a named list of extra objects for the worker, for values capture cannot find on its own.

### `observeAsync(expr, ..., globals = NULL)`

Like `observe()`, but `expr` runs in a worker. Returns an observer handle with `$status()`, `$cancel()`, `$destroy()`, `$suspend()` and `$resume()`. Nothing reads an observer's result, so errors from `expr` are reported with `message()` and the status becomes `"error"` until a dependency changes.

### `reactiveValAsync(value = NULL)`

A `reactiveVal` whose setter accepts a `mirai` (or a `promise`). The value updates when the task resolves.

### `mirai_async(expr, ..., globals = NULL)`

Runs `expr` in a worker with the same dependency capture as `reactiveAsync()` and returns the plain `mirai`, to set on a `reactiveValAsync()`, typically one per item in a fan-out. Reactive reads and `req()` are evaluated on the session when the call is made, so inside an `observe()` they register on that observer. `...` goes to `mirai::mirai()` (e.g. `.timeout`); extra data goes in `globals`.

### `status_async(r)`

Reactive status of an async reactive: `"idle"`, `"running"`, `"success"` or `"error"`. Use it for spinners and disabled buttons. Reading the status of a `reactiveAsync()` that has not run yet starts it, the same way reading its value does.

### `has_value_async(r)`

Reactive read that is `TRUE` when reading `r` would return a value rather than halt or throw: a result has arrived and the last run did not fail. Combine it with `status_async()` to show a spinner on first load and a "previous result" note during a refresh.

### `cancel_async(r)`

Stops the running task, if any, and drops queued tasks. The last successful value is kept and the status becomes `"idle"`, so `has_value_async()` stays `TRUE`; a `bindEvent()` task stays in that state until its next event, and an event still waiting for an upstream async reactive is dropped.

### `pool_async(workers = NULL, ..., everywhere = NULL)`

Starts a `mirai` daemon pool shared by every session in the R process. Optional: the first task that runs without daemons starts one with the defaults and says so. The pool is reference-counted: each session that calls it holds it until the session ends, a call outside a session holds it until the app stops, and the pool shuts down when the last holder releases it. `...` passes through to `mirai::daemons()`, so remote or TLS-backed pools work as usual. `everywhere` is an expression run once on every worker when the pool starts, for data, models and connections that every task needs; see [Preloading the workers](#preloading-the-workers). If you already manage `daemons()` yourself, skip this.

## How dependency capture works

An async expression is split in two. Reactive reads that appear literally in it, such as `input$x`, `input[["x"]]`, `rv$y`, `data()` or `mod$data()`, are evaluated on the session thread, where they register dependencies as usual. Their values, together with any user-defined functions and objects the expression refers to, are sent to a worker that evaluates the rest. Packages attached in the session and used by the expression are attached in the worker.

`print(r)` shows the split for an async reactive: the calls evaluated on the
session, the objects and packages shipped to the worker, and the rewritten
worker expression. Nothing is evaluated and the task is not started. In
`shiny::devmode()` each async reactive prints itself on its first run, and each
`mirai_async()` call site the first time it runs in a session, without the
worker expression, so the console shows what every task shipped; set
`options(shinyasync.print = FALSE)` to turn that off.

```r
simulate <- function(n, sd) density(rnorm(n, sd = sd))
r <- reactiveAsync(simulate(input$n, sd = sd))
print(r)
#> ── <reactiveAsync> reactiveAsync(simulate(input$n, sd = sd)) ────────────
#>   status   idle
#>   session  .sa_1 input$n
#>   shipped  sd <numeric>, simulate()
#>   attached stats
#>   worker   simulate(.sa_1, sd = sd)
```

Limits that follow from this:

- **Reads must be literal.** Reads inside helper functions or with computed names (`input[[paste0("x", i)]]`) cannot be resolved. Read such values in a separate `reactive()` first, or pass them with `globals = list(...)`.
- **Every read is a dependency**, even in a branch that is not taken: `if (input$a) input$b else 0` re-runs when `input$b` changes. Wrap a read in `isolate()` to use its value without depending on it.
- **`req()` and `validate()` run on the session** only when they are certain to run, i.e. not inside `if`, `&&`, `||`, `switch`, loops or functions, and not when they use variables created inside the expression. Otherwise they run in the worker, where a failure keeps the previous value.
- **Reads inside formulas are hoisted**, so `lm(y ~ input$x)` fits on the value of `input$x` under a placeholder name. Other formula symbols are shipped if they name a plain object in the session.
- **Assigning to reactive values** (`rv$x <- ...`) inside the expression is an error. Do that in an `observe()` that reads the result.
- **Shipped objects are sent on every run.** A session object the expression refers to is serialized with every task, so a large data frame at the top of `app.R` costs its size on every invalidation. Load such objects once on every worker with `pool_async(everywhere = )` instead; the names it creates are *resident* and left out of what a task ships.
- **An upstream `reactiveAsync()` is waited for.** The expression keeps its previous value while an upstream it reads is catching up with its own inputs, reports `"running"` while that upstream runs, and re-runs once it has settled. In `mode = "latest"` a run already computing on the old upstream value is dropped. With an upstream gated by `bindEvent()`, a downstream that also reads one of the upstream's inputs therefore waits for the next event instead of recomputing with the old upstream value; wrap the upstream read in `isolate()` if you want neither the wait nor the dependency. A downstream gated by `bindEvent()` whose event fires while its upstream is running runs once the upstream settles, as a synchronous `reactive()` would have been ready at the click; if the upstream is stale but not running, the event is dropped.

## Testing

`shiny::testServer()` works as usual once the test lets the worker finish:
pump `later::run_now()` and `session$flushReact()` until the status settles.
`vignette("testing")` has the helper and tests for values, buttons, errors
and modules.

## Deployment

`shinyasync` is deployment-agnostic. Each R process gets its own daemon pool, sized to one less than the cores available to it unless you call `pool_async()` (a container's CPU quota or cpuset counts, as on Posit Connect), so under a multi-process load balancer such as [faucet](https://github.com/ixpantia/faucet) or Posit Connect you should size `workers` so that `processes × workers` fits the host, or point every process at one shared remote daemon set:

```r
pool_async(url = "tcp://daemon-host:5555")
```

### Monitoring the pool

Tasks run first come, first served across one pool, so a button-triggered task
can wait behind background refreshes. `mirai::status()` reports the worker
count, how many tasks are executing and how many are waiting for a worker, so a
task monitor can show why something has not started:

```r
output$pool <- renderText({
  invalidateLater(1000)
  s <- mirai::status()
  sprintf("%d workers, %d running, %d queued",
          s$connections, s$mirai[["executing"]], s$mirai[["awaiting"]])
})
```

Each worker attaches a package the first time a task needs it; later tasks on
that worker find it already attached.

### Preloading the workers

Objects a task refers to are serialized to the worker on every run, and a
database connection cannot be serialized at all. Run the code that creates them
once on every worker instead:

```r
pool_async(workers = 4, everywhere = {
  sales <- arrow::read_parquet("data/sales.parquet")  # one copy per worker
  con <- DBI::dbConnect(RSQLite::SQLite(), "app.db")  # cannot be serialized
  rates <- fx_rates                                   # shipped once from the session
  .Last <- function() DBI::dbDisconnect(con)          # runs when the worker exits
})

server <- function(input, output, session) {
  by_region <- reactiveAsync(summarise_sales(sales, con, input$region))
}
```

The expression runs with `mirai::everywhere()` when the pool starts, before any
task. Objects it refers to are resolved in the calling environment and sent
once; the names it assigns and those objects are *resident*, so tasks do not
ship them even when the session holds an object of the same name, and
`print()` shows them on a `resident` line. Call it at the top level of `app.R`
so the preload runs once per process and connections live until the app stops.
Resident data is static: data that changes belongs in a reactive read. Every
worker holds its own copy and opens its own connection, so size memory and the
database's connection limit by `processes × workers`. To close what the
expression opened, assign a `.Last` function in it: R runs it when the worker
process exits, on pool shutdown and when the app process dies, and the pool
does not wait for it. A daemon that joins after the preload, as
a remote daemon may, does not receive it; a task using a resident name there
fails with a message saying so.

## Design notes

- **Standard API first.** No new verbs. If a Shiny idiom works with `reactive()`, it should work here.
- **mirai only.** `crew` already covers autoscaling and heterogeneous workers; keeping to mirai keeps the package small.
- **Pure R.** The dispatch path is already native (nanonext/NNG, later); the time goes to the user's code in the worker. Rust or C++ would add install friction without measurable gain.
- **Cancellation is built in.** `ExtendedTask` has no cancel method; `shinyasync` holds the mirai reference and calls `stop_mirai()` for you.

## Status and feedback wanted

Experimental. The API may still change until the package is published to CRAN. See the [changelog](https://dawiegriesel.github.io/shinyasync/news/) for changes.

The package is not on CRAN yet on purpose. Dependency capture rewrites your expression, and real apps are the best way to find the cases it gets wrong. If you try it, please report:

- anything that behaves differently from `reactive()`, `observe()` or `reactiveVal()`;
- expressions that fail in the worker but work in a plain reactive, with the
  output of `print(r)`;
- results from remote daemon pools or multi-process deployments.

Bugs go in [Issues](https://github.com/dawiegriesel/shinyasync/issues). Questions, design feedback and "it worked for me" reports go in [Discussions](https://github.com/dawiegriesel/shinyasync/discussions).

## Related

- [shinyasync-example](https://github.com/dawiegriesel/shinyasync-example) — a full dashboard built on the package
- [mirai](https://mirai.r-lib.org/) — the async engine
- [Shiny ExtendedTask](https://shiny.posit.co/r/reference/shiny/latest/extendedtask.html) — the mechanism this is modelled on
- [crew](https://wlandau.github.io/crew/) — autoscaling worker pools, if you outgrow this
- [faucet](https://github.com/ixpantia/faucet) — multi-process load balancing for Shiny and Plumber

## License

MIT
