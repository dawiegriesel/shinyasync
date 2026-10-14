# shinyasync

<!-- badges: start -->
[![R-CMD-check](https://github.com/dawiegriesel/shinyasync/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dawiegriesel/shinyasync/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![GitHub release](https://img.shields.io/github/v/release/dawiegriesel/shinyasync?label=version)](https://github.com/dawiegriesel/shinyasync/releases)
[![pkgdown](https://github.com/dawiegriesel/shinyasync/actions/workflows/pkgdown.yaml/badge.svg)](https://dawiegriesel.github.io/shinyasync/)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE.md)
<!-- badges: end -->

> Async Shiny reactives on `mirai`, with the standard Shiny API.

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

Because they are real reactives, `bindEvent()`, `debounce()`, `req()`, `observe()` and modules all work unchanged. See [How dependency capture works](#how-dependency-capture-works) for what can and cannot appear inside an async expression.

Documentation: <https://dawiegriesel.github.io/shinyasync/>

## Installation

```r
# install.packages("pak")
pak::pak("dawiegriesel/shinyasync")
```

Requires R ≥ 4.1, shiny ≥ 1.8.1 and mirai ≥ 2.0.

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
  pool_async(workers = 4)             # daemons shared by all sessions in this process

  r <- reactiveAsync({
    x <- rnorm(input$n)               # runs in a worker, not the session
    density(x)
  })

  output$p      <- renderPlot(plot(r()))
  output$status <- renderText(status_async(r))
}

shinyApp(ui, server)
```

Drag the slider: the previous density stays on screen while the new one computes, and other users of the app are never blocked.

### Button-triggered

Use `bindEvent()` exactly as you would with `reactive()`:

```r
r <- reactiveAsync({ fit_model(input$formula, data()) }) |>
  bindEvent(input$go)
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

### Manual control

```r
v <- reactiveValAsync()

observe({
  # A raw mirai ships only what you pass it; functions must exist in the worker.
  v(mirai::mirai(summary(rnorm(n)), n = input$n))   # resolves in the background
})

output$t <- renderPrint(v())
```

### Chained async reactives

An async reactive can read another one. While the upstream task runs its
reactive returns the previous value, so a downstream expression can briefly see
a new input next to an upstream value computed from the old one. Guard the
upstream read with `stale_async()` to wait until it has caught up:

```r
wind <- reactiveAsync(fetch_wind(input$spot))

scored <- reactiveAsync({
  req(!stale_async(wind))                 # keep the old score until wind matches input$spot
  score(wind(), input$spot)
})
```

Staleness follows a chain, so one guard protects the link it is in, and an error
in `wind` propagates through the guard as it would through `reactive()`.
`inputs_async(wind)` returns the values `wind()` was computed from, keyed by
the deparsed read, e.g. `inputs_async(wind)[["input$spot"]]`.

### Modules

Nothing changes inside `moduleServer()`: the namespaced `input` and the module's local objects are captured like any others, and the async reactive can be returned to the caller. Call `pool_async()` once in the top-level server.

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
  pool_async(workers = 2)

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
a raw mirai feeding `reactiveValAsync()`, and a module mounted twice. A task
monitor in the sidebar shows the status of each one.

## API

### `reactiveAsync(expr, mode = c("latest", "queue"))`

Returns a reactive. `expr` is evaluated in a worker. Reactive dependencies are captured on the session thread and their resolved values shipped to the worker.

- `mode = "latest"` (default): a new invalidation cancels the running task.
- `mode = "queue"`: tasks run in order, matching `ExtendedTask`'s default.

### `observeAsync(expr, ...)`

Like `observe()`, but `expr` runs in a worker. Returns an observer handle with `$status()`, `$cancel()`, `$destroy()`, `$suspend()` and `$resume()`. Nothing reads an observer's result, so errors from `expr` are reported with `message()` and the status becomes `"error"` until a dependency changes.

### `reactiveValAsync(value = NULL)`

A `reactiveVal` whose setter accepts a `mirai` (or a `promise`). The value updates when the task resolves.

### `status_async(r)`

Reactive status of an async reactive: `"idle"`, `"running"`, `"success"` or `"error"`. Use it for spinners and disabled buttons. Reading the status of a `reactiveAsync()` that has not run yet starts it, the same way reading its value does.

### `stale_async(r)` and `inputs_async(r)`

Provenance of a `reactiveAsync()` result. `inputs_async(r)` is a reactive read of the values the current result was computed from, keyed by the deparsed read (`"input$x"`, `"data()"`), or `NULL` before the first result. `stale_async(r)` is `TRUE` when there is no result yet, when those reads now evaluate to something different, or when an upstream `reactiveAsync()` that `r` reads is itself stale; it is `FALSE` otherwise, including while a task re-runs with the same inputs. Reads inside `isolate()` are not part of the provenance. An error in `r` is re-thrown, as on a read of `r()`.

### `cancel_async(r)`

Stops the running task, if any, and drops queued tasks. The last successful value is kept and the status becomes `"idle"`.

### `pool_async(workers = NULL, ...)`

Starts a `mirai` daemon pool shared by every session in the R process. The pool is reference-counted: each session that calls it holds it until the session ends, a call outside a session holds it until the app stops, and the pool shuts down when the last holder releases it. Arguments pass through to `mirai::daemons()`, so remote or TLS-backed pools work as usual. If you already manage `daemons()` yourself, skip this.

## How dependency capture works

An async expression is split in two. Reactive reads that appear literally in it, such as `input$x`, `input[["x"]]`, `rv$y` or `data()`, are evaluated on the session thread, where they register dependencies as usual. Their values, together with any user-defined functions and objects the expression refers to, are sent to a worker that evaluates the rest. Packages attached in the session and used by the expression are attached in the worker.

Limits that follow from this:

- **Reads must be literal.** Reads inside helper functions or with computed names (`input[[paste0("x", i)]]`) cannot be resolved. Read such values in a separate `reactive()` first, or pass them with `globals = list(...)`.
- **Every read is a dependency**, even in a branch that is not taken: `if (input$a) input$b else 0` re-runs when `input$b` changes. Wrap a read in `isolate()` to use its value without depending on it.
- **`req()` and `validate()` run on the session** only when they are certain to run, i.e. not inside `if`, `&&`, `||`, `switch`, loops or functions, and not when they use variables created inside the expression. Otherwise they run in the worker, where a failure keeps the previous value.
- **Reads inside formulas are hoisted**, so `lm(y ~ input$x)` fits on the value of `input$x` under a placeholder name. Other formula symbols are shipped if they name a plain object in the session.
- **Assigning to reactive values** (`rv$x <- ...`) inside the expression is an error. Do that in an `observe()` that reads the result.

## Deployment

`shinyasync` is deployment-agnostic. Each R process gets its own daemon pool, so under a multi-process load balancer such as [faucet](https://github.com/ixpantia/faucet) or Posit Connect you should size `workers` so that `processes × workers` fits the host, or point every process at one shared remote daemon set:

```r
pool_async(url = "tcp://daemon-host:5555")
```

## Design notes

- **Standard API first.** No new verbs. If a Shiny idiom works with `reactive()`, it should work here.
- **mirai only.** `crew` already covers autoscaling and heterogeneous workers; keeping to mirai keeps the package small.
- **Pure R.** The dispatch path is already native (nanonext/NNG, later); the time goes to the user's code in the worker. Rust or C++ would add install friction without measurable gain.
- **Cancellation is built in.** `ExtendedTask` has no cancel method; `shinyasync` holds the mirai reference and calls `stop_mirai()` for you.

## Status and feedback wanted

Experimental. 0.1.0 is the first usable release; the API may still change before 1.0. See [NEWS.md](NEWS.md) for changes.

The package is not on CRAN yet on purpose. Dependency capture rewrites your expression, and real apps are the best way to find the cases it gets wrong. If you try it, please report:

- anything that behaves differently from `reactive()`, `observe()` or `reactiveVal()`;
- expressions that fail in the worker but work in a plain reactive;
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
