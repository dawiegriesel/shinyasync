# shinyasync <img src="man/figures/logo.png" align="right" height="139" alt="" />

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

Because they are real reactives, `bindEvent()`, `debounce()`, `req()`, `observe()` and modules all work unchanged. See [How dependency capture works](https://dawiegriesel.github.io/shinyasync/articles/capture.html) for what can and cannot appear inside an async expression. `vignette("extendedtask")` writes one app both ways, with `ExtendedTask` and with shinyasync, piece by piece.

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
  selectInput("x", "Predictor of mpg", c("wt", "hp", "disp", "qsec")),
  sliderInput("B", "Bootstrap resamples", 500, 5000, 2000, step = 500),
  plotOutput("slopes"),
  textOutput("status")
)

server <- function(input, output, session) {
  slopes <- reactiveAsync({
    f <- reformulate(input$x, "mpg")    # runs in a worker, not the session
    replicate(input$B, {
      resample <- mtcars[sample(nrow(mtcars), replace = TRUE), ]
      coef(lm(f, resample))[[2]]
    })
  })

  output$slopes <- renderPlot(hist(slopes(), main = "Bootstrap slopes", xlab = "Slope"))
  output$status <- renderText(status_async(slopes))
}

shinyApp(ui, server)
```

Change the predictor while a fit is running: the run on the old predictor is cancelled, the previous histogram stays on screen until the new one arrives, and other users of the app are never blocked. The first task starts a daemon pool shared by every session in the process; call `pool_async()` at the top level of `app.R` to size it.

## Learn more

- [Get started](https://dawiegriesel.github.io/shinyasync/articles/shinyasync.html): spinners, buttons and cancel, debounced input, side effects, background refresh, fan-out, chained async reactives and modules.
- [How dependency capture works](https://dawiegriesel.github.io/shinyasync/articles/capture.html): what an async expression can contain, and how `print()` shows what it ships.
- [Deployment and the daemon pool](https://dawiegriesel.github.io/shinyasync/articles/deployment.html): sizing the pool, multi-process servers, monitoring and preloading the workers.
- [Testing async reactives](https://dawiegriesel.github.io/shinyasync/articles/testing.html) with `shiny::testServer()`.
- [ExtendedTask and shinyasync side by side](https://dawiegriesel.github.io/shinyasync/articles/extendedtask.html): one app written both ways.
- [shinyasync-example](https://github.com/dawiegriesel/shinyasync-example): a surf forecast dashboard that uses every primitive in the package.

## API

- [`reactiveAsync()`](https://dawiegriesel.github.io/shinyasync/reference/reactiveAsync.html): the async `reactive()`.
- [`observeAsync()`](https://dawiegriesel.github.io/shinyasync/reference/observeAsync.html): the async `observe()`.
- [`reactiveValAsync()`](https://dawiegriesel.github.io/shinyasync/reference/reactiveValAsync.html): the async `reactiveVal()`, set with a `mirai`.
- [`mirai_async()`](https://dawiegriesel.github.io/shinyasync/reference/mirai_async.html): starts a `mirai` with the same capture, for fan-out into `reactiveValAsync()`.
- [`status_async()`](https://dawiegriesel.github.io/shinyasync/reference/status_async.html): `"initial"`, `"running"`, `"success"`, `"error"` or `"idle"`, for spinners and disabled buttons.
- [`has_value_async()`](https://dawiegriesel.github.io/shinyasync/reference/has_value_async.html): tells a first run from a refresh.
- [`cancel_async()`](https://dawiegriesel.github.io/shinyasync/reference/cancel_async.html): stops the running task and keeps the last value.
- [`bind_task_button()`](https://dawiegriesel.github.io/shinyasync/reference/bind_task_button.html): bslib's task button methods for the async objects.
- [`pool_async()`](https://dawiegriesel.github.io/shinyasync/reference/pool_async.html): sizes and preloads the daemon pool.

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
