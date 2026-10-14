#' Provenance of an async result
#'
#' Every run of a [reactiveAsync()] records the reactive reads it was computed
#' from: `input$x`, `data()` and the reads inside hoisted `req()` and
#' `validate()` calls, but not reads wrapped in [shiny::isolate()] and not
#' [status_async()] and friends. `inputs_async()` returns those values for the
#' current result, and `stale_async()` compares them with the same reads
#' evaluated now.
#'
#' Both are reactive reads. The main use is a chained async reactive: while a
#' task runs, reading a `reactiveAsync()` returns its previous value, so a
#' downstream expression can briefly see a new `input$x` next to an upstream
#' value computed from the old one. Guarding the upstream read with
#' `req(!stale_async(up))` keeps the downstream's previous value until the
#' upstream has caught up, and re-runs it when that happens. Staleness follows
#' a chain: if `up` itself reads another `reactiveAsync()` that is stale, `up`
#' is stale too. An error in `up` is re-thrown by `stale_async(up)`, as it
#' would be by reading `up()`, so errors propagate as in a chain of
#' [shiny::reactive()] expressions.
#'
#' Like [status_async()], reading either starts a `reactiveAsync()` that has
#' not run yet. Inside an async expression both are evaluated on the session
#' thread, like reactive reads, when their argument is a name or a literal
#' field of a list such as a module's return value (`stale_async(mod$up)`).
#'
#' Use the `req()` form rather than `if (stale_async(up)) "waiting" else up()`:
#' every reactive read in an async expression is a dependency, so the `up()`
#' in the untaken branch halts the expression until `up` has a first value and
#' the placeholder never shows on initial load.
#'
#' @param r An object created by [reactiveAsync()].
#'
#' @return `inputs_async()`: a named list of the values the current result was
#'   computed from, keyed by the deparsed read (`"input$x"`, `"data()"`; a
#'   `[[` read is keyed with double quotes however it was written), or `NULL`
#'   before the first result.
#'
#'   `stale_async()`: `TRUE` when there is no result yet or when any of the
#'   reads evaluate to something other than what the current result was
#'   computed from (compared with [identical()]); `FALSE` otherwise. A task
#'   that is re-running with the same inputs, for example after
#'   `invalidateLater()`, is not stale. A read that halts, such as a
#'   `reactive()` that calls `req()`, counts as stale. For a reactive gated
#'   with [shiny::bindEvent()] the stored inputs are those of the last event,
#'   so it is stale as soon as an input changes, before the next event.
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   server <- function(input, output, session) {
#'     pool_async(workers = 2)
#'     wind <- reactiveAsync(fetch_wind(input$spot))
#'     scored <- reactiveAsync({
#'       req(!stale_async(wind))   # wait for wind to catch up with input$spot
#'       score(wind(), input$spot)
#'     })
#'     output$score <- renderText(scored())
#'   }
#' }
inputs_async <- function(r) {
  task <- get_capture_task(r, "inputs_async")
  task$start()
  task$inputs()
}

#' @rdname inputs_async
#' @export
stale_async <- function(r) {
  task <- get_capture_task(r, "stale_async")
  task$start()
  err <- task$error()
  if (!is.null(err)) stop(err)
  value <- task$value()
  stored <- task$inputs()
  if (is.null(value)) return(TRUE)
  for (up in task$capture$upstream()) {
    if (stale_async(up)) return(TRUE)
  }
  live <- tryCatch(
    list(task$capture$inputs()),
    shiny.silent.error = function(e) NULL
  )
  if (is.null(live)) return(TRUE)
  !identical(stored, live[[1]])
}

get_capture_task <- function(x, caller) {
  if (!inherits(x, "reactiveAsync") ||
      !is.environment(attr(x, "shinyasync", exact = TRUE))) {
    stop(
      sprintf("`%s()` needs an object created by reactiveAsync().\n", caller),
      "Wrappers such as debounce() return plain reactives; pass the original object instead.",
      call. = FALSE
    )
  }
  attr(x, "shinyasync", exact = TRUE)
}
