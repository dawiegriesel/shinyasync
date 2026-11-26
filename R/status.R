#' Status of an async reactive
#'
#' A reactive read of the task status, for spinners and disabled buttons.
#' Reading the status of a [reactiveAsync()] that has not run yet starts it,
#' just as reading its value would, so a UI that waits for `"success"` before
#' reading the value does not wait forever. A [reactiveAsync()] that is waiting
#' for an upstream async reactive reports `"running"` while that upstream runs.
#'
#' @param r An object created by [reactiveAsync()], [observeAsync()] or
#'   [reactiveValAsync()].
#'
#' @return One of `"initial"`, `"running"`, `"success"`, `"error"` or `"idle"`.
#'   The first four are the statuses of a [shiny::ExtendedTask]: `"initial"`
#'   until the first task is sent to a worker, never again afterwards.
#'   `"idle"` is a task that has been sent but has no value: it was cancelled
#'   with [cancel_async()], or it halted on the worker in a `req()` before any
#'   result.
#' @export
#' @seealso [has_value_async()] to tell a first run from a refresh,
#'   [cancel_async()] to stop the task, and `vignette("testing")` to test
#'   status changes with [shiny::testServer()].
#' @examplesIf interactive()
#' library(shiny)
#' library(shinyasync)
#' ui <- fluidPage(
#'   numericInput("n", "Seconds of work", 2, min = 1, max = 10),
#'   actionButton("go", "Run"),
#'   textOutput("status"),
#'   textOutput("result")
#' )
#' server <- function(input, output, session) {
#'   result <- reactiveAsync({
#'     Sys.sleep(input$n)
#'     paste("Slept for", input$n, "seconds")
#'   }) |> bindEvent(input$go)
#'   # "initial" before the first click, then "running" and "success".
#'   output$status <- renderText(paste("Status:", status_async(result)))
#'   output$result <- renderText(result())
#' }
#' shinyApp(ui, server)
status_async <- function(r) {
  task <- get_task(r, "status_async")
  task$start()
  task$current_status()
}

#' Does an async reactive have a value?
#'
#' A reactive read that is `TRUE` exactly when reading `r` would return a
#' value rather than halt or throw: a result has arrived and the last run did
#' not fail. [status_async()] alone cannot tell a first run from a refresh,
#' since both are `"running"`; combine the two to show a spinner on first load
#' and a "showing the previous result" note while a new one computes.
#'
#' Like [status_async()], reading it starts a [reactiveAsync()] that has not
#' run yet. Inside an async expression it is evaluated on the session thread,
#' like a reactive read. Use the `req()` form rather than
#' `if (has_value_async(up)) up() else NA` there: every reactive read in an
#' async expression is a dependency, so the `up()` in the untaken branch halts
#' the expression until `up` has a value and the fallback never shows.
#'
#' A [reactiveValAsync()] has a value from the start, its initial one if
#' nothing else, like [shiny::reactiveVal()], unless its last task failed. For
#' an [observeAsync()] it is `TRUE` once a run has succeeded and the last run
#' did not fail.
#'
#' @inheritParams status_async
#' @return `TRUE` or `FALSE`.
#' @export
#' @seealso [status_async()] and [cancel_async()].
#' @examplesIf interactive()
#' library(shiny)
#' library(shinyasync)
#' server <- function(input, output, session) {
#'   # fetch_forecast() is defined in the app's R/ directory.
#'   forecast <- reactiveAsync(fetch_forecast(input$spot))
#'   output$forecast <- renderTable(forecast())
#'   output$note <- renderUI({
#'     running <- status_async(forecast) == "running"
#'     if (running && !has_value_async(forecast)) {
#'       icon("spinner", class = "fa-spin")
#'     } else if (running) {
#'       span(class = "text-muted", "Showing the previous forecast while the new one loads")
#'     }
#'   })
#' }
has_value_async <- function(r) {
  task <- get_task(r, "has_value_async")
  task$start()
  task$has_value()
}

#' Cancel a running async task
#'
#' Stops the running task, if any, and drops queued tasks. The last successful
#' value is kept and the status becomes `"idle"`, so [has_value_async()] stays
#' `TRUE` and reading the reactive still returns that value. A task gated with
#' [shiny::bindEvent()] stays in that state until its next event, and an event
#' still waiting for an upstream async reactive is dropped; any other
#' task re-runs when a dependency changes. Interrupting a task that is
#' already executing requires a daemon pool with a dispatcher, which
#' [pool_async()] always uses; otherwise the result is discarded when it
#' arrives.
#'
#' @inheritParams status_async
#' @return `NULL`, invisibly.
#' @export
#' @seealso [status_async()], which is `"idle"` after a cancel, and
#'   [has_value_async()], which stays `TRUE`.
#' @examplesIf interactive()
#' library(shiny)
#' library(shinyasync)
#' ui <- fluidPage(
#'   actionButton("go", "Simulate"),
#'   actionButton("cancel", "Cancel"),
#'   textOutput("status"),
#'   verbatimTextOutput("summary")
#' )
#' server <- function(input, output, session) {
#'   sims <- reactiveAsync({
#'     Sys.sleep(5)  # stands in for a slow simulation
#'     rnorm(1e5)
#'   }) |> bindEvent(input$go)
#'   observe(cancel_async(sims)) |> bindEvent(input$cancel)
#'   # A cancel keeps the previous summary on screen.
#'   output$status <- renderText(status_async(sims))
#'   output$summary <- renderPrint(summary(sims()))
#' }
#' shinyApp(ui, server)
cancel_async <- function(r) {
  get_task(r, "cancel_async")$cancel()
}
