#' Status of an async reactive
#'
#' A reactive read of the task status, for spinners and disabled buttons.
#' Reading the status of a [reactiveAsync()] that has not run yet starts it,
#' just as reading its value would, so a UI that waits for `"success"` before
#' reading the value does not wait forever.
#'
#' @param r An object created by [reactiveAsync()], [observeAsync()] or
#'   [reactiveValAsync()].
#'
#' @return One of `"idle"`, `"running"`, `"success"` or `"error"`.
#' @export
status_async <- function(r) {
  task <- get_task(r, "status_async")
  task$start()
  task$status()
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
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   server <- function(input, output, session) {
#'     pool_async(workers = 2)
#'     forecast <- reactiveAsync(fetch_forecast(input$spot))
#'     output$forecast <- renderTable(forecast())
#'     output$note <- renderUI({
#'       running <- status_async(forecast) == "running"
#'       if (running && !has_value_async(forecast)) {
#'         icon("spinner", class = "fa-spin")
#'       } else if (running) {
#'         span(class = "text-muted", "Showing the previous forecast while the new one loads")
#'       }
#'     })
#'   }
#' }
has_value_async <- function(r) {
  task <- get_task(r, "has_value_async")
  task$start()
  task$has_value()
}

#' Cancel a running async task
#'
#' Stops the running task, if any, and drops queued tasks. The last successful
#' value is kept and the status becomes `"idle"`. Interrupting a task that is
#' already executing requires a daemon pool with a dispatcher, which
#' [pool_async()] always uses; otherwise the result is discarded when it
#' arrives.
#'
#' @inheritParams status_async
#' @return `NULL`, invisibly.
#' @export
cancel_async <- function(r) {
  get_task(r, "cancel_async")$cancel()
}
