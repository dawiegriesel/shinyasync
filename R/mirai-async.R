#' Start a mirai with dependency capture
#'
#' Evaluates `expr` on a `mirai` daemon the way [reactiveAsync()] does, but
#' returns the `mirai` itself instead of a reactive, to set on a
#' [reactiveValAsync()]. The usual case is a fan-out: one observer starting a
#' task per item. For a single task derived from inputs, [reactiveAsync()]
#' with [shiny::bindEvent()] is simpler. A raw [mirai::mirai()] ships only
#' what you hand it; `mirai_async()` sends the user-defined functions and
#' objects `expr` refers to, and the functions they call, along
#' automatically, and attaches the packages it uses.
#'
#' Reactive reads in `expr` (`input$x`, `data()`) and `req()`, `validate()`,
#' `invalidateLater()` and `isolate()` calls are evaluated on the session
#' thread before the task starts, under the same rules as in
#' [reactiveAsync()]. Called inside an [shiny::observe()], they behave as they
#' would in plain Shiny: the reads register dependencies on that observer, a
#' failed `req()` halts it, and `invalidateLater()` reschedules it. Outside a
#' reactive context `expr` must not read reactives.
#'
#' The task starts immediately. Errors while resolving `expr` on the session,
#' such as a reactive read that cannot be hoisted, are raised by the call;
#' errors in the worker arrive as the mirai's error value and are re-thrown
#' when a [reactiveValAsync()] holding it is read.
#'
#' In [shiny::devmode()] each call site prints what it captured the first
#' time it runs in a session, as [reactiveAsync()] does on its first run.
#'
#' @inheritParams reactiveAsync
#' @param ... Passed to [mirai::mirai()], e.g. `.timeout` or `.compute`. Pass
#'   data to the worker with `globals`, not here.
#'
#' @return A `mirai`.
#' @seealso [reactiveValAsync()], whose setter accepts the result.
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shinyasync)
#'   server <- function(input, output, session) {
#'     # One forecast per spot, fetched in parallel on each refresh.
#'     forecasts <- lapply(spots, function(spot) reactiveValAsync())
#'     observe({
#'       for (i in seq_along(spots)) {
#'         # fetch_marine() and its helpers are shipped to the worker.
#'         forecasts[[i]](mirai_async(fetch_marine(spots[[i]], days = input$days)))
#'       }
#'     }) |> bindEvent(input$refresh)
#'
#'     output$table <- renderTable(do.call(rbind, lapply(forecasts, function(f) f())))
#'   }
#' }
mirai_async <- function(expr, ..., globals = NULL, env = parent.frame(), quoted = FALSE) {
  force(env)
  if (!quoted) expr <- substitute(expr)
  capture <- new_capture(expr, env, globals)
  m <- dispatch_job(capture$resolve(), ...)
  devmode_print_call(default_label("mirai_async", expr), capture)
  m
}
