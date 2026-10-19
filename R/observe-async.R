#' Create an async observer
#'
#' Like [shiny::observe()], but `expr` runs on a `mirai` daemon. Dependencies
#' are captured the same way as in [reactiveAsync()]. When a dependency changes
#' while a task is running, the running task is cancelled and a new one starts.
#'
#' Unlike [reactiveAsync()], it does not wait to be read: like
#' [shiny::observe()], it runs on the next reactive flush. There is no
#' `mode = "queue"`; for work that must run once per change, use
#' `reactiveAsync(mode = "queue")` and read its result.
#'
#' Errors raised by `expr`, in the worker or while resolving its reactive
#' dependencies, are reported with [message()] and leave the observer in
#' status `"error"` until a dependency changes; the session stays open.
#'
#' @inheritParams reactiveAsync
#' @param ... Passed to [shiny::observe()], e.g. `priority`, `suspended` or
#'   `domain`.
#'
#' @return An observer handle of class `"observeAsync"`: a list with methods
#'   `status()`, `cancel()`, `destroy()`, `suspend()` and `resume()`.
#' @export
observeAsync <- function(expr, ..., globals = NULL, label = NULL,
                         env = parent.frame(), quoted = FALSE) {
  force(env)
  if (!quoted) expr <- substitute(expr)
  label <- label %||% default_label("observeAsync", expr)

  domain <- list(...)$domain %||% shiny::getDefaultReactiveDomain()
  task <- new_async_task("latest", label, domain = domain)
  task$kind <- "observeAsync"
  capture <- new_capture(expr, env, globals)
  task$capture <- capture

  # Nothing reads an observer's result, so a failure would otherwise be lost.
  task$on_error <- function(cond) {
    message(sprintf("Error in %s: %s", label, conditionMessage(cond)))
  }

  task$trigger <- shiny::observe(run_trigger(task, capture), label = label, ...)

  structure(
    list(
      status = function() task$current_status(),
      cancel = function() task$cancel(),
      destroy = function() {
        task$cancel()
        task$trigger$destroy()
      },
      suspend = function() task$trigger$suspend(),
      resume = function() task$trigger$resume()
    ),
    class = "observeAsync",
    shinyasync = task
  )
}

