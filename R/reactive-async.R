#' Create an async reactive expression
#'
#' Like [shiny::reactive()], but `expr` is evaluated on a `mirai` daemon so the
#' session stays responsive. Reactive reads that appear in `expr`, such as
#' `input$x`, `input[["x"]]`, `rv$y` or `data()`, are evaluated on the session
#' thread, where they register dependencies as usual, and their values are sent
#' to the worker. Calls to `req()`, `validate()`, `invalidateLater()` and
#' `isolate()` also run on the session thread, unless they use variables
#' created inside `expr` or may not run at all; see Limits below.
#'
#' The task starts the first time the reactive or its [status_async()] is read,
#' so a spinner driven by the status is enough to kick it off. While a task
#' runs, reading the reactive returns the previous value, or halts silently
#' like `req()` when there is none yet. Errors raised in the worker are
#' re-thrown when the reactive is read.
#'
#' User-defined functions and objects that `expr` refers to (and functions they
#' call) are sent along automatically. Packages that are attached in the
#' session and used by `expr` are attached in the worker.
#'
#' @section Limits:
#' Reactive reads must appear literally in `expr`; reads inside helper
#' functions or with computed names (`input[[paste0("x", i)]]`) cannot be
#' resolved. Read such values in a separate [shiny::reactive()] first, or pass
#' them with `globals`.
#'
#' Every reactive read in `expr` is a dependency, even one in a branch that is
#' not taken: `if (input$a) input$b else 0` re-runs when `input$b` changes.
#' Wrap a read in [shiny::isolate()] to use its value without depending on it.
#' `req()` and `validate()` run on the session only when they are certain to
#' run, i.e. not inside `if`, `&&`, `||`, `switch`, loops or functions;
#' otherwise they run in the worker, where a failure keeps the previous value.
#'
#' Reads inside formulas are hoisted too, so `lm(y ~ input$x)` fits the model
#' on the value of `input$x` under a placeholder name. Other formula symbols
#' are shipped to the worker if they name a plain object in the session.
#'
#' Assigning to reactive values (`rv$x <- ...`) inside `expr` is an error;
#' do that in an [shiny::observe()] that reads the result.
#'
#' @param expr An expression.
#' @param mode `"latest"` (default) cancels the running task when a dependency
#'   changes. `"queue"` runs tasks in order, like [shiny::ExtendedTask].
#' @param globals A named list of extra objects to make available to `expr` in
#'   the worker.
#' @param label A label for debugging.
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Is `expr` a quoted expression?
#'
#' @return A reactive expression of class `"reactiveAsync"`. Use it with
#'   [status_async()], [has_value_async()], [stale_async()], [cancel_async()]
#'   and [shiny::bindEvent()].
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   server <- function(input, output, session) {
#'     pool_async(workers = 2)
#'     r <- reactiveAsync({
#'       Sys.sleep(1)
#'       rnorm(input$n)
#'     })
#'     output$p <- renderPlot(hist(r()))
#'   }
#'   ui <- fluidPage(sliderInput("n", "n", 10, 1000, 100), plotOutput("p"))
#'   shinyApp(ui, server)
#' }
reactiveAsync <- function(expr, mode = c("latest", "queue"), globals = NULL,
                          label = NULL, env = parent.frame(), quoted = FALSE) {
  mode <- match.arg(mode)
  force(env)
  if (!quoted) expr <- substitute(expr)
  label <- label %||% default_label("reactiveAsync", expr)

  task <- new_async_task(mode, label)
  capture <- new_capture(expr, env, globals)
  task$capture <- capture

  # Starts suspended so the task only runs once something reads the result.
  task$trigger <- shiny::observe(
    run_trigger(task, capture),
    label = paste0(label, "$trigger"),
    suspended = TRUE
  )

  started <- FALSE
  task$start <- function() {
    if (!started) {
      started <<- TRUE
      task$trigger$resume()
    }
    invisible()
  }

  r <- shiny::reactive(label = label, {
    task$start()
    if (!task$has_value()) {
      err <- task$error()
      if (!is.null(err)) stop(err)
      if (identical(task$status(), "running")) {
        shiny::req(FALSE, cancelOutput = "progress")
      }
      shiny::req(FALSE)
    }
    task$value()[[1]]
  })

  class(r) <- c("reactiveAsync", class(r))
  attr(r, "shinyasync") <- task
  r
}

default_label <- function(fn, expr) {
  text <- paste(deparse(expr, width.cutoff = 60L), collapse = " ")
  if (nchar(text) > 60L) text <- paste0(substr(text, 1L, 57L), "...")
  sprintf("%s(%s)", fn, text)
}
