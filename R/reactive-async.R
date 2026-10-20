#' Create an async reactive expression
#'
#' Like [shiny::reactive()], but `expr` is evaluated on a `mirai` daemon so the
#' session stays responsive. Reactive reads that appear in `expr`, such as
#' `input$x`, `input[["x"]]`, `rv$y`, `data()` or `mod$data()`, are evaluated on the session
#' thread, where they register dependencies as usual, and their values are sent
#' to the worker. Calls to `req()`, `validate()`, `invalidateLater()` and
#' `isolate()` also run on the session thread, unless they use variables
#' created inside `expr` or may not run at all; see Limits below.
#'
#' The task starts the first time the reactive or its [status_async()] is read,
#' so a spinner driven by the status is enough to kick it off. An output in a
#' hidden tab is suspended and does not read, so a task only it depends on
#' starts when the tab is opened; use [shiny::outputOptions()] with
#' `suspendWhenHidden = FALSE` to start it on load. While a task runs, reading
#' the reactive returns the previous value, or halts silently like `req()`
#' when there is none yet. Errors raised in the worker are re-thrown when the
#' reactive is read.
#'
#' User-defined functions and objects that `expr` refers to (and functions they
#' call) are sent along automatically. Packages that are attached in the
#' session and used by `expr` are attached in the worker. `print()` shows the
#' split: the calls evaluated on the session, the objects and packages shipped
#' to the worker, and the rewritten worker expression, without evaluating
#' anything or starting the task. In [shiny::devmode()] each async reactive
#' prints itself on its first run; `options(shinyasync.print = FALSE)` turns
#' that off.
#'
#' An async reactive can read another one, and the chain stays consistent as
#' a chain of [shiny::reactive()] expressions does: while an upstream task is
#' catching up with its own inputs, the downstream keeps its previous value
#' instead of combining a new `input$x` with an upstream value computed from
#' the old one, reports `"running"` while the upstream runs, and re-runs once
#' the upstream has settled. In `mode = "latest"` a downstream run already
#' computing on the old upstream value is dropped; in `mode = "queue"` it
#' completes. Staleness follows the chain, and an upstream error propagates
#' down it. A downstream gated with [shiny::bindEvent()] whose event fires
#' while its upstream is running runs once the upstream settles, as a
#' synchronous `reactive()` would have been ready at the click; if the
#' upstream is stale but not running, the event is dropped. The one
#' difference from `reactive()` is an upstream gated with `bindEvent()`: a
#' downstream that also reads one of the upstream's inputs waits for the next
#' event rather than recomputing with the old upstream value.
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
#'   [status_async()], [has_value_async()], [cancel_async()]
#'   and [shiny::bindEvent()].
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   server <- function(input, output, session) {
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
  task$kind <- "reactiveAsync"
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
      if (identical(task$current_status(), "running")) {
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

# The name a call is assigned to, as shiny::reactiveVal() labels itself:
# `report <- reactiveValAsync()` gives "report". The statement's source
# reference rides on the call when the code was parsed with keep.source;
# otherwise, or when the call is not the whole right-hand side, `default`.
assign_label <- function(call, default) {
  srcref <- attr(call, "srcref", exact = TRUE)
  if (is.null(srcref)) return(unique_label(default))
  text <- tryCatch(paste(as.character(srcref), collapse = "\n"), error = function(e) "")
  m <- regmatches(text, regexec("^\\s*([^[:space:]]+)\\s*(<<-|<-|=)\\s*reactiveValAsync\\b", text))[[1]]
  if (length(m) < 2L) return(unique_label(default))
  m[2]
}

# A value not created by an assignment, as in lapply(), gets a label that is
# unique in the process, as shiny::reactiveVal() falls back to one.
label_counter <- new.env(parent = emptyenv())
unique_label <- function(default) {
  n <- (label_counter[[default]] %||% 0L) + 1L
  label_counter[[default]] <- n
  paste0(default, "#", n)
}
