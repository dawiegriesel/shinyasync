# Task engine --------------------------------------------------------------
#
# Mirrors ExtendedTask's status vocabulary and queueing, but owns its state so
# that running work can be cancelled. Every dispatch bumps a generation
# counter; a result whose generation is stale was superseded or cancelled and
# is dropped without touching status, value or error.

new_async_task <- function(mode = c("latest", "queue"), label = "shinyasync",
                           domain = shiny::getDefaultReactiveDomain()) {
  mode <- match.arg(mode)
  st <- new.env(parent = emptyenv())

  st$status <- shiny::reactiveVal("idle", label = paste0(label, "$status"))
  # Wrapped in a list so that NULL results are distinguishable from "no value".
  st$value <- shiny::reactiveVal(NULL, label = paste0(label, "$value"))
  st$error <- shiny::reactiveVal(NULL, label = paste0(label, "$error"))
  # The reactive reads the current value was computed from; see inputs_async().
  st$inputs <- shiny::reactiveVal(NULL, label = paste0(label, "$inputs"))
  st$state <- "idle"
  st$gen <- 0L
  st$current <- NULL
  st$queue <- list()
  st$trigger <- NULL
  # Called with the condition whenever a task fails; observeAsync() reports it.
  st$on_error <- NULL
  # Starts a lazily created task. reactiveAsync() replaces this with a function
  # that resumes its suspended trigger; a read of the value or of the status
  # both count as interest in the result.
  st$start <- function() invisible()

  # Mirror status in a plain field so it can be checked outside reactive
  # contexts, including after the session has ended.
  set_status <- function(status) {
    st$state <- status
    st$status(status)
  }
  is_running <- function() identical(st$state, "running")

  settled_status <- function() if (is.null(shiny::isolate(st$value()))) "idle" else "success"

  # Whether a read would return a value rather than halt or throw. Reads both
  # reactive values unconditionally so callers depend on both; a value is only
  # ever stored together with a cleared error (see succeed()).
  st$has_value <- function() {
    has_error <- !is.null(st$error())
    has_value <- !is.null(st$value())
    !has_error && has_value
  }

  stop_current <- function() {
    if (inherits(st$current, "mirai")) mirai::stop_mirai(st$current)
    st$current <- NULL
  }

  next_job <- function() {
    if (length(st$queue)) {
      job <- st$queue[[1]]
      st$queue <- st$queue[-1]
      st$dispatch(job)
    }
  }

  st$invoke <- function(job) {
    if (is_running()) {
      if (mode == "queue") {
        st$queue[[length(st$queue) + 1L]] <- job
        return(invisible())
      }
      stop_current()
    }
    st$dispatch(job)
  }

  st$dispatch <- function(job) {
    warn_if_no_daemons()
    # mirai() only treats `.expr` as a language object when given a symbol.
    expr <- job$expr
    m <- mirai::mirai(.expr = expr, list2env(job$globals), .args = job$args)
    st$adopt(m, job$inputs)
  }

  # Track any mirai or promise as the current task. `inputs` are stored with
  # the value if this task succeeds.
  st$adopt <- function(x, inputs = NULL) {
    if (!identical(st$current, x)) stop_current()
    st$gen <- gen <- st$gen + 1L
    st$current <- x
    done <- FALSE

    # Settle at most once, and only if this task is still the current one.
    finish <- function(commit) {
      if (done || gen != st$gen) return(invisible())
      done <<- TRUE
      commit()
      next_job()
    }

    if (inherits(x, "mirai")) {
      settle <- function(...) finish(function() settle_mirai(x$data, inputs))
      promises::then(x, onFulfilled = settle, onRejected = settle)
      # The promise notification for a mirai that resolves very quickly can be
      # lost (seen with mirai 2.7.3 / nanonext 1.10.3), so also poll.
      poll <- function() {
        if (done || gen != st$gen) return()
        if (mirai::unresolved(x)) later::later(poll, 0.1) else settle()
      }
      later::later(poll, 0.1)
    } else {
      promises::then(
        x,
        onFulfilled = function(value) finish(function() st$succeed(value, inputs)),
        onRejected = function(e) finish(function() {
          if (inherits(e, "shiny.silent.error")) settle_silent() else st$fail(e)
        })
      )
    }

    set_status("running")
    invisible(x)
  }

  settle_mirai <- function(data, inputs) {
    if (!mirai::is_error_value(data)) {
      st$succeed(data, inputs)
    } else if ("shiny.silent.error" %in% attr(data, "condition.class")) {
      settle_silent()
    } else if (mirai::is_mirai_error(data)) {
      st$fail(task_error(attr(data, "message") %||% as.character(data), data))
    } else {
      st$fail(task_error(paste("The task did not complete:", format(data)), data))
    }
  }

  # A req() failure in the worker: keep the previous result, like req(). A
  # stale error must not survive, or reads would throw it while the status
  # reports "idle" or "success".
  settle_silent <- function() {
    st$current <- NULL
    st$error(NULL)
    set_status(settled_status())
  }

  st$succeed <- function(value, inputs = NULL) {
    st$current <- NULL
    st$value(list(value))
    st$inputs(inputs)
    st$error(NULL)
    set_status("success")
  }

  st$fail <- function(cond) {
    st$current <- NULL
    st$error(cond)
    set_status("error")
    if (is.function(st$on_error)) st$on_error(cond)
  }

  st$set <- function(value) {
    stop_current()
    st$gen <- st$gen + 1L
    st$succeed(value)
  }

  st$cancel <- function() {
    stop_current()
    st$gen <- st$gen + 1L
    st$queue <- list()
    if (is_running()) {
      st$error(NULL)
      set_status("idle")
    }
    invisible()
  }

  # Drop running and queued work without touching reactive state.
  st$abandon <- function() {
    stop_current()
    st$gen <- st$gen + 1L
    st$queue <- list()
  }

  if (!is.null(domain)) domain$onSessionEnded(st$abandon)

  st
}

# Session-side failure while preparing a task: surface it like a worker error,
# but let req()-style silent errors propagate so the observer halts quietly.
run_trigger <- function(task, capture) {
  job <- tryCatch(capture$resolve(), error = function(e) {
    if (inherits(e, "shiny.silent.error")) stop(e)
    task$cancel()
    task$fail(e)
    NULL
  })
  if (!is.null(job)) task$invoke(job)
  # Never return the mirai: shiny would treat the observer as async and close
  # the session if the task is rejected or cancelled.
  invisible(NULL)
}

task_error <- function(message, data = NULL) {
  structure(
    class = c("shinyasync_error", "error", "condition"),
    list(message = message, call = NULL, data = data)
  )
}

get_task <- function(x, caller) {
  task <- attr(x, "shinyasync", exact = TRUE)
  if (!is.environment(task)) {
    stop(
      sprintf(
        "`%s()` needs an object created by reactiveAsync(), observeAsync() or reactiveValAsync().\n",
        caller
      ),
      "Wrappers such as debounce() return plain reactives; pass the original object instead.",
      call. = FALSE
    )
  }
  task
}
