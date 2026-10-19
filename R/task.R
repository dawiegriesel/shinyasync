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

  st$label <- label
  st$kind <- "reactiveValAsync"
  st$status <- shiny::reactiveVal("idle", label = paste0(label, "$status"))
  # Wrapped in a list so that NULL results are distinguishable from "no value".
  st$value <- shiny::reactiveVal(NULL, label = paste0(label, "$value"))
  st$error <- shiny::reactiveVal(NULL, label = paste0(label, "$error"))
  # The reactive reads the current value was computed from; see is_stale().
  st$inputs <- shiny::reactiveVal(NULL, label = paste0(label, "$inputs"))
  st$state <- "idle"
  st$gen <- 0L
  st$current <- NULL
  st$queue <- list()
  st$trigger <- NULL
  # Set by bindEvent(): the trigger only runs on its event, so a run that
  # halts on a busy upstream is re-run by a one-shot waiter observer instead
  # of by the dependencies it registered. See run_trigger().
  st$bound <- FALSE
  st$waiting <- FALSE
  st$waiter <- NULL
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

  # The status a reader sees: "running" also while this task waits for an
  # upstream that is running. Derived from the upstream's status rather than
  # stored, so it falls back to the settled status as soon as the upstream
  # stops, however it stops.
  st$current_status <- function() {
    status <- st$status()
    if (identical(status, "running") || !st$waiting) return(status)
    if (upstream_running(st)) "running" else status
  }

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

  st$dispatch <- function(job) st$adopt(dispatch_job(job), job$inputs)

  # Track any mirai or promise as the current task. `inputs` are stored with
  # the value if this task succeeds.
  st$adopt <- function(x, inputs = NULL) {
    st$unwait()
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
      # Up to nanonext 1.10.3, mirai's as.promise() checked unresolved() and
      # then registered the notification, and a task that completed between
      # the two never notified (r-lib/mirai#656; the first registration in a
      # process loads later, which is window enough for a quick task). then()
      # has returned, so the notification is registered now: a task found
      # resolved here either completed in that window or is about to notify,
      # and finish() settles once either way. nanonext 1.10.3.9000 checks for
      # completion when it registers (r-lib/nanonext#363), so the check is
      # skipped there and can go once that version is required.
      if (loses_notification()) {
        later::later(function() {
          if (!done && gen == st$gen && !mirai::unresolved(x)) settle()
        })
      }
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

  # The run halted because an upstream is stale. In "latest" mode a run on
  # the old upstream value is dropped, as a changed input would drop it; in
  # "queue" mode every run completes. Status is derived, see current_status().
  st$wait <- function() {
    if (mode == "latest" && is_running()) {
      stop_current()
      st$gen <- st$gen + 1L
      st$error(NULL)
      set_status(settled_status())
    }
    st$waiting <- TRUE
  }

  st$unwait <- function() {
    st$waiting <- FALSE
    if (!is.null(st$waiter)) {
      st$waiter$destroy()
      st$waiter <- NULL
    }
  }

  st$set <- function(value) {
    st$unwait()
    stop_current()
    st$gen <- st$gen + 1L
    st$succeed(value)
  }

  st$cancel <- function() {
    was_waiting <- st$waiting
    st$unwait()
    stop_current()
    st$gen <- st$gen + 1L
    st$queue <- list()
    if (is_running() || was_waiting) {
      st$error(NULL)
      set_status("idle")
    }
    invisible()
  }

  # Drop running and queued work without touching reactive state.
  st$abandon <- function() {
    st$unwait()
    stop_current()
    st$gen <- st$gen + 1L
    st$queue <- list()
  }

  if (!is.null(domain)) domain$onSessionEnded(st$abandon)

  st
}

# Whether the installed nanonext can lose a promise notification; see adopt().
loses_notification <- function() {
  utils::packageVersion("nanonext") < "1.10.3.9000"
}

# Send a resolved job (see new_capture()) to a worker. `...` goes to mirai().
dispatch_job <- function(job, ...) {
  ensure_pool(list(...)$.compute)
  # mirai() only treats `.expr` as a language object when given a symbol.
  expr <- job$expr
  mirai::mirai(.expr = expr, list2env(job$globals), .args = job$args, ...)
}

# Session-side failure while preparing a task: surface it like a worker error,
# but let req()-style silent errors propagate so the observer halts quietly.
run_trigger <- function(task, capture) {
  # One handler: a condition re-thrown from a tryCatch() handler is caught by
  # the handlers listed after it, so separate class handlers would turn a
  # silent halt into a failure.
  job <- tryCatch(capture$resolve(), error = function(e) {
    if (inherits(e, "shinyasync_waiting")) {
      task$wait()
      if (task$bound) arm_waiter(task, capture)
      return(NULL)
    }
    if (inherits(e, "shiny.silent.error")) {
      task$unwait()
      stop(e)
    }
    task$cancel()
    task$fail(e)
    NULL
  })
  devmode_print(task)
  if (!is.null(job)) task$invoke(job)
  # Never return the mirai: shiny would treat the observer as async and close
  # the session if the task is rejected or cancelled.
  invisible(NULL)
}

# A bound trigger runs inside isolate(), so the dependencies a waiting halt
# registered cannot re-run it. Watch the upstreams instead and, once none is
# stale, run the trigger as the event would have: isolated, with no
# dependencies. An upstream error counts as settled; run_trigger() fails the
# task with it. The event lives exactly as long as the status reads "running":
# if the upstream is stale but not running (its own req() halts, or its own
# bindEvent() has not fired), the event is dropped, as a reactive() whose
# req() fails drops it.
arm_waiter <- function(task, capture) {
  if (!is.null(task$waiter)) return(invisible())
  task$waiter <- shiny::observe(
    {
      settled <- tryCatch(!upstream_stale(capture), error = function(e) TRUE)
      if (settled) {
        task$unwait()
        shiny::isolate(run_trigger(task, capture))
      } else if (!upstream_running(task)) {
        task$unwait()
      }
    },
    label = paste0(task$label, "$waiter")
  )
  invisible()
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
