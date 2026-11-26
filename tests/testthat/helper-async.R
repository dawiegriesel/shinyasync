# Tests that start daemons depend on wall-clock windows (a task must still be
# running when the test acts), which a loaded CRAN machine cannot promise.
# They run everywhere NOT_CRAN is "true": locally under testthat, and on CI.
local_pool <- function(n = 2, env = parent.frame()) {
  testthat::skip_on_cran()
  mirai::daemons(n, dispatcher = TRUE)
  withr::defer(mirai::daemons(0), envir = env)
}

# Pump the event loop and the mock session until `cond()` is TRUE.
wait_for <- function(cond, session, timeout = 15) {
  start <- Sys.time()
  repeat {
    session$flushReact()
    if (isTRUE(shiny::isolate(cond()))) {
      # Let observers see a value that arrived during the last flush.
      session$flushReact()
      return(invisible(TRUE))
    }
    if (difftime(Sys.time(), start, units = "secs") > timeout) {
      stop(
        "Timed out after ", timeout, "s waiting for: ",
        paste(deparse(body(cond)), collapse = " "),
        call. = FALSE
      )
    }
    later::run_now(0.05)
  }
}

# Read a reactive, returning a marker instead of signalling req() halts.
peek <- function(r) {
  tryCatch(shiny::isolate(r()), shiny.silent.error = function(e) "<halted>")
}

# Return the pool to its initial state between tests that start or hold one.
reset_pool <- function() {
  if (mirai::daemons_set()) mirai::daemons(0)
  pool_state$owned <- FALSE
  pool_state$refs <- 0L
  pool_state$preload <- NULL
  pool_state$shinyasync_holds_pool <- FALSE
}
