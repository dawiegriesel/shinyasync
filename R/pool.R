pool_state <- new.env(parent = emptyenv())
pool_state$refs <- 0L
pool_state$owned <- FALSE
pool_state$announced <- FALSE
# The `everywhere` expression loaded on the pool, if any; see new_preload().
pool_state$preload <- NULL

#' Start a daemon pool
#'
#' Starts a `mirai` daemon pool shared by every session in this R process. The
#' pool is reference-counted: each call made inside a session holds it until
#' that session ends, and a call made outside a session holds it until the app
#' stops. The pool shuts down when the last holder releases it.
#'
#' A hold taken outside an app, in a script or at the console, is released
#' when the next app in the process stops, since that is when
#' [shiny::onStop()] fires. A script that never runs an app keeps its pool
#' until the process exits, when the daemons exit with it; call
#' `mirai::daemons(0)` to stop it earlier.
#'
#' You need not call it. The first task that runs while no daemons are set
#' starts a pool with the defaults, with a message saying so, and the session
#' it runs in holds that pool as if it had called `pool_async()`. Call it
#' yourself, before any task, to choose the number of workers, to use
#' remote daemons or to preload the workers.
#'
#' If you already manage [mirai::daemons()] yourself, skip this; calling it
#' anyway does nothing, except that an `everywhere` expression is still loaded
#' on your daemons, since you asked for it.
#'
#' Tasks run first come, first served across the pool. [mirai::status()]
#' reports the worker count and how many tasks are executing and awaiting a
#' worker, for a task monitor that shows why a task has not started.
#'
#' @section Preloading the workers:
#' Objects an async expression refers to are sent to the worker on every run.
#' For a large data frame, a model or a database connection, run the code that
#' creates them once on every worker instead, with `everywhere`:
#'
#' ```r
#' pool_async(workers = 4, everywhere = {
#'   sales <- arrow::read_parquet("data/sales.parquet")  # one copy per worker
#'   con <- DBI::dbConnect(RSQLite::SQLite(), "app.db")  # cannot be serialized
#'   rates <- fx_rates                                   # shipped once from the session
#'   .Last <- function() DBI::dbDisconnect(con)          # runs when the worker exits
#' })
#'
#' sales_by_region <- reactiveAsync(summarise_sales(sales, con, input$region))
#' ```
#'
#' The expression runs on each worker with [mirai::everywhere()] when the pool
#' starts, before any task, and again if the pool is restarted. Objects it
#' refers to, such as `fx_rates`, are resolved in the calling environment and
#' sent along once, as a task's globals are; packages attached in the session
#' and used by the expression are attached on the workers. The names it
#' assigns and the objects sent with it are *resident*: dependency capture
#' leaves them out of what a task ships, even if the session has an object of
#' the same name, and `print()` lists them on a `resident` line.
#'
#' Call it at the top level of `app.R`, so the preload runs once per process
#' and the pool, with its connections, lives until the app stops. Called in
#' `server()`, the pool is released when the last session ends and preloaded
#' again when the next one arrives. One expression per pool: a later call with
#' a different `everywhere` warns and keeps the first. The lazily started
#' default pool has no preload.
#'
#' Resident data is static: if the session reloads `sales`, the workers keep
#' their copy. Data that changes belongs in a reactive read, which is sent per
#' run. Every worker holds its own copy, so four workers and a 2 GB frame need
#' 8 GB, and each worker opens its own connection, so three processes of four
#' workers hold twelve. To close what the expression opened, assign a `.Last`
#' function in it, as above: R runs `.Last` when the worker process exits,
#' which happens when the pool shuts down and also when the app process dies,
#' since a worker that loses its connection to the app exits on its own. The
#' pool does not wait for it, so a slow disconnect never delays shutdown. It
#' does not run if the worker process itself is killed by a signal. `.Last` is
#' an assigned name like any other, so `print()` lists it as resident. A daemon
#' that connects after the preload ran, as a remote daemon may, does not
#' receive it; a task that uses a resident name on such a worker fails with a
#' message saying so.
#'
#' @param workers Number of local daemons. Defaults to one less than the
#'   number of cores available to the process: the machine's cores, capped by
#'   a CPU quota or cpuset on its cgroup, as in a container or on Posit
#'   Connect.
#' @param ... Passed to [mirai::daemons()], e.g. `url` or `remote` for remote
#'   pools. A dispatcher is always used, because cancellation requires one.
#' @param everywhere An expression to evaluate once on every worker when the
#'   pool starts; see the section on preloading. Unevaluated, like the
#'   expression of [reactiveAsync()].
#'
#' @return `TRUE` invisibly if this call started or joined a shinyasync pool,
#'   `FALSE` if daemons were already managed elsewhere.
#' @export
pool_async <- function(workers = NULL, ..., everywhere = NULL) {
  preload <- new_preload(substitute(everywhere), parent.frame())
  # Daemons may have been torn down externally (e.g. mirai::daemons(0)) since
  # this process started a pool; a pool that no longer exists is not owned.
  # Holders of the old pool keep their reference, so the new pool outlives
  # them all.
  if (pool_state$owned && !mirai::daemons_set()) pool_state$owned <- FALSE
  if (!pool_state$owned) {
    if (mirai::daemons_set()) {
      message("shinyasync: mirai daemons are already set; using them as they are.")
      if (set_preload(preload)) run_preload()
      return(invisible(FALSE))
    }
    args <- list(...)
    if (!is.null(workers) || is.null(args$url)) {
      args$n <- workers %||% default_workers()
    }
    args$dispatcher <- TRUE
    do.call(mirai::daemons, args)
    pool_state$owned <- TRUE
    set_preload(preload)
    # Local daemons are all launched by now; wait for every one of them so no
    # task runs on a worker that has not been preloaded.
    run_preload(min = args$n %||% 1L)
  } else if (set_preload(preload)) {
    run_preload()
  }
  pool_state$refs <- pool_state$refs + 1L

  # Mark the holder so the first task in this session (ensure_pool()) does
  # not take a second reference.
  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session)) {
    pool_state$shinyasync_holds_pool <- TRUE
    shiny::onStop(release_pool)
  } else {
    session$userData$shinyasync_holds_pool <- TRUE
    session$onSessionEnded(release_pool)
  }
  invisible(TRUE)
}

release_pool <- function() {
  pool_state$refs <- max(0L, pool_state$refs - 1L)
  if (pool_state$refs == 0L && pool_state$owned) {
    mirai::daemons(0)
    pool_state$owned <- FALSE
    pool_state$shinyasync_holds_pool <- FALSE
  }
  invisible()
}

# Hold the shinyasync pool for the session a task runs in, starting it if no
# daemons are set, as a pool_async() call in that session's server function
# would. Each session holds once, whether through pool_async() or here; a
# module's session proxy shares its root's userData, so a task dispatched
# inside moduleServer() counts for the whole session. Outside a session the
# process holds once, released at app stop.
ensure_pool <- function(compute = NULL) {
  # A named compute profile was set up by the user; the default pool is only
  # for tasks on the default profile.
  if (!is.null(compute) && !identical(compute, "default")) return(invisible())
  set <- mirai::daemons_set()
  if (set && !pool_state$owned) return(invisible())  # user-managed daemons
  session <- shiny::getDefaultReactiveDomain()
  holder <- if (is.null(session)) pool_state else session$userData
  if (set && isTRUE(holder$shinyasync_holds_pool)) return(invisible())
  holder$shinyasync_holds_pool <- TRUE
  if (!set && !pool_state$announced) {
    pool_state$announced <- TRUE
    message(
      "shinyasync: starting a pool of ", default_workers(), " local daemons. ",
      "Call pool_async() in your server function to size it."
    )
  }
  pool_async()
}

# One less than the cores available to the process, leaving one for the
# session; see available_cores() for what limits them.
default_workers <- function() max(1L, available_cores() - 1L)

# Preloading ---------------------------------------------------------------

# Resolve an `everywhere` expression in `env` the way a task's expression is:
# the objects it refers to are sent along, the packages it uses are attached.
# `assigned` are the names it binds, copied into each worker's global
# environment after it runs; `residents` are those plus the sent objects,
# which tasks need not ship.
new_preload <- function(expr, env) {
  if (is.null(expr)) return(NULL)
  deps <- collect_globals(expr, env)
  assigned <- local_names(expr)
  list(
    expr = expr,
    globals = deps$values,
    pkgs = deps$pkgs,
    assigned = assigned,
    residents = unique(c(assigned, names(deps$values)))
  )
}

# Record `preload` as the pool's. TRUE if it is new and should run now, FALSE
# if there is nothing to do: no preload given, or the same one again, as a
# pool_async() call in every server() gives. A different one is refused, since
# the workers already hold the first.
set_preload <- function(preload) {
  if (is.null(preload)) return(FALSE)
  stored <- pool_state$preload
  if (is.null(stored)) {
    pool_state$preload <- preload
    return(TRUE)
  }
  if (!identical(stored$expr, preload$expr)) {
    warning(
      "pool_async(): a different `everywhere` expression is already loaded on the pool; ",
      "keeping the first.",
      call. = FALSE
    )
  }
  FALSE
}

# Evaluate the stored preload on every connected daemon, waiting for `min` of
# them. everywhere() evaluates in a fresh environment, so the names the
# expression assigns are copied into the global environment, where the sent
# objects already are and where tasks look. It returns at once; a failure is
# reported when it arrives, since a task would otherwise see only "object not
# found".
run_preload <- function(min = 1L) {
  p <- pool_state$preload
  if (is.null(p)) return(invisible())
  expr <- bquote({
    .(worker_expr(p$expr, p$pkgs))
    for (.sa_nm in .(p$assigned)) {
      if (exists(.sa_nm, envir = environment(), inherits = FALSE)) {
        assign(.sa_nm, get(.sa_nm, envir = environment()), envir = globalenv())
      }
    }
    invisible()
  })
  ms <- mirai::everywhere(expr, list2env(p$globals), .min = as.integer(min))
  for (m in ms) {
    promises::then(m, onRejected = function(e) {
      message("shinyasync: the pool preload failed on a worker: ", conditionMessage(e))
    })
  }
  invisible()
}

pool_residents <- function() pool_state$preload$residents %||% character()
