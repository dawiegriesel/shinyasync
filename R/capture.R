# Dependency capture -------------------------------------------------------
#
# An async expression is split in two. Reactive reads (`input$x`, `data()`,
# `rv[["y"]]`) and session-only calls (`req()`, `validate()`,
# `invalidateLater()`, `isolate()`) are hoisted out and evaluated on the
# session thread, where they register dependencies as usual. Each is replaced
# by a placeholder symbol, and the rewritten expression is shipped to a worker
# together with the placeholder values and any user-defined globals it needs.

session_fns <- c("req", "validate", "invalidateLater", "isolate")
# Reactive reads of async objects; hoisted like reactive reads, but not part
# of a result's provenance since they change without the inputs changing.
async_fns <- c("status_async", "has_value_async")

new_capture <- function(expr, env, globals = NULL) {
  if (!is.null(globals) && (!is.list(globals) || is.null(names(globals)) ||
                            any(names(globals) == ""))) {
    stop("`globals` must be a named list.", call. = FALSE)
  }
  analysis <- NULL
  # Free names of helper closures, so findGlobals() runs once per helper
  # rather than on every invalidation.
  fn_cache <- new.env(parent = emptyenv())
  # Unresolved functions already warned about, so each warns once per capture.
  warned <- character()

  # Analysed on first use, so reactives defined later in server() resolve.
  analyze <- function() {
    if (is.null(analysis)) analysis <<- analyze_expr(expr, env)
  }

  # The reactive reads a run depends on, evaluated now: a named list keyed by
  # the deparsed read. Stored with each result so is_stale() can compare.
  # `args` are the values of the hoisted calls, if they have been evaluated
  # already; only reads nested inside a hoisted call are evaluated here.
  inputs <- function(args = NULL) {
    analyze()
    out <- lapply(names(analysis$inputs), function(key) {
      i <- match(key, analysis$keys)
      if (!is.null(args) && !is.na(i)) args[[i]] else eval(analysis$inputs[[key]], env)
    })
    names(out) <- names(analysis$inputs)
    out
  }

  # The reactiveAsync() objects among the tracked reads: the upstreams a run
  # waits for, and the links staleness follows.
  upstream <- function() {
    analyze()
    out <- list()
    for (x in analysis$inputs) {
      if (!is.call(x) || length(x) != 1L) next
      obj <- async_arg_object(x[[1]], env, character())
      if (inherits(obj, "reactiveAsync")) out[[length(out) + 1L]] <- obj
    }
    out
  }

  # This capture, for upstream_stale(); the list returned below is not built
  # yet while resolve() is defined.
  self <- environment()

  resolve <- function() {
    analyze()
    # Wait for upstream async reactives to catch up with their own inputs, so
    # a chain is self-consistent as a chain of reactive() expressions is. A
    # halt while an upstream is stale, whether from the upstream read itself
    # or a req() around it, is a waiting halt; the task reports "running"
    # while the upstream runs and the trigger re-runs when it settles.
    args <- lapply(seq_along(analysis$hoisted), function(i) {
      tryCatch(
        eval(analysis$hoisted[[i]], env),
        shiny.silent.error = function(e) {
          if (analysis$reads_upstream[[i]] && upstream_stale(self)) waiting_halt()
          stop(e)
        }
      )
    })
    names(args) <- analysis$placeholders
    if (upstream_stale(self)) waiting_halt()
    skip <- c(analysis$placeholders, names(globals))
    deps <- collect_globals(
      setdiff(analysis$free, skip), env,
      soft = setdiff(analysis$soft, skip), cache = fn_cache,
      resident = pool_residents(), calls = analysis$calls
    )
    # The worker would fail with "could not find function" after the task
    # has run; say so now. A warning, not an error: user-managed daemons may
    # define the name with everywhere().
    for (name in setdiff(deps$missing, warned)) {
      warned <<- c(warned, name)
      warning(missing_fn_message(name), call. = FALSE)
    }
    list(
      expr = worker_expr(analysis$expr, deps$pkgs, deps$resident),
      args = args,
      globals = c(deps$values, globals),
      inputs = inputs(args)
    )
  }

  # What a run would hoist and ship, by name, for print(). Nothing is
  # evaluated, so it needs no reactive context, and the analysis is not
  # cached, so printing before a reactive the expression reads is defined
  # cannot fix a wrong classification into the first real run. Analysis and
  # resolution errors are returned rather than raised: they are what the
  # caller is trying to diagnose.
  describe <- function() {
    a <- tryCatch(analysis %||% analyze_expr(expr, env), error = identity)
    if (inherits(a, "error")) {
      return(list(error = conditionMessage(a)))
    }
    skip <- c(a$placeholders, names(globals))
    deps <- tryCatch(
      collect_globals(
        setdiff(a$free, skip), env,
        soft = setdiff(a$soft, skip), cache = fn_cache,
        resident = pool_residents(), calls = a$calls
      ),
      error = identity
    )
    if (inherits(deps, "error")) {
      # Keep the residents the expression itself names, so the print still
      # says what the worker will supply while the error is being fixed.
      deps <- list(
        values = list(), pkgs = character(),
        resident = intersect(pool_residents(), c(a$free, a$soft)),
        error = conditionMessage(deps)
      )
    }
    list(
      hoisted = a$keys,
      shipped = deps$values,
      resident = deps$resident,
      missing = deps$missing,
      globals = globals,
      pkgs = deps$pkgs,
      expr = a$expr,
      error = deps$error
    )
  }

  list(resolve = resolve, inputs = inputs, upstream = upstream, describe = describe)
}

# Rewrite `expr` for the worker. Returns the rewritten expression, the hoisted
# calls with their placeholder names, the reactive reads that make up a
# result's provenance (`inputs`), the free names the worker needs (`free`),
# and names that appear in formulas (`soft`), which are shipped only if they
# resolve to a plain object. `calls` are the free names in call position: an
# unresolved one is a function the worker will not find.
analyze_expr <- function(expr, env) {
  hoisted <- list()
  keys <- character()
  inputs <- structure(list(), names = character())
  roots <- character()
  formula_names <- character()

  hoist <- function(x, isolated) {
    if (isolated) x <- as.call(list(quote(shiny::isolate), x))
    key <- paste(deparse(x), collapse = "\n")
    i <- match(key, keys)
    if (is.na(i)) {
      keys <<- c(keys, key)
      hoisted[[length(keys)]] <<- x
      i <- length(keys)
    }
    as.name(paste0(".sa_", i))
  }

  # Record the reactive reads in a hoisted call as the provenance of a result:
  # the call itself, or the reads inside `req(input$n)`, skipping anything
  # under isolate() and the status reads of async objects.
  track_reads <- function(x) {
    if (!is.call(x)) return(invisible())
    head <- x[[1]]
    if (identical(head, quote(quote)) || identical(head, quote(`function`))) {
      return(invisible())
    }
    kind <- hoist_kind(x, env, character())
    if (identical(kind, "read")) {
      key <- paste(deparse(x), collapse = "\n")
      if (!key %in% names(inputs)) inputs[[key]] <<- x
      return(invisible())
    }
    if (identical(kind, "isolate") || identical(kind, "async")) return(invisible())
    for (j in seq_along(x)[-1]) track_reads(x[[j]])
    invisible()
  }

  walk_args <- function(x, idx, locals, conditional, isolated) {
    for (j in idx) {
      # Only calls can be rewritten; leave symbols and missing args alone.
      if (is.call(x[[j]])) x[[j]] <- walk(x[[j]], locals, conditional, isolated)
    }
    x
  }

  # `locals`: names bound inside the expression before this point, which
  # shadow anything in `env`. `conditional`: the call may not run, so session-side
  # req()/validate() would halt too eagerly. `isolated`: inside isolate().
  walk <- function(x, locals, conditional = FALSE, isolated = FALSE) {
    if (!is.call(x)) return(x)
    head <- x[[1]]
    n <- length(x)
    rest <- seq_len(n)[-1]

    if (identical(head, quote(quote))) return(x)

    # A block binds names in order: `data <- data()` reads the reactive and
    # only then shadows it, so a statement sees the locals of those before it.
    if (identical(head, quote(`{`))) {
      for (j in rest) {
        if (is.call(x[[j]])) x[[j]] <- walk(x[[j]], locals, conditional, isolated)
        locals <- c(locals, local_names(x[[j]]))
      }
      return(x)
    }

    if (identical(head, quote(`function`)) && n >= 3L) {
      params <- names(x[[2]])
      # Only a call can be rewritten, and assigning NULL into a call deletes
      # the element: `function(e) NULL` would take its srcref as its body.
      if (is.call(x[[3]])) x[[3]] <- walk(x[[3]], c(locals, params), TRUE, isolated)
      return(x)
    }

    if (is_assign(head) && n == 3L && is.call(x[[2]])) {
      root <- assign_root(x[[2]])
      if (!is.null(root) && !root %in% locals) {
        hit <- lookup(root, env)
        if (hit$found && shiny::is.reactivevalues(hit$value)) {
          stop(
            sprintf("`%s` assigns to reactive values inside an async expression.\n",
                    paste(deparse(x), collapse = " ")),
            "The expression runs in a worker; assign the result in a separate observe() instead.",
            call. = FALSE
          )
        }
        # `df$col <- v` reads `df` before making it local; it must be shipped.
        roots <<- c(roots, root)
      }
      return(walk_args(x, rest, locals, conditional, isolated))
    }

    if (identical(head, quote(`~`))) {
      x <- walk_args(x, rest, locals, conditional, isolated)
      formula_names <<- c(formula_names, setdiff(all.names(x), c("~", locals)))
      return(x)
    }

    kind <- hoist_kind(x, env, locals)
    if (identical(kind, "read") || identical(kind, "async")) return(hoist(x, isolated))

    if (!is.null(kind)) {
      uses_locals <- any(free_names(x) %in% locals)
      if (kind == "isolate") {
        if (!uses_locals) return(hoist(x, FALSE))
        # Reads inside are isolated on the session; the call itself is a
        # no-op in the worker.
        inner <- walk_args(x, rest, locals, conditional, TRUE)
        return(as.call(list(quote(`(`), inner[[2]])))
      }
      # req()/validate() that may not run, or that use worker-local values,
      # stay in the worker, where their silent errors are handled.
      if (!uses_locals && (!conditional || kind == "invalidateLater")) {
        return(hoist(x, isolated))
      }
      return(walk_args(x, rest, locals, conditional, isolated))
    }

    if (identical(head, quote(`if`))) {
      x <- walk_args(x, 2L, locals, conditional, isolated)
      return(walk_args(x, rest[-1], locals, TRUE, isolated))
    }
    if (identical(head, quote(`&&`)) || identical(head, quote(`||`))) {
      x <- walk_args(x, 2L, locals, conditional, isolated)
      return(walk_args(x, rest[-1], locals, TRUE, isolated))
    }
    if (identical(head, quote(switch)) && n >= 2L) {
      x <- walk_args(x, 2L, locals, conditional, isolated)
      return(walk_args(x, rest[-1], locals, TRUE, isolated))
    }
    if (identical(head, quote(`for`)) && n == 4L) {
      x <- walk_args(x, 3L, locals, conditional, isolated)
      if (is.symbol(x[[2]])) locals <- c(locals, as.character(x[[2]]))
      return(walk_args(x, 4L, locals, TRUE, isolated))
    }
    if (identical(head, quote(`while`)) && n == 3L) {
      x <- walk_args(x, 2L, locals, conditional, isolated)
      return(walk_args(x, 3L, locals, TRUE, isolated))
    }
    if (identical(head, quote(`repeat`))) {
      return(walk_args(x, rest, locals, TRUE, isolated))
    }

    walk_args(x, rest, locals, conditional, isolated)
  }

  rewritten <- walk(expr, character())
  # Hoisted calls never use expression locals, so no shadowing to respect here.
  for (x in hoisted) track_reads(x)
  placeholders <- if (length(keys)) paste0(".sa_", seq_along(keys)) else character()
  names <- free_names(rewritten, split = TRUE)
  free <- setdiff(unique(c(names$all, roots)), placeholders)
  list(
    expr = rewritten,
    hoisted = hoisted,
    placeholders = placeholders,
    keys = keys,
    inputs = inputs,
    free = free,
    calls = setdiff(names$functions, placeholders),
    soft = setdiff(unique(formula_names), c(free, placeholders)),
    reads_upstream = vapply(hoisted, reads_upstream, logical(1), env = env)
  )
}

# Does a hoisted call read a reactiveAsync(), directly or inside req() and the
# like? A halt in such a call while that upstream is stale is a wait, not a
# req() failure.
reads_upstream <- function(x, env) {
  if (!is.call(x)) return(FALSE)
  if (length(x) == 1L &&
      inherits(async_arg_object(x[[1]], env, character()), "reactiveAsync")) {
    return(TRUE)
  }
  for (j in seq_along(x)[-1]) if (reads_upstream(x[[j]], env)) return(TRUE)
  FALSE
}

is_assign <- function(head) {
  identical(head, quote(`<-`)) || identical(head, quote(`=`)) || identical(head, quote(`<<-`))
}

# The variable a complex assignment such as `x$a[[i]] <- v` modifies.
assign_root <- function(lhs) {
  while (is.call(lhs)) {
    if (length(lhs) < 2L) return(NULL)
    lhs <- lhs[[2]]
  }
  if (is.symbol(lhs)) as.character(lhs) else NULL
}

# Names bound by plain assignment or `for` in `x`, not descending into nested
# function definitions, which have their own scope. Unlike
# codetools::findFuncLocals(), `rv$y <- v` does not make `rv` local.
local_names <- function(x) {
  out <- character()
  visit <- function(x) {
    if (!is.call(x)) return()
    head <- x[[1]]
    if (identical(head, quote(`function`)) || identical(head, quote(quote))) return()
    if (is_assign(head) && !identical(head, quote(`<<-`)) && length(x) == 3L &&
        is.symbol(x[[2]])) {
      out <<- c(out, as.character(x[[2]]))
    }
    if (identical(head, quote(`for`)) && length(x) == 4L && is.symbol(x[[2]])) {
      out <<- c(out, as.character(x[[2]]))
    }
    for (j in seq_along(x)[-1]) visit(x[[j]])
  }
  visit(x)
  unique(out)
}

# "read" for a reactive read, "async" for status_async() and friends on an
# async object, the function name for a session-only shiny call, NULL
# otherwise.
hoist_kind <- function(x, env, locals) {
  head <- x[[1]]

  # input$name, rv[["name"]]
  if ((identical(head, quote(`$`)) || identical(head, quote(`[[`))) && length(x) == 3L) {
    obj <- x[[2]]
    key <- x[[3]]
    literal <- is.character(key) || (identical(head, quote(`$`)) && is.symbol(key))
    if (is.symbol(obj) && literal && !as.character(obj) %in% locals) {
      hit <- lookup(as.character(obj), env)
      if (hit$found && shiny::is.reactivevalues(hit$value)) return("read")
    }
    return(NULL)
  }

  # reactive()
  if (is.symbol(head) && length(x) == 1L && !as.character(head) %in% locals) {
    hit <- lookup(as.character(head), env, mode = "function")
    if (hit$found && shiny::is.reactive(hit$value)) return("read")
  }

  # mod$wind(), mod[["wind"]](): a reactive in a module's returned list
  if (is.call(head) && length(x) == 1L &&
      shiny::is.reactive(async_arg_object(head, env, locals))) {
    return("read")
  }

  # status_async(r), has_value_async(r), also on `mod$r`
  if (length(x) == 2L && !is.null(async_fn_name(head, env, locals))) {
    obj <- async_arg_object(x[[2]], env, locals)
    if (is.environment(attr(obj, "shinyasync", exact = TRUE))) return("async")
  }

  shiny_fn_name(head, env, locals)
}

# The object a status_async()-style call, or the head of a `mod$wind()` read,
# refers to: a symbol, or a literal `$`/`[[` field of a plain list or
# environment such as a module's return value. NULL when it cannot be
# resolved without a reactive context.
async_arg_object <- function(arg, env, locals) {
  if (is.symbol(arg)) {
    if (as.character(arg) %in% locals) return(NULL)
    hit <- lookup(as.character(arg), env)
    return(if (hit$found) hit$value)
  }
  if (!is.call(arg) || length(arg) != 3L) return(NULL)
  head <- arg[[1]]
  if (!identical(head, quote(`$`)) && !identical(head, quote(`[[`))) return(NULL)
  key <- arg[[3]]
  literal <- is.character(key) || (identical(head, quote(`$`)) && is.symbol(key))
  if (!literal) return(NULL)
  root <- async_arg_object(arg[[2]], env, locals)
  if (is.null(root) || is_reactive_like(root) || !(is.list(root) || is.environment(root))) {
    return(NULL)
  }
  root[[as.character(key)]]
}

async_fn_name <- function(head, env, locals = character()) {
  exported_fn_name(head, "shinyasync", async_fns, env, locals)
}

shiny_fn_name <- function(head, env, locals = character()) {
  exported_fn_name(head, "shiny", session_fns, env, locals)
}

# The name in `fns` that the head of a call refers to, if it is one of
# `pkg`'s exports: written as `pkg::fn`, as a bare `fn` bound to that export,
# or as a field of a list or environment holding it, `mod$fn` or
# `mod[["fn"]]`, which is how box::use(shiny) binds them. NULL otherwise.
exported_fn_name <- function(head, pkg, fns, env, locals) {
  if (is.call(head) && identical(head[[1]], quote(`::`))) {
    if (identical(head[[2]], as.name(pkg)) && as.character(head[[3]]) %in% fns) {
      return(as.character(head[[3]]))
    }
    return(NULL)
  }
  name <- if (is.symbol(head)) {
    as.character(head)
  } else if (is.call(head) && length(head) == 3L) {
    key <- head[[3]]
    if (is.character(key) || is.symbol(key)) as.character(key)
  }
  if (is.null(name) || !name %in% fns) return(NULL)
  obj <- async_arg_object(head, env, locals)
  if (is.function(obj) && identical(obj, getExportedValue(pkg, name))) name
}

# Globals ------------------------------------------------------------------

# Resolve `names` from `env` into values to ship to the worker and packages to
# attach there. Helper closures are rebound to a copy of their defining
# environment that holds only what they use, so closures from different scopes
# keep their own bindings and nothing drags the session along.
# `resident` names, made resident on every worker by pool_async(everywhere = ),
# are not shipped, whether the expression uses them directly or a shipped
# helper does; the ones met are returned so a run can check for them first.
# `calls` are the names used as functions; those the session cannot resolve
# are returned as `missing`.
collect_globals <- function(names, env, soft = character(), cache = NULL,
                            resident = character(), calls = NULL) {
  if (!is.character(names)) {
    split <- free_names(names, split = TRUE)
    names <- split$all
    calls <- calls %||% split$functions
  }
  values <- list()
  pkgs <- character()
  clones <- list()
  met <- character()
  missing <- character()

  clone_for <- function(src) {
    for (entry in clones) if (identical(entry$src, src)) return(entry$clone)
    clone <- new.env(parent = globalenv())
    clones[[length(clones) + 1L]] <<- list(src = src, clone = clone)
    clone
  }

  classify <- function(name, from, soft = FALSE, call = name %in% calls) {
    if (name %in% resident) {
      met <<- c(met, name)
      return(NULL)
    }
    hit <- lookup(name, from)
    if (!hit$found) {
      if (call) missing <<- c(missing, name)
      return(NULL)
    }
    where <- hit$env

    if (identical(where, baseenv()) || identical(where, .BaseNamespaceEnv)) return(NULL)
    env_name <- environmentName(where)
    if (startsWith(env_name, "package:")) {
      pkgs <<- c(pkgs, sub("^package:", "", env_name))
      return(NULL)
    }
    if (!identical(where, globalenv()) && !isNamespace(where) &&
        !startsWith(env_name, "imports:") && env_name != "" && !is_box_env(where)) {
      return(NULL)  # Autoloads, tools:rstudio and similar
    }

    value <- hit$value
    if (is_reactive_like(value)) {
      if (soft) return(NULL)
      stop(
        sprintf(
          "`%s` is a reactive object that shinyasync could not resolve before sending the task to a worker.\n",
          name
        ),
        "Read it into a variable in a separate reactive or pass the value with `globals = list(...)`.",
        call. = FALSE
      )
    }
    if (is.function(value) && !is.primitive(value) && !in_namespace(value)) {
      value <- ship_closure(value, name)
    }
    list(value)
  }

  ship_closure <- function(f, name) {
    src <- environment(f)
    clone <- clone_for(src)
    if (exists(name, envir = clone, inherits = FALSE)) {
      return(get(name, envir = clone, inherits = FALSE))
    }
    fnames <- closure_free_names(f, name, cache)
    environment(f) <- clone
    # Bind before resolving dependencies so recursive helpers terminate.
    assign(name, f, envir = clone)
    for (nm in fnames$all) {
      if (exists(nm, envir = clone, inherits = FALSE)) next
      r <- classify(nm, src, call = nm %in% fnames$functions)
      if (!is.null(r)) assign(nm, r[[1]], envir = clone)
    }
    f
  }

  for (name in names) {
    r <- classify(name, env)
    if (!is.null(r)) values[name] <- r
  }
  for (name in soft) {
    r <- classify(name, env, soft = TRUE)
    if (!is.null(r)) values[name] <- r
  }

  list(
    values = values,
    pkgs = setdiff(unique(pkgs), "base"),
    resident = resident[resident %in% met],
    missing = unique(missing)
  )
}

closure_free_names <- function(f, name, cache) {
  if (is.null(cache)) return(free_names(f, split = TRUE))
  hit <- cache[[name]]
  if (!is.null(hit) && identical(hit$fn, f)) return(hit$names)
  names <- free_names(f, split = TRUE)
  cache[[name]] <- list(fn = f, names = names)
  names
}

# The free names of a function or expression; with `split`, also the subset
# used in call position.
free_names <- function(x, split = FALSE) {
  if (!is.function(x)) {
    f <- function() NULL
    body(f) <- x
    x <- f
  }
  if (!split) return(codetools::findGlobals(x, merge = TRUE))
  g <- codetools::findGlobals(x, merge = FALSE)
  list(all = unique(c(g$functions, g$variables)), functions = g$functions)
}

missing_fn_message <- function(name) {
  sprintf(
    "`%s()` is not defined in the session, so it cannot be sent to the worker. Attach its package with library() in the session, or call it as pkg::%s().",
    name, name
  )
}

# The expression a worker evaluates: `expr` after attaching `pkgs`. Names the
# pool's preload made resident (see pool_async()) are checked first, so a
# daemon that joined after the preload fails with the cause rather than with
# "object not found".
worker_expr <- function(expr, pkgs, resident = character()) {
  if (!length(pkgs) && !length(resident)) return(call("{", expr))
  parts <- list()
  if (length(resident)) {
    parts[[length(parts) + 1L]] <- bquote(
      for (.sa_nm in .(resident)) {
        if (!exists(.sa_nm, envir = globalenv())) {
          stop(sprintf(
            "`%s` was not preloaded on this worker. A daemon that connects after pool_async() ran does not receive its `everywhere` expression.",
            .sa_nm
          ), call. = FALSE)
        }
      }
    )
  }
  if (length(pkgs)) {
    parts[[length(parts) + 1L]] <- bquote(
      for (.sa_pkg in .(pkgs)) {
        suppressPackageStartupMessages(library(.sa_pkg, character.only = TRUE))
      }
    )
  }
  as.call(c(quote(`{`), parts, list(expr)))
}

# Helpers ------------------------------------------------------------------

lookup <- function(name, env, mode = "any") {
  while (!identical(env, emptyenv())) {
    if (exists(name, envir = env, mode = mode, inherits = FALSE)) {
      return(list(found = TRUE, value = get(name, envir = env, mode = mode), env = env))
    }
    env <- parent.env(env)
  }
  list(found = FALSE)
}

# A box module's own environment ("namespace:<mod>") or the one a top-level
# box::use() attaches ("mod:<path>"). Neither is an R namespace a worker could
# find again by name, so their objects are shipped as the global environment's
# are. The marker test is box's own is_namespace().
is_box_env <- function(env) {
  inherits(env, "box$mod") ||
    exists(".__module__.", envir = env, mode = "environment", inherits = FALSE)
}

# Functions shipped as they are rather than rebound to a clone: those of a
# package, and S4 generics, whose environment holds the method tables dispatch
# needs (box binds DBI::dbConnect() in a module's imports like any function).
in_namespace <- function(f) {
  e <- environment(f)
  is.null(e) || isNamespace(e) || identical(e, baseenv()) ||
    (isS4(f) && exists(".Generic", envir = e, inherits = FALSE))
}

is_reactive_like <- function(x) {
  shiny::is.reactive(x) || shiny::is.reactivevalues(x) ||
    inherits(x, c("ShinySession", "session_proxy", "MockShinySession", "shinyoutput"))
}
