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
async_fns <- c("status_async", "stale_async", "inputs_async")

new_capture <- function(expr, env, globals = NULL) {
  if (!is.null(globals) && (!is.list(globals) || is.null(names(globals)) ||
                            any(names(globals) == ""))) {
    stop("`globals` must be a named list.", call. = FALSE)
  }
  analysis <- NULL
  # Free names of helper closures, so findGlobals() runs once per helper
  # rather than on every invalidation.
  fn_cache <- new.env(parent = emptyenv())

  # Analysed on first use, so reactives defined later in server() resolve.
  analyze <- function() {
    if (is.null(analysis)) analysis <<- analyze_expr(expr, env)
  }

  # The reactive reads a run depends on, evaluated now: a named list keyed by
  # the deparsed read. Stored with each result so stale_async() can compare.
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

  # The reactiveAsync() objects among the tracked reads, so staleness can
  # follow a chain.
  upstream <- function() {
    analyze()
    out <- list()
    for (x in analysis$inputs) {
      if (!is.call(x) || length(x) != 1L || !is.symbol(x[[1]])) next
      hit <- lookup(as.character(x[[1]]), env, mode = "function")
      if (hit$found && inherits(hit$value, "reactiveAsync")) out[[length(out) + 1L]] <- hit$value
    }
    out
  }

  resolve <- function() {
    analyze()
    args <- lapply(analysis$hoisted, eval, envir = env)
    names(args) <- analysis$placeholders
    skip <- c(analysis$placeholders, names(globals))
    deps <- collect_globals(
      setdiff(analysis$free, skip), env,
      soft = setdiff(analysis$soft, skip), cache = fn_cache
    )
    list(
      expr = worker_expr(analysis$expr, deps$pkgs),
      args = args,
      globals = c(deps$values, globals),
      inputs = inputs(args)
    )
  }

  list(resolve = resolve, inputs = inputs, upstream = upstream)
}

# Rewrite `expr` for the worker. Returns the rewritten expression, the hoisted
# calls with their placeholder names, the reactive reads that make up a
# result's provenance (`inputs`), the free names the worker needs (`free`),
# and names that appear in formulas (`soft`), which are shipped only if they
# resolve to a plain object.
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

  # `locals`: names bound inside the expression at this point, which shadow
  # anything in `env`. `conditional`: the call may not run, so session-side
  # req()/validate() would halt too eagerly. `isolated`: inside isolate().
  walk <- function(x, locals, conditional = FALSE, isolated = FALSE) {
    if (!is.call(x)) return(x)
    head <- x[[1]]
    n <- length(x)
    rest <- seq_len(n)[-1]

    if (identical(head, quote(quote))) return(x)

    if (identical(head, quote(`function`)) && n >= 3L) {
      params <- names(x[[2]])
      x[[3]] <- walk(x[[3]], c(locals, params, local_names(x[[3]])), TRUE, isolated)
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

  rewritten <- walk(expr, local_names(expr))
  # Hoisted calls never use expression locals, so no shadowing to respect here.
  for (x in hoisted) track_reads(x)
  placeholders <- if (length(keys)) paste0(".sa_", seq_along(keys)) else character()
  free <- setdiff(unique(c(free_names(rewritten), roots)), placeholders)
  list(
    expr = rewritten,
    hoisted = hoisted,
    placeholders = placeholders,
    keys = keys,
    inputs = inputs,
    free = free,
    soft = setdiff(unique(formula_names), c(free, placeholders))
  )
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

  # status_async(r), stale_async(r), inputs_async(r), also on `mod$r`
  if (length(x) == 2L && !is.null(async_fn_name(head, env, locals))) {
    obj <- async_arg_object(x[[2]], env, locals)
    if (is.environment(attr(obj, "shinyasync", exact = TRUE))) return("async")
  }

  shiny_fn_name(head, env, locals)
}

# The object a status_async()-style call refers to: a symbol, or a literal
# `$`/`[[` field of a plain list or environment such as a module's return
# value. NULL when it cannot be resolved without a reactive context.
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
  if (is.call(head) && identical(head[[1]], quote(`::`)) &&
      identical(head[[2]], quote(shinyasync)) && as.character(head[[3]]) %in% async_fns) {
    return(as.character(head[[3]]))
  }
  if (is.symbol(head) && as.character(head) %in% async_fns &&
      !as.character(head) %in% locals) {
    name <- as.character(head)
    hit <- lookup(name, env, mode = "function")
    if (hit$found && identical(hit$value, getExportedValue("shinyasync", name))) return(name)
  }
  NULL
}

shiny_fn_name <- function(head, env, locals = character()) {
  if (is.call(head) && identical(head[[1]], quote(`::`)) &&
      identical(head[[2]], quote(shiny)) && as.character(head[[3]]) %in% session_fns) {
    return(as.character(head[[3]]))
  }
  if (is.symbol(head) && as.character(head) %in% session_fns &&
      !as.character(head) %in% locals) {
    name <- as.character(head)
    hit <- lookup(name, env, mode = "function")
    if (hit$found && identical(hit$value, getExportedValue("shiny", name))) return(name)
  }
  NULL
}

# Globals ------------------------------------------------------------------

# Resolve `names` from `env` into values to ship to the worker and packages to
# attach there. Helper closures are rebound to a copy of their defining
# environment that holds only what they use, so closures from different scopes
# keep their own bindings and nothing drags the session along.
collect_globals <- function(names, env, soft = character(), cache = NULL) {
  if (!is.character(names)) names <- free_names(names)
  values <- list()
  pkgs <- character()
  clones <- list()

  clone_for <- function(src) {
    for (entry in clones) if (identical(entry$src, src)) return(entry$clone)
    clone <- new.env(parent = globalenv())
    clones[[length(clones) + 1L]] <<- list(src = src, clone = clone)
    clone
  }

  classify <- function(name, from, soft = FALSE) {
    hit <- lookup(name, from)
    if (!hit$found) return(NULL)
    where <- hit$env

    if (identical(where, baseenv()) || identical(where, .BaseNamespaceEnv)) return(NULL)
    env_name <- environmentName(where)
    if (startsWith(env_name, "package:")) {
      pkgs <<- c(pkgs, sub("^package:", "", env_name))
      return(NULL)
    }
    if (!identical(where, globalenv()) && !isNamespace(where) &&
        !startsWith(env_name, "imports:") && env_name != "") {
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
    for (nm in fnames) {
      if (exists(nm, envir = clone, inherits = FALSE)) next
      r <- classify(nm, src)
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

  list(values = values, pkgs = setdiff(unique(pkgs), "base"))
}

closure_free_names <- function(f, name, cache) {
  if (is.null(cache)) return(free_names(f))
  hit <- cache[[name]]
  if (!is.null(hit) && identical(hit$fn, f)) return(hit$names)
  names <- free_names(f)
  cache[[name]] <- list(fn = f, names = names)
  names
}

free_names <- function(x) {
  if (!is.function(x)) {
    f <- function() NULL
    body(f) <- x
    x <- f
  }
  codetools::findGlobals(x, merge = TRUE)
}

worker_expr <- function(expr, pkgs) {
  if (!length(pkgs)) return(call("{", expr))
  bquote({
    for (.sa_pkg in .(pkgs)) {
      suppressPackageStartupMessages(library(.sa_pkg, character.only = TRUE))
    }
    .(expr)
  })
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

in_namespace <- function(f) {
  e <- environment(f)
  is.null(e) || isNamespace(e) || identical(e, baseenv())
}

is_reactive_like <- function(x) {
  shiny::is.reactive(x) || shiny::is.reactivevalues(x) ||
    inherits(x, c("ShinySession", "session_proxy", "MockShinySession", "shinyoutput"))
}
