# Printing -----------------------------------------------------------------
#
# print() shows what dependency capture resolved: the calls hoisted to the
# session, the objects and packages shipped to the worker, and the rewritten
# worker expression. Nothing is evaluated and the task is not started, so it
# is safe at any point in a server function.

#' @export
print.reactiveAsync <- function(x, ...) print_async(x)

#' @export
print.observeAsync <- function(x, ...) print_async(x)

#' @export
print.reactiveValAsync <- function(x, ...) print_async(x)

print_async <- function(x) {
  print_task(attr(x, "shinyasync", exact = TRUE))
  invisible(x)
}

# In shiny::devmode(), each task prints itself on its first run, when the
# analysis is final. The app's own code is on screen then, so the worker
# expression is left out. options(shinyasync.print = FALSE) turns it off.
devmode_print <- function(task) {
  if (isTRUE(task$printed)) return(invisible())
  task$printed <- TRUE
  on <- shiny::get_devmode_option("shinyasync.print", default = FALSE)
  if (isTRUE(on)) print_task(task, expr = FALSE)
  invisible()
}

# mirai_async() has no task to hold a printed flag, and a fan-out calls it
# once per item, so each call site prints once per session.
print_state <- new.env(parent = emptyenv())

devmode_print_call <- function(label, capture) {
  on <- shiny::get_devmode_option("shinyasync.print", default = FALSE)
  if (!isTRUE(on)) return(invisible())
  session <- shiny::getDefaultReactiveDomain()
  holder <- if (is.null(session)) print_state else session$userData
  if (label %in% holder$shinyasync_printed) return(invisible())
  holder$shinyasync_printed <- c(holder$shinyasync_printed, label)
  task <- list(
    kind = "mirai_async", label = label, capture = capture,
    status = function() "running"
  )
  print_task(task, expr = FALSE)
}

print_task <- function(task, expr = TRUE) {
  header <- cli::format_inline("{.cls {task$kind}} {task$label}")
  header <- cli::ansi_strtrim(header, cli::console_width() - 6L)
  cli::cat_line(cli::rule(left = header))
  print_field("status", style_status(shiny::isolate(task$current_status())))
  if (is.null(task$capture)) return(invisible())

  d <- task$capture$describe()
  if (!is.null(d$error)) {
    print_field(cli::col_red(cli::symbol$cross), d$error)
  }
  if (is.null(d$hoisted)) return(invisible())

  # In the full form the placeholders name the reads, so the worker
  # expression below can be read against them.
  session <- if (expr && length(d$hoisted)) {
    paste(cli::col_grey(sprintf(".sa_%d", seq_along(d$hoisted))), d$hoisted)
  } else {
    d$hoisted
  }
  print_field("session", session)
  print_field("shipped", c(
    vapply(names(d$shipped), function(nm) describe_object(nm, d$shipped[[nm]]), ""),
    vapply(names(d$globals), function(nm) {
      paste(describe_object(nm, d$globals[[nm]]), cli::col_grey("(globals)"))
    }, "")
  ))
  if (length(d$resident)) print_field("resident", d$resident)
  if (length(d$pkgs)) print_field("attached", d$pkgs)
  if (length(d$missing)) {
    print_field("missing", c(
      cli::col_red(paste0(d$missing, "()")),
      cli::col_grey("not defined in the session: attach the package or use pkg::fn()")
    ))
  }

  if (expr) {
    # The statements of a `{` block, one per line, without the braces.
    e <- d$expr
    parts <- if (is.call(e) && identical(e[[1]], quote(`{`))) as.list(e)[-1] else list(e)
    lines <- unlist(lapply(parts, deparse, width.cutoff = 70L))
    print_field("worker", lines, sep = "\n")
  }
  invisible()
}

# One aligned line per field, its values comma-separated and wrapped under
# the value column.
print_field <- function(name, values, sep = ", ") {
  key <- cli::style_bold(formatC(name, width = 8L, flag = "-"))
  if (!length(values)) {
    cli::cat_line("  ", key, " ", cli::col_grey("none"))
    return(invisible())
  }
  if (identical(sep, "\n")) {
    cli::cat_line("  ", key, " ", values[1])
    for (v in values[-1]) cli::cat_line(strrep(" ", 11L), v)
    return(invisible())
  }
  text <- paste(values, collapse = sep)
  width <- max(40L, cli::console_width() - 11L)
  lines <- cli::ansi_strwrap(text, width = width)
  cli::cat_line("  ", key, " ", lines[1])
  for (l in lines[-1]) cli::cat_line(strrep(" ", 11L), l)
  invisible()
}

describe_object <- function(name, x) {
  if (is.function(x)) return(paste0(name, "()"))
  cli::format_inline("{name} {.cls {class(x)[[1]]}}")
}

style_status <- function(status) {
  switch(
    status,
    running = cli::col_yellow(status),
    success = cli::col_green(status),
    error = cli::col_red(status),
    cli::col_grey(status)
  )
}
