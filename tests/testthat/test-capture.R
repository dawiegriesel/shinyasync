test_that("input$x, input[['x']] and reactive calls are hoisted", {
  input <- shiny::reactiveValues(n = 5, m = 2)
  data <- shiny::reactive(1:3)
  a <- analyze_expr(quote({
    x <- rnorm(input$n) + input[["m"]]
    sum(data(), input$n)
  }), environment())

  expect_length(a$hoisted, 3)
  expect_equal(a$hoisted[[1]], quote(input$n))
  expect_equal(a$hoisted[[2]], quote(input[["m"]]))
  expect_equal(a$hoisted[[3]], quote(data()))
  expect_false("input" %in% all.names(a$expr))
  expect_true(all(c(".sa_1", ".sa_2", ".sa_3") %in% all.names(a$expr)))
})

test_that("non-reactive calls and quoted code are left alone", {
  input <- list(n = 1)
  f <- function() 1
  a <- analyze_expr(quote({ input$n + f(); quote(f()) }), environment())
  expect_length(a$hoisted, 0)
})

test_that("req(), validate() and invalidateLater() run on the session unless they use locals", {
  input <- shiny::reactiveValues(n = 5)
  req <- shiny::req
  a <- analyze_expr(quote({
    shiny::invalidateLater(1000)
    n <- req(input$n)
    y <- n * 2
    req(y > 0)
    y
  }), environment())

  expect_equal(a$hoisted[[1]], quote(shiny::invalidateLater(1000)))
  expect_equal(a$hoisted[[2]], quote(req(input$n)))
  expect_length(a$hoisted, 2)
  expect_true("req" %in% all.names(a$expr))
})

test_that("resolve() evaluates hoisted reads and ships user globals transitively", {
  input <- shiny::reactiveValues(n = 3)
  helper <- function(x) x + offset
  offset <- 100
  fit <- function(n) helper(n)
  cap <- new_capture(quote(fit(input$n) + extra), environment(), globals = list(extra = 1))
  job <- shiny::isolate(cap$resolve())

  expect_equal(job$args, list(.sa_1 = 3))
  expect_setequal(names(job$globals), c("fit", "extra"))
  fit_env <- environment(job$globals$fit)
  expect_identical(parent.env(fit_env), globalenv())
  expect_setequal(ls(fit_env), c("fit", "helper", "offset"))
  expect_identical(environment(fit_env$helper), fit_env)

  for (nm in names(job$globals)) assign(nm, job$globals[[nm]], envir = globalenv())
  withr::defer(rm(list = names(job$globals), envir = globalenv()))
  expect_equal(eval(job$expr, job$args, globalenv()), 104)
})

test_that("attached packages used by the expression are recorded", {
  env <- new.env(parent = globalenv())
  deps <- collect_globals(quote(stats::sd(x) + median(y)), env)
  expect_true("stats" %in% deps$pkgs)
  expect_length(deps$values, 0)
})

test_that("unresolvable reactive reads give an informative error", {
  input <- shiny::reactiveValues(x1 = 1)
  cap <- new_capture(quote(input[[paste0("x", 1)]]), environment())
  expect_error(shiny::isolate(cap$resolve()), "reactive object")
})

test_that("isolate() reads are evaluated on the session without a dependency", {
  input <- shiny::reactiveValues(n = 5, seed = 1)
  isolate <- shiny::isolate
  a <- analyze_expr(quote({
    k <- 2
    isolate(input$seed) + input$n + isolate(input$n * k)
  }), environment())

  expect_equal(a$hoisted[[1]], quote(isolate(input$seed)))
  expect_equal(a$hoisted[[2]], quote(input$n))
  expect_equal(a$hoisted[[3]], quote(shiny::isolate(input$n)))
  expect_equal(a$expr[[3]], quote(.sa_1 + .sa_2 + (.sa_3 * k)))
})

test_that("req() and validate() inside conditionals stay in the worker", {
  input <- shiny::reactiveValues(a = NULL, b = NULL)
  req <- shiny::req
  a <- analyze_expr(quote(if (is.null(input$a)) 0 else req(input$b)), environment())

  expect_equal(a$hoisted, list(quote(input$a), quote(input$b)))
  expect_equal(a$expr, quote(if (is.null(.sa_1)) 0 else req(.sa_2)))

  a <- analyze_expr(quote({
    req(input$a)
    x <- input$a && shiny::validate(shiny::need(input$b, "b"))
    for (i in 1:2) req(input$b)
  }), environment())
  expect_equal(a$hoisted[[1]], quote(req(input$a)))
  expect_length(a$hoisted, 3)
  expect_true("validate" %in% all.names(a$expr))
  expect_equal(sum(all.names(a$expr) == "req"), 1)
})

test_that("NULL server variables are shipped", {
  env <- new.env(parent = globalenv())
  env$limit <- NULL
  deps <- collect_globals("limit", env)
  expect_true("limit" %in% names(deps$values))
  expect_null(deps$values$limit)
})

test_that("assigning to a server object ships it; assigning to reactive values errors", {
  rv <- shiny::reactiveValues(count = 0)
  df <- data.frame(x = 1)
  a <- analyze_expr(quote({ df$y <- 2; df }), environment())
  expect_true("df" %in% a$free)

  expect_error(
    analyze_expr(quote({ res <- 1; rv$result <- res; rv$count + 1 }), environment()),
    "assigns to reactive values"
  )
})

test_that("lambda parameters shadow reactive objects", {
  input <- shiny::reactiveValues(a = 1)
  a <- analyze_expr(quote(sapply(xs, function(input) input$a)), environment())
  expect_length(a$hoisted, 0)
  a <- analyze_expr(quote(lapply(1:2, function(i) input$a + i)), environment())
  expect_length(a$hoisted, 1)
})

test_that("reactive reads inside formulas are hoisted and formula symbols are soft globals", {
  input <- shiny::reactiveValues(x = 1)
  y <- shiny::reactive(1)
  w <- 1:3
  a <- analyze_expr(quote(lm(y ~ input$x + w, data = d)), environment())
  expect_equal(a$hoisted, list(quote(input$x)))
  expect_equal(a$expr, quote(lm(y ~ .sa_1 + w, data = d)))
  expect_true(all(c("y", "w") %in% a$soft))

  deps <- collect_globals(a$free, environment(), soft = a$soft)
  expect_equal(deps$values$w, 1:3)
  expect_false("y" %in% names(deps$values))
})

test_that("helpers with same-named free variables keep their own bindings", {
  k <- 100
  f1 <- local({ k <- 1; function() k })
  f2 <- local({ k <- 2; function() k })
  rec <- function(n) if (n <= 0) 0 else n + rec(n - 1)
  cap <- new_capture(quote(f1() + f2() + k + rec(3)), environment())
  job <- shiny::isolate(cap$resolve())

  expect_equal(job$globals$k, 100)
  expect_equal(job$globals$f1(), 1)
  expect_equal(job$globals$f2(), 2)
  expect_equal(job$globals$rec(3), 6)
})

test_that("closure free names are cached across runs", {
  helper <- function(x) x + 1
  cap <- new_capture(quote(helper(input$n)), environment())
  input <- shiny::reactiveValues(n = 1)
  shiny::isolate(cap$resolve())
  local_mocked_bindings(free_names = function(x) stop("findGlobals should not run again"))
  expect_silent(job <- shiny::isolate(cap$resolve()))
  expect_equal(job$globals$helper(1), 2)
})

test_that("provenance tracks plain reads and reads inside req(), not isolated or status reads", {
  input <- shiny::reactiveValues(n = 1, m = 2, seed = 3)
  data <- shiny::reactive(1)
  up <- reactiveAsync(1)
  req <- shiny::req
  isolate <- shiny::isolate
  a <- analyze_expr(quote({
    req(input$n, isolate(input$seed))
    k <- isolate(input$m + 1)
    if (status_async(up) == "success") data() else has_value_async(up)
  }), environment())

  expect_equal(names(a$inputs), c("input$n", "data()"))
  expect_equal(a$inputs[["data()"]], quote(data()))
  has_hoisted <- function(call) any(vapply(a$hoisted, identical, logical(1), call))
  expect_true(has_hoisted(quote(status_async(up))))
  expect_true(has_hoisted(quote(has_value_async(up))))
  expect_false("up" %in% a$free)

  cap <- new_capture(quote(input$n + data()), environment())
  expect_equal(shiny::isolate(cap$inputs()), list(`input$n` = 1, `data()` = 1))
  job <- shiny::isolate(cap$resolve())
  expect_equal(job$inputs, list(`input$n` = 1, `data()` = 1))
})

test_that("status_async() on a non-async object or a local is left alone", {
  up <- shiny::reactive(1)
  a <- analyze_expr(quote(status_async(up)), environment())
  expect_length(a$hoisted, 0)
  a <- analyze_expr(quote(sapply(1:2, function(up) status_async(up))), environment())
  expect_length(a$hoisted, 0)
})

test_that("status_async() on a list field or a `[[` read is hoisted; reactive fields are not touched", {
  mod <- list(up = reactiveAsync(1))
  rv <- shiny::reactiveValues(up = 1)
  input <- shiny::reactiveValues(x = 1)
  a <- analyze_expr(quote(status_async(mod$up) + has_value_async(mod[["up"]]) + input[["x"]]), environment())
  expect_equal(a$hoisted[[1]], quote(status_async(mod$up)))
  expect_equal(a$hoisted[[2]], quote(has_value_async(mod[["up"]])))
  expect_equal(names(a$inputs), "input[[\"x\"]]")
  expect_false("mod" %in% a$free)
  a <- analyze_expr(quote(status_async(rv$up)), environment())
  expect_equal(a$hoisted, list(quote(rv$up)))

  cap <- new_capture(quote(input[["x"]] + has_value_async(mod$up)), environment())
  expect_identical(cap$upstream(), list())
  cap <- new_capture(quote(mod$up + up2()), environment())
  up2 <- mod$up
  expect_identical(cap$upstream(), list(up2))
})

test_that("run inputs reuse the evaluated hoisted values", {
  input <- shiny::reactiveValues(n = 1)
  calls <- 0
  data <- shiny::reactive({ calls <<- calls + 1; 2 })
  cap <- new_capture(quote(input$n + data()), environment())
  job <- shiny::isolate(cap$resolve())
  expect_equal(job$inputs, list(`input$n` = 1, `data()` = 2))
  expect_equal(calls, 1)
})

test_that("a reactive in a module's returned list is hoisted as a read", {
  mod <- list(wind = shiny::reactive(1), up = reactiveAsync(2))
  a <- analyze_expr(quote(mod$wind() + mod[["up"]]()), environment())
  expect_equal(a$hoisted, list(quote(mod$wind()), quote(mod[["up"]]())))
  expect_equal(names(a$inputs), c("mod$wind()", "mod[[\"up\"]]()"))
  expect_false("mod" %in% a$free)

  cap <- new_capture(quote(mod$wind() + mod$up()), environment())
  expect_identical(cap$upstream(), list(mod$up))
  a <- analyze_expr(quote(sapply(1:2, function(mod) mod$wind())), environment())
  expect_length(a$hoisted, 0)
})

test_that("a name becomes local only after its assignment", {
  data <- shiny::reactive(1:3)
  wind <- shiny::reactive(2)
  input <- shiny::reactiveValues(n = 1)
  a <- analyze_expr(quote({
    data <- data()
    for (i in seq_along(data)) wind <- wind() * i
    input <- list(n = 10)
    sum(data) * wind + input$n
  }), environment())

  expect_equal(a$hoisted, list(quote(data()), quote(wind())))
  expect_false(any(c("data", "wind", "input") %in% a$free))
  expect_equal(a$expr[[2]], quote(data <- .sa_1))
})

test_that("a self-shadowing assignment ships nothing and reads the reactive", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    data <- shiny::reactive(c(input$n, input$n))
    r <- reactiveAsync({
      data <- data()
      sum(data)
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 4)
    wait_for(function() identical(peek(r), 8), session)
    expect_equal(peek(r), 8)
  })
})

test_that("a function the session cannot resolve warns once and still dispatches", {
  input <- shiny::reactiveValues(n = 1)
  cap <- new_capture(quote(no_such_fn(input$n)), environment())
  expect_warning(
    job <- shiny::isolate(cap$resolve()),
    "`no_such_fn\\(\\)` is not defined in the session.*library\\(\\).*pkg::no_such_fn\\(\\)"
  )
  expect_equal(job$args, list(.sa_1 = 1))
  expect_silent(shiny::isolate(cap$resolve()))
  expect_equal(cap$describe()$missing, "no_such_fn")
})

test_that("only unresolved names in call position warn", {
  env <- new.env(parent = globalenv())
  # Formula and data-mask symbols are values, not calls.
  expect_silent(deps <- collect_globals(quote(lm(mpg ~ cyl, data = mtcars)), env))
  expect_length(deps$missing, 0)
  expect_silent(deps <- collect_globals(quote(stats::sd(x) + median(y)), env))
  expect_length(deps$missing, 0)
  deps <- collect_globals(quote(subset(mtcars, mpg > undefined_var)), env)
  expect_length(deps$missing, 0)
  deps <- collect_globals(quote(undefined_fn(1)), env, resident = "undefined_fn")
  expect_length(deps$missing, 0)
  deps <- collect_globals(quote(undefined_fn(1)), env)
  expect_equal(deps$missing, "undefined_fn")
})

test_that("an unresolved function inside a shipped helper is reported", {
  helper <- function(x) GET_nowhere(x)
  cap <- new_capture(quote(helper(input$n)), environment())
  input <- shiny::reactiveValues(n = 1)
  expect_warning(shiny::isolate(cap$resolve()), "GET_nowhere\\(\\)")
  expect_silent(shiny::isolate(cap$resolve()))
})

test_that("the unresolved-function warning reaches the worker error", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync(no_such_fn(input$n))
    output$x <- shiny::renderText(r())
  }, {
    expect_warning(session$setInputs(n = 1), "no_such_fn\\(\\)")
    wait_for(function() status_async(r) == "error", session)
    expect_error(shiny::isolate(r()), "no_such_fn")
  })
})

test_that("session calls through a module object, as box::use(shiny) binds them, are hoisted", {
  input <- shiny::reactiveValues(n = 5, seed = 1)
  # The shape box::use(shiny) and box::use(shinyasync) leave in the caller:
  # an environment whose fields are the package's exports.
  shiny <- new.env()
  for (nm in session_fns) assign(nm, getExportedValue("shiny", nm), envir = shiny)
  shinyasync <- list(status_async = status_async, has_value_async = has_value_async)
  up <- reactiveAsync(1)
  a <- analyze_expr(quote({
    shiny$req(input$n)
    shiny$validate(shiny$need(input$n > 0, "positive"))
    shiny$isolate(input$seed) + shinyasync$status_async(up) + shiny[["isolate"]](input$n)
  }), environment())

  expect_equal(a$hoisted[[1]], quote(shiny$req(input$n)))
  expect_equal(a$hoisted[[2]], quote(shiny$validate(shiny$need(input$n > 0, "positive"))))
  expect_equal(a$hoisted[[3]], quote(shiny$isolate(input$seed)))
  expect_equal(a$hoisted[[4]], quote(shinyasync$status_async(up)))
  expect_equal(a$hoisted[[5]], quote(shiny[["isolate"]](input$n)))
  expect_equal(names(a$inputs), "input$n")
  expect_false(any(c("shiny", "shinyasync", "up") %in% a$free))

  # A module object with a same-named field that is not the shiny function
  # is an ordinary call and runs in the worker.
  other <- list(req = function(x) x)
  a <- analyze_expr(quote(other$req(input$n)), environment())
  expect_equal(a$hoisted, list(quote(input$n)))
  expect_true("other" %in% a$free)
})

test_that("a function with a NULL body keeps it when the expression has srcrefs", {
  # Parsed with srcrefs, as a sourced file, a box module or the console has
  # it, `function(e) NULL` carries its srcref as a fourth element.
  expr <- parse(text = "tryCatch(stop('boom'), error = function(e) NULL)", keep.source = TRUE)[[1]]
  a <- analyze_expr(expr, environment())
  handler <- a$expr[[3]]
  expect_identical(handler[[1]], quote(`function`))
  expect_null(handler[[3]])
  expect_null(eval(a$expr, globalenv()))
})

# The shapes box gives a module: its own environment, named "namespace:<mod>"
# and marked by `.__module__.`, whose parent holds its imports; and the
# `box$mod` environment a top-level box::use() attaches.
box_namespace <- function(name, imports = list()) {
  imp <- list2env(imports, parent = baseenv())
  attr(imp, "name") <- paste0("imports:", name)
  ns <- new.env(parent = imp)
  attr(ns, "name") <- paste0("namespace:", name)
  assign(".__module__.", new.env(), envir = ns)
  ns
}
in_env <- function(f, env) {
  environment(f) <- env
  f
}

test_that("a function defined in a box module is shipped", {
  view <- box_namespace("view")
  view$local_helper <- in_env(function(x) x + offset, view)
  view$offset <- 1
  server_env <- new.env(parent = view)

  deps <- collect_globals(quote(local_helper(41)), server_env)
  expect_named(deps$values, "local_helper")
  expect_length(deps$missing, 0)
  f <- deps$values$local_helper
  expect_identical(parent.env(environment(f)), globalenv())
  expect_equal(f(41), 42)
})

test_that("an imported box function is shipped with the helpers its module defines", {
  helpers <- box_namespace("helpers")
  helpers$helper <- in_env(function(x) x * 2, helpers)
  helpers$f <- in_env(function(x) helper(x), helpers)
  view <- box_namespace("view", imports = list(f = helpers$f))
  server_env <- new.env(parent = view)

  deps <- collect_globals(quote(f(21)), server_env)
  f <- deps$values$f
  expect_setequal(ls(environment(f)), c("f", "helper"))
  expect_equal(f(21), 42)
})

test_that("a function attached by a top-level box::use() is shipped", {
  helpers <- box_namespace("helpers")
  helpers$f <- in_env(function(x) x * 2, helpers)
  attached <- new.env(parent = globalenv())
  class(attached) <- "box$mod"
  attr(attached, "name") <- "mod:helpers"
  attached$f <- helpers$f
  caller <- new.env(parent = attached)

  deps <- collect_globals(quote(f(21)), caller)
  expect_named(deps$values, "f")
  expect_equal(deps$values$f(21), 42)
})

test_that("box module functions run on the worker", {
  local_pool(1)
  view <- box_namespace("view", imports = list(reactiveAsync = reactiveAsync))
  view$local_helper <- in_env(function(x) x + 1, view)
  server <- in_env(function(input, output, session) {
    r <- reactiveAsync(local_helper(input$n))
    output$x <- shiny::renderText(r())
  }, new.env(parent = view))

  shiny::testServer(server, {
    session$setInputs(n = 41)
    wait_for(function() status_async(r) == "success", session)
    expect_equal(shiny::isolate(r()), 42)
  })
})

test_that("an S4 generic a box module imports is shipped as it is", {
  # Rebound to a clone, a generic loses the method tables in its own
  # environment and dispatch fails ("unable to find an inherited method").
  view <- box_namespace("view", imports = list(show_it = show))
  deps <- collect_globals(quote(show_it(1)), new.env(parent = view))
  expect_identical(deps$values$show_it, show)
})
