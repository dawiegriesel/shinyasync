test_that("value resolves in a worker and updates with its dependencies", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync({
      Sys.sleep(1)
      input$n * 2
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 2)
    expect_equal(peek(r), "<halted>")
    wait_for(function() status_async(r) == "success", session)
    expect_equal(peek(r), 4)

    session$setInputs(n = 5)
    wait_for(function() identical(peek(r), 10), session)
    expect_equal(status_async(r), "success")
  })
})

test_that("reading the status alone starts the task", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync(input$n * 2)
    # Nothing reads r() until the status says so; the status read must start it.
    output$x <- shiny::renderText(if (status_async(r) == "success") r() else "")
  }, {
    session$setInputs(n = 3)
    # No value read here: peek() would start the task itself.
    wait_for(function() status_async(r) == "success", session, timeout = 3)
    expect_equal(peek(r), 6)
    expect_equal(output$x, "6")
  })
})

test_that("runs in a different process", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync(Sys.getpid())
    output$x <- shiny::renderText(r())
  }, {
    session$flushReact()
    wait_for(function() status_async(r) == "success", session)
    expect_false(peek(r) == Sys.getpid())
  })
})

test_that("previous value is returned while a new task runs", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync({
      Sys.sleep(input$wait)
      input$n
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 1, wait = 0)
    wait_for(function() identical(peek(r), 1), session)

    session$setInputs(n = 2, wait = 1)
    expect_equal(status_async(r), "running")
    expect_equal(peek(r), 1)
    wait_for(function() identical(peek(r), 2), session)
  })
})

test_that("a new invalidation cancels the running task without an error", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    log <- new.env()
    log$seen <- character()
    r <- reactiveAsync({
      Sys.sleep(if (input$n == 1) 5 else 0)
      input$n
    })
    shiny::observe(log$seen <- c(log$seen, status_async(r)))
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 1)
    expect_equal(status_async(r), "running")
    session$setInputs(n = 2)
    wait_for(function() identical(peek(r), 2), session)
    expect_true("running" %in% log$seen)
    expect_false("error" %in% log$seen)
  })
})

test_that("worker errors surface on read and clear on the next success", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync({
      if (input$fail) stop("boom")
      "ok"
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(fail = TRUE)
    wait_for(function() status_async(r) == "error", session)
    expect_error(shiny::isolate(r()), "boom")

    session$setInputs(fail = FALSE)
    wait_for(function() status_async(r) == "success", session)
    expect_equal(peek(r), "ok")
  })
})

test_that("req() halts quietly on the session side", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync(shiny::req(input$n) + 1)
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = NULL)
    expect_equal(status_async(r), "initial")
    session$setInputs(n = 1)
    wait_for(function() identical(peek(r), 2), session)
  })
})

test_that("req() on worker-local values halts quietly and keeps the last value", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync({
      x <- input$n
      shiny::req(x > 0)
      x
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 1)
    wait_for(function() identical(peek(r), 1), session)
    session$setInputs(n = -1)
    wait_for(function() status_async(r) != "running", session)
    expect_equal(status_async(r), "success")
    expect_equal(peek(r), 1)
  })
})

test_that("invalidateLater() re-runs the task", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync({
      shiny::invalidateLater(1000)
      runif(1)
    })
    output$x <- shiny::renderText(r())
  }, {
    session$flushReact()
    wait_for(function() status_async(r) == "success", session)
    first <- peek(r)
    session$elapse(1001)
    wait_for(function() !identical(peek(r), first), session)
    expect_false(identical(peek(r), first))
  })
})

test_that("tasks are not started until the reactive is read", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync(input$n)
  }, {
    session$setInputs(n = 1)
    expect_equal(status_async(r), "initial")
  })
})

test_that("queue mode runs every invocation in order", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    log <- new.env()
    log$results <- c()
    r <- reactiveAsync({
      Sys.sleep(0.3)
      input$n
    }, mode = "queue")
    shiny::observe(log$results <- c(log$results, r()))
  }, {
    session$setInputs(n = 1)
    session$setInputs(n = 2)
    session$setInputs(n = 3)
    wait_for(function() identical(peek(r), 3), session)
    expect_equal(log$results, c(1, 2, 3))
  })
})

test_that("cancel_async stops the running task and keeps the last value", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync({
      Sys.sleep(input$wait)
      input$wait
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(wait = 0)
    wait_for(function() identical(peek(r), 0), session)
    session$setInputs(wait = 5)
    expect_equal(status_async(r), "running")
    cancel_async(r)
    session$flushReact()
    expect_equal(status_async(r), "idle")
    expect_equal(peek(r), 0)
  })
})

test_that("status_async rejects plain reactives", {
  expect_error(status_async(shiny::reactive(1)), "reactiveAsync")
})

test_that("isolate() does not restart the task and conditional req() works end to end", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    runs <- shiny::reactiveVal(0)
    r <- reactiveAsync({
      seed <- isolate(input$seed)
      if (is.null(input$a)) "none" else req(input$b)
    })
    shiny::observe({ status_async(r); if (status_async(r) == "running") runs(isolate(runs()) + 1) })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(seed = 1, a = NULL, b = NULL)
    wait_for(function() identical(peek(r), "none"), session)
    n1 <- shiny::isolate(runs())
    session$setInputs(seed = 2)
    session$flushReact()
    later::run_now(0.3)
    expect_equal(shiny::isolate(runs()), n1)

    session$setInputs(a = 1)
    wait_for(function() runs() > n1 && status_async(r) != "running", session)
    expect_equal(shiny::isolate(status_async(r)), "success")
    expect_equal(peek(r), "none")
    session$setInputs(b = "b")
    wait_for(function() identical(peek(r), "b"), session)
  })
})

test_that("helpers, NULL globals and modified server objects work in a worker", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    limit <- NULL
    df <- data.frame(x = 1:3)
    scale <- 10
    f1 <- local({ k <- 1; function() k })
    f2 <- local({ k <- 2; function() k })
    total <- function(d) sum(d$y) * scale
    r <- reactiveAsync({
      df$y <- df$x * input$m
      if (is.null(limit)) total(df) + f1() + f2() else limit
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(m = 2)
    wait_for(function() identical(peek(r), 123), session)
    expect_equal(peek(r), 123)
  })
})

test_that("reactiveAsync works inside a module and captures module locals", {
  local_pool()
  mod <- function(id, k) {
    shiny::moduleServer(id, function(input, output, session) {
      scale <- function(x) x * k
      r <- reactiveAsync(scale(input$n))
      output$x <- shiny::renderText(r())
      r
    })
  }
  shiny::testServer(mod, args = list(id = "mod", k = 10), {
    session$setInputs(n = 4)
    wait_for(function() identical(peek(r), 40), session)
    expect_equal(peek(r), 40)
    expect_equal(session$ns("n"), "mod-n")
  })
})
