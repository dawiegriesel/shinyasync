test_that("inputs_async reports the reads the current value was computed from", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    data <- shiny::reactive(input$k * 2)
    r <- reactiveAsync({
      req(input$n)
      seed <- isolate(input$seed)
      input$n + data()
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 1, k = 2, seed = 9)
    expect_null(shiny::isolate(inputs_async(r)))
    wait_for(function() identical(peek(r), 5), session)
    expect_equal(shiny::isolate(inputs_async(r)), list(`input$n` = 1, `data()` = 4))

    session$setInputs(n = 3)
    wait_for(function() identical(peek(r), 7), session)
    expect_equal(shiny::isolate(inputs_async(r)), list(`input$n` = 3, `data()` = 4))
  })
})

test_that("stale_async tracks inputs, ignores isolated reads and re-runs", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync({
      seed <- isolate(input$seed)
      Sys.sleep(isolate(input$wait))
      input$n
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 1, seed = 1, wait = 0)
    expect_true(shiny::isolate(stale_async(r)))
    wait_for(function() identical(peek(r), 1), session)
    expect_false(shiny::isolate(stale_async(r)))
    expect_equal(shiny::isolate(inputs_async(r)), list(`input$n` = 1))

    # Isolated reads are not part of the provenance.
    session$setInputs(seed = 2, wait = 1)
    session$flushReact()
    expect_equal(status_async(r), "success")
    expect_false(shiny::isolate(stale_async(r)))

    session$setInputs(n = 2)
    expect_equal(status_async(r), "running")
    expect_true(shiny::isolate(stale_async(r)))
    wait_for(function() identical(peek(r), 2), session)
    expect_false(shiny::isolate(stale_async(r)))
  })
})

test_that("a worker-side req() halt keeps the previous value and its inputs", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync({
      x <- input$n
      req(x > 0)
      x
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 1)
    wait_for(function() identical(peek(r), 1), session)
    session$setInputs(n = -1)
    wait_for(function() status_async(r) != "running", session)
    expect_equal(peek(r), 1)
    expect_equal(shiny::isolate(inputs_async(r)), list(`input$n` = 1))
    expect_true(shiny::isolate(stale_async(r)))
  })
})

test_that("a superseded run does not overwrite the inputs of the last value", {
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
    session$setInputs(n = 2, wait = 5)
    expect_equal(status_async(r), "running")
    cancel_async(r)
    session$flushReact()
    expect_equal(shiny::isolate(inputs_async(r)), list(`input$wait` = 0, `input$n` = 1))
  })
})

# The scenario from issue #2: a chained async briefly combines a new input
# with an upstream value computed from the old one.
chained_server <- function(guard) {
  function(input, output, session) {
    wind <- reactiveAsync({
      Sys.sleep(1)
      paste0("wind-", input$spot)
    })
    scored <- if (guard) {
      reactiveAsync({
        req(!stale_async(wind))
        paste(wind(), input$spot)
      })
    } else {
      reactiveAsync(paste(wind(), input$spot))
    }
    log <- new.env()
    log$seen <- character()
    shiny::observe(log$seen <- c(log$seen, scored()))
  }
}

test_that("without a guard a chained async sees a mismatched upstream value", {
  local_pool()
  shiny::testServer(chained_server(guard = FALSE), {
    session$setInputs(spot = "a")
    wait_for(function() identical(peek(scored), "wind-a a"), session)
    session$setInputs(spot = "b")
    wait_for(function() identical(peek(scored), "wind-b b"), session)
    expect_true("wind-a b" %in% log$seen)
  })
})

test_that("req(!stale_async()) waits for the upstream and then re-runs", {
  local_pool()
  shiny::testServer(chained_server(guard = TRUE), {
    session$setInputs(spot = "a")
    wait_for(function() identical(peek(scored), "wind-a a"), session)
    session$setInputs(spot = "b")
    expect_equal(peek(scored), "wind-a a")
    # Nothing else touches scored: the stored inputs of wind must re-run it.
    wait_for(function() identical(peek(scored), "wind-b b"), session)
    expect_equal(log$seen, c("wind-a a", "wind-b b"))
  })
})

test_that("stale_async() and status_async() are hoisted inside an async expression", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    wind <- reactiveAsync({
      Sys.sleep(1)
      paste0("wind-", input$spot)
    })
    scored <- reactiveAsync({
      if (stale_async(wind)) "waiting" else paste(wind(), input$spot, status_async(wind))
    })
    output$x <- shiny::renderText(scored())
  }, {
    session$setInputs(spot = "a")
    # wind() in the untaken branch halts until wind has a value, as documented.
    wait_for(function() identical(peek(scored), "wind-a a success"), session)
    session$setInputs(spot = "b")
    wait_for(function() identical(peek(scored), "waiting"), session)
    wait_for(function() identical(peek(scored), "wind-b b success"), session)
    expect_equal(output$x, "wind-b b success")
  })
})

test_that("a halting upstream read counts as stale", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    gated <- shiny::reactive(shiny::req(input$n))
    r <- reactiveAsync(gated() * 2)
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 1)
    wait_for(function() identical(peek(r), 2), session)
    expect_false(shiny::isolate(stale_async(r)))
    session$setInputs(n = NULL)
    session$flushReact()
    expect_true(shiny::isolate(stale_async(r)))
    expect_equal(peek(r), 2)
  })
})

test_that("an upstream error propagates through the stale guard", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    wind <- reactiveAsync({
      if (input$spot == "bad") stop("no such spot")
      paste0("wind-", input$spot)
    })
    scored <- reactiveAsync({
      req(!stale_async(wind))
      paste(wind(), input$spot)
    })
    output$x <- shiny::renderText(scored())
  }, {
    session$setInputs(spot = "a")
    wait_for(function() identical(peek(scored), "wind-a a"), session)
    session$setInputs(spot = "bad")
    wait_for(function() status_async(scored) == "error", session)
    expect_error(shiny::isolate(scored()), "no such spot")
    expect_error(shiny::isolate(stale_async(wind)), "no such spot")
    # The error is re-thrown until wind recomputes, as wind() itself would.
    session$setInputs(spot = "c")
    value <- function() tryCatch(shiny::isolate(scored()), error = function(e) NULL)
    wait_for(function() identical(value(), "wind-c c"), session)
    expect_equal(status_async(wind), "success")
  })
})

test_that("staleness follows a chain of async reactives", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    a <- reactiveAsync({
      Sys.sleep(1)
      paste0("a-", input$spot)
    })
    b <- reactiveAsync(paste0("b(", a(), ")"))
    final <- reactiveAsync({
      req(!stale_async(b))
      paste(b(), input$spot)
    })
    log <- new.env()
    log$seen <- character()
    shiny::observe(log$seen <- c(log$seen, final()))
  }, {
    session$setInputs(spot = "x")
    wait_for(function() identical(peek(final), "b(a-x) x"), session)
    session$setInputs(spot = "y")
    expect_true(shiny::isolate(stale_async(a)))
    expect_true(shiny::isolate(stale_async(b)))
    wait_for(function() identical(peek(final), "b(a-y) y"), session)
    expect_equal(log$seen, c("b(a-x) x", "b(a-y) y"))
    expect_false(shiny::isolate(stale_async(b)))
  })
})

test_that("stale_async() on a module's returned reactive is hoisted", {
  local_pool()
  windServer <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      list(wind = reactiveAsync(paste0("wind-", input$spot)))
    })
  }
  shiny::testServer(function(input, output, session) {
    mod <- windServer("w")
    scored <- reactiveAsync({
      if (stale_async(mod$wind)) "waiting" else paste(status_async(mod[["wind"]]), input$k)
    })
    output$x <- shiny::renderText(scored())
  }, {
    session$setInputs(`w-spot` = "a", k = 1)
    wait_for(function() identical(peek(scored), "success 1"), session)
    expect_equal(output$x, "success 1")
  })
})

test_that("provenance helpers reject objects without captured inputs", {
  expect_error(stale_async(shiny::reactive(1)), "needs an object created by reactiveAsync\\(\\)\\.")
  expect_error(inputs_async(reactiveValAsync(1)), "needs an object created by reactiveAsync\\(\\)\\.")
  o <- observeAsync(1)
  expect_error(stale_async(o), "needs an object created by reactiveAsync\\(\\)\\.")
  expect_error(stale_async(1), "debounce")
})
