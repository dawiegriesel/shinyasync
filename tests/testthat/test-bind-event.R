test_that("bindEvent gates the task and the result still updates", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync(input$n * 10) |> shiny::bindEvent(input$go)
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 1)
    expect_equal(status_async(r), "initial")

    session$setInputs(go = 1)
    wait_for(function() identical(peek(r), 10), session)

    session$setInputs(n = 2)
    expect_equal(status_async(r), "success")
    expect_equal(peek(r), 10)

    session$setInputs(go = 2)
    wait_for(function() identical(peek(r), 20), session)
  })
})

test_that("bindEvent works on observeAsync", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    o <- observeAsync(input$n) |> shiny::bindEvent(input$go)
  }, {
    session$setInputs(n = 1)
    expect_equal(o$status(), "initial")
    session$setInputs(go = 1)
    wait_for(function() o$status() == "success", session)
  })
})

test_that("a bindEvent() downstream clicked while its upstream is busy runs once it settles", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    data <- reactiveAsync({
      Sys.sleep(1)
      paste0("data-", input$file)
    })
    fit <- reactiveAsync(paste(data(), input$formula)) |> shiny::bindEvent(input$go)
    log <- new.env()
    log$seen <- character()
    shiny::observe(log$seen <- c(log$seen, fit()))
  }, {
    session$setInputs(file = "a", formula = "f")
    expect_equal(status_async(fit), "initial")
    expect_equal(peek(fit), "<halted>")

    # Click before the upstream has started: it starts, and the click holds.
    session$setInputs(go = 1)
    expect_equal(status_async(fit), "running")
    expect_equal(status_async(data), "running")
    wait_for(function() identical(peek(fit), "data-a f"), session)
    expect_equal(status_async(fit), "success")

    # Click while the upstream is re-running: no stale result, one run after.
    session$setInputs(file = "b")
    session$flushReact()
    session$setInputs(go = 2)
    expect_equal(status_async(fit), "running")
    expect_equal(peek(fit), "data-a f")
    wait_for(function() identical(peek(fit), "data-b f"), session)
    expect_equal(log$seen, c("data-a f", "data-b f"))

    # Without a click nothing runs, even though the upstream changed.
    session$setInputs(file = "c")
    wait_for(function() identical(peek(data), "data-c"), session)
    expect_equal(peek(fit), "data-b f")
    expect_equal(status_async(fit), "success")
  })
})

test_that("cancel_async() while waiting on an upstream drops the pending click", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    data <- reactiveAsync({
      Sys.sleep(1)
      paste0("data-", input$file)
    })
    fit <- reactiveAsync(paste(data(), "f")) |> shiny::bindEvent(input$go)
    output$x <- shiny::renderText(fit())
  }, {
    session$setInputs(file = "a", go = 1)
    expect_equal(status_async(fit), "running")
    cancel_async(fit)
    expect_equal(status_async(fit), "idle")
    wait_for(function() identical(peek(data), "data-a"), session)
    expect_equal(status_async(fit), "idle")
    expect_equal(peek(fit), "<halted>")
  })
})

test_that("a pending click is dropped when its upstream is not running", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    data <- reactiveAsync(paste0("data-", input$file)) |> shiny::bindEvent(input$load)
    fit <- reactiveAsync(paste(data(), "f")) |> shiny::bindEvent(input$go)
    log <- new.env()
    log$seen <- character()
    shiny::observe(log$seen <- c(log$seen, fit()))
  }, {
    session$setInputs(file = "a")
    # The upstream is idle and not going to produce: the click is lost, as
    # it would be with a reactive() whose req() fails.
    session$setInputs(go = 1)
    expect_equal(status_async(fit), "initial")
    expect_equal(peek(fit), "<halted>")
    session$setInputs(load = 1)
    wait_for(function() identical(peek(data), "data-a"), session)
    wait_for(function() status_async(fit) != "running", session)
    expect_equal(status_async(fit), "initial")
    expect_equal(peek(fit), "<halted>")
    expect_equal(log$seen, character())
    session$setInputs(go = 2)
    wait_for(function() identical(peek(fit), "data-a f"), session)
  })
})

test_that("cancel_async() on a waiting task with a value reports idle", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    data <- reactiveAsync({
      Sys.sleep(1)
      paste0("data-", input$file)
    })
    fit <- reactiveAsync(paste(data(), "f")) |> shiny::bindEvent(input$go)
    output$x <- shiny::renderText(fit())
  }, {
    session$setInputs(file = "a", go = 1)
    wait_for(function() identical(peek(fit), "data-a f"), session)
    session$setInputs(file = "b")
    session$flushReact()
    session$setInputs(go = 2)
    expect_equal(status_async(fit), "running")
    cancel_async(fit)
    expect_equal(status_async(fit), "idle")
    expect_true(has_value_async(fit))
    wait_for(function() identical(peek(data), "data-b"), session)
    expect_equal(status_async(fit), "idle")
    expect_equal(peek(fit), "data-a f")
  })
})
