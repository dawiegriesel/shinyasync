test_that("a silent failure clears a previous error", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync({
      n <- input$n
      if (n < 0) stop("negative")
      req(n > 0)
      n
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = -1)
    wait_for(function() status_async(r) == "error", session)
    expect_error(shiny::isolate(r()), "negative")

    session$setInputs(n = 0)
    wait_for(function() status_async(r) == "idle", session)
    expect_equal(peek(r), "<halted>")
  })
})

test_that("cancelling a task that was started after an error clears the error", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync({
      n <- input$n
      if (n < 0) stop("negative")
      Sys.sleep(5)
      n
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = -1)
    wait_for(function() status_async(r) == "error", session)
    session$setInputs(n = 1)
    wait_for(function() status_async(r) == "running", session)
    cancel_async(r)
    expect_equal(shiny::isolate(status_async(r)), "idle")
    expect_equal(peek(r), "<halted>")
  })
})

test_that("the post-registration check is gated on the nanonext version", {
  local_mocked_bindings(packageVersion = function(...) package_version("1.10.3"), .package = "utils")
  expect_true(loses_notification())
  local_mocked_bindings(packageVersion = function(...) package_version("1.10.3.9000"), .package = "utils")
  expect_false(loses_notification())
})

test_that("a task that completes before its promise is registered still settles", {
  # mirai's as.promise() checks unresolved() and then registers the
  # notification with .keep(); a task that completes in between never
  # notifies. Widen that window so every task falls into it.
  local_pool()
  keep <- get(".keep", asNamespace("nanonext"))
  local_mocked_bindings(.keep = function(x, ctx) { Sys.sleep(0.2); keep(x, ctx) }, .package = "mirai")
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync(input$n, mode = "queue")
    shiny::observe(r())
  }, {
    for (i in 1:3) session$setInputs(n = i)
    wait_for(function() identical(peek(r), 3L), session, timeout = 5)
    expect_equal(status_async(r), "success")
  })
})
