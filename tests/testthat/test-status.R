test_that("has_value_async tells a first run from a refresh", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync({
      Sys.sleep(isolate(input$wait))
      n <- input$n
      if (n < 0) stop("negative")
      req(n > 0)
      n
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 1, wait = 0)
    expect_equal(status_async(r), "running")
    expect_false(shiny::isolate(has_value_async(r)))
    wait_for(function() identical(peek(r), 1), session)
    expect_true(shiny::isolate(has_value_async(r)))

    # Refresh: running with a previous value on screen.
    session$setInputs(wait = 1)
    session$setInputs(n = 2)
    expect_equal(status_async(r), "running")
    expect_true(shiny::isolate(has_value_async(r)))
    wait_for(function() identical(peek(r), 2), session)

    # A worker-side req() halt keeps the value.
    session$setInputs(wait = 0)
    session$setInputs(n = 0)
    wait_for(function() status_async(r) != "running", session)
    expect_equal(peek(r), 2)
    expect_true(shiny::isolate(has_value_async(r)))

    # An error hides the value until the next success.
    session$setInputs(n = -1)
    wait_for(function() status_async(r) == "error", session)
    expect_false(shiny::isolate(has_value_async(r)))
    session$setInputs(n = 3)
    value <- function() tryCatch(shiny::isolate(r()), error = function(e) NULL)
    wait_for(function() identical(value(), 3), session)
    expect_true(shiny::isolate(has_value_async(r)))
  })
})

test_that("has_value_async starts a task that has not run yet", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync(input$n)
    output$x <- shiny::renderText(if (has_value_async(r)) r() else "loading")
  }, {
    session$setInputs(n = 4)
    expect_equal(output$x, "loading")
    wait_for(function() has_value_async(r), session, timeout = 3)
    expect_equal(output$x, "4")
  })
})

test_that("has_value_async works for observers and async reactive values", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    o <- observeAsync(input$n)
    empty <- reactiveValAsync()
    v <- reactiveValAsync(0)
  }, {
    expect_false(shiny::isolate(has_value_async(o)))
    session$setInputs(n = 1)
    wait_for(function() o$status() == "success", session)
    expect_true(shiny::isolate(has_value_async(o)))

    # Like reactiveVal(), an async reactive value always has one, even NULL.
    expect_true(shiny::isolate(has_value_async(empty)))
    expect_true(shiny::isolate(has_value_async(v)))
    v(mirai::mirai({ Sys.sleep(5); 1 }))
    expect_equal(status_async(v), "running")
    expect_true(shiny::isolate(has_value_async(v)))
    v(1)
    v(mirai::mirai(stop("nope")))
    wait_for(function() status_async(v) == "error", session)
    expect_false(shiny::isolate(has_value_async(v)))
  })
})

test_that("has_value_async rejects plain reactives and is hoisted in async expressions", {
  expect_error(has_value_async(shiny::reactive(1)), "reactiveAsync")
  up <- reactiveAsync(1)
  a <- analyze_expr(quote(if (has_value_async(up)) 1 else 0), environment())
  expect_equal(a$hoisted, list(quote(has_value_async(up))))
  expect_false("up" %in% a$free)
})

test_that("has_value_async agrees with the read: a value, a halt or an error", {
  local_pool()
  agrees <- function(r) {
    read <- tryCatch(list(shiny::isolate(r())), shiny.silent.error = function(e) "halt",
                     error = function(e) "error")
    expect_equal(shiny::isolate(has_value_async(r)), is.list(read))
  }
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync({ n <- input$n; if (n < 0) stop("negative"); n })
    v <- reactiveValAsync()
    output$x <- shiny::renderText(r())
  }, {
    agrees(v)
    v(mirai::mirai(stop("nope")))
    wait_for(function() status_async(v) == "error", session)
    agrees(v)

    session$setInputs(n = 1)
    agrees(r)
    wait_for(function() status_async(r) == "success", session)
    agrees(r)
    session$setInputs(n = -1)
    wait_for(function() status_async(r) == "error", session)
    agrees(r)
  })
})
