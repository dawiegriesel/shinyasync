test_that("mirai_async ships helpers, their callees and globals", {
  local_pool()
  offset <- 100
  helper <- function(x) x + offset
  fit <- function(n) helper(n)
  m <- mirai_async(fit(2))
  expect_s3_class(m, "mirai")
  expect_equal(mirai::call_mirai(m)$data, 102)
})

test_that("reactive reads register on the enclosing observer", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    scale <- function(n) n * 10
    v <- reactiveValAsync()
    shiny::observe(v(mirai_async(scale(input$n))))
  }, {
    session$setInputs(n = 1)
    wait_for(function() identical(peek(v), 10), session)
    expect_equal(shiny::isolate(v()), 10)
    session$setInputs(n = 2)
    wait_for(function() identical(peek(v), 20), session)
    expect_equal(shiny::isolate(v()), 20)
  })
})

test_that("works with bindEvent() on the observer", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    v <- reactiveValAsync(0)
    shiny::observe(v(mirai_async(input$n + 1))) |> shiny::bindEvent(input$go)
  }, {
    session$setInputs(n = 1)
    session$flushReact()
    expect_equal(shiny::isolate(v()), 0)
    session$setInputs(go = 1)
    wait_for(function() identical(peek(v), 2), session)
    session$setInputs(n = 5)
    session$flushReact()
    expect_equal(shiny::isolate(v()), 2)
    session$setInputs(go = 2)
    wait_for(function() identical(peek(v), 6), session)
  })
})

test_that("a hoisted req() halts the observer and leaves the value alone", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    v <- reactiveValAsync("initial")
    shiny::observe(v(mirai_async({
      req(input$n > 0)
      input$n
    })))
  }, {
    session$setInputs(n = 0)
    session$flushReact()
    expect_equal(status_async(v), "initial")
    expect_equal(shiny::isolate(v()), "initial")
    session$setInputs(n = 3)
    wait_for(function() identical(peek(v), 3), session)
  })
})

test_that("unresolvable reactive reads error from the call", {
  local_pool()
  input <- shiny::reactiveValues(x1 = 1)
  expect_error(
    shiny::isolate(mirai_async(input[[paste0("x", 1)]])),
    "reactive object"
  )
})

test_that("globals take precedence over the calling environment", {
  local_pool()
  k <- 1
  m <- mirai_async(k * 2, globals = list(k = 5))
  expect_equal(mirai::call_mirai(m)$data, 10)
})

test_that("... is passed to mirai()", {
  local_pool()
  m <- mirai_async(Sys.sleep(5), .timeout = 100)
  expect_true(mirai::is_error_value(mirai::call_mirai(m)$data))
})

test_that("quoted expressions are accepted", {
  local_pool()
  e <- quote(sum(1:4))
  expect_equal(mirai::call_mirai(mirai_async(e, quoted = TRUE))$data, 10)
})

test_that("fan-out from a function frame ships its locals", {
  local_pool()
  weight <- 10
  score <- function(i) i * weight
  ms <- lapply(1:3, function(i) mirai_async(score(i)))
  expect_equal(lapply(ms, function(m) mirai::call_mirai(m)$data), list(10, 20, 30))
})
