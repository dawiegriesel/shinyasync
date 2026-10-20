test_that("reactiveValAsync accepts plain values, mirai and promises", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    v <- reactiveValAsync(0)
  }, {
    expect_equal(shiny::isolate(v()), 0)
    expect_equal(status_async(v), "idle")

    v(1)
    expect_equal(shiny::isolate(v()), 1)

    v(mirai::mirai(x * 2, x = 21))
    expect_equal(status_async(v), "running")
    expect_equal(shiny::isolate(v()), 1)
    wait_for(function() status_async(v) == "success", session)
    expect_equal(shiny::isolate(v()), 42)

    v(promises::promise_resolve("p"))
    wait_for(function() identical(v(), "p"), session)
  })
})

test_that("values not made by an assignment get distinct labels", {
  vals <- lapply(1:3, function(i) reactiveValAsync())
  labels <- vapply(vals, function(v) attr(v, "shinyasync")$label, "")
  expect_match(labels, "^reactiveValAsync#[0-9]+$")
  expect_length(unique(labels), 3L)
})

test_that("a newer task supersedes an older one", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    v <- reactiveValAsync()
  }, {
    v(mirai::mirai({ Sys.sleep(5); "slow" }))
    v(mirai::mirai("fast"))
    wait_for(function() identical(v(), "fast"), session)
    expect_equal(shiny::isolate(v()), "fast")
  })
})

test_that("rejected tasks surface on read", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    v <- reactiveValAsync()
  }, {
    v(mirai::mirai(stop("nope")))
    wait_for(function() status_async(v) == "error", session)
    expect_error(shiny::isolate(v()), "nope")
  })
})

test_that("reactiveValAsync reads are captured by reactiveAsync", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    v <- reactiveValAsync(3)
    r <- reactiveAsync(v() + 1)
    output$x <- shiny::renderText(r())
  }, {
    session$flushReact()
    wait_for(function() identical(peek(r), 4), session)
    expect_equal(peek(r), 4)
  })
})

test_that("the label is the name the value is assigned to", {
  env <- new.env()
  eval(parse(text = "report <- reactiveValAsync()\nx = reactiveValAsync(1)", keep.source = TRUE), env)
  expect_equal(attr(env$report, "shinyasync")$label, "report")
  expect_equal(attr(env$x, "shinyasync")$label, "x")
  eval(parse(text = "plain <- reactiveValAsync()", keep.source = FALSE), env)
  expect_match(attr(env$plain, "shinyasync")$label, "^reactiveValAsync#[0-9]+$")
  named <- reactiveValAsync(label = "mine")
  expect_equal(attr(named, "shinyasync")$label, "mine")
})
