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

test_that("reactiveValAsync generates distinct default labels", {
  first <- reactiveValAsync()
  second <- reactiveValAsync()
  first_label <- attr(first, "shinyasync")$label
  second_label <- attr(second, "shinyasync")$label

  expect_match(first_label, "^reactiveValAsync")
  expect_false(identical(first_label, second_label))

  named <- reactiveValAsync(label = "report")
  expect_identical(attr(named, "shinyasync")$label, "report")
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
