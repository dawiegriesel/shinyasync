local_plain_cli <- function(env = parent.frame()) {
  withr::local_options(cli.num_colors = 1L, cli.unicode = FALSE, .local_envir = env)
}

test_that("print() lists hoisted calls, shipped objects and the worker expression", {
  local_plain_cli()
  input <- shiny::reactiveValues(n = 5)
  data <- shiny::reactive(1:3)
  helper <- function(x) x * 2
  sd <- 1
  r <- reactiveAsync({
    shiny::req(input$n)
    helper(data()) + sd + stats::rnorm(1)
  }, globals = list(extra = letters))

  out <- capture.output(print(r))
  expect_match(out[1], "<reactiveAsync> reactiveAsync\\(")
  expect_match(out[2], "^  status   initial$")
  expect_true(any(grepl(".sa_1 shiny::req(input$n), .sa_2 data()", out, fixed = TRUE)))
  expect_true(any(grepl("helper(), sd <numeric>, extra <character> (globals)", out, fixed = TRUE)))
  expect_true(any(grepl("helper(.sa_2) + sd + stats::rnorm(1)", out, fixed = TRUE)))
  # Printing is not a read: the task has not started.
  expect_equal(attr(r, "shinyasync")$state, "initial")
})

test_that("print() reports a capture error instead of raising it", {
  local_plain_cli()
  input <- shiny::reactiveValues(n = 5)
  r <- reactiveAsync(helper(input$n))
  # A helper that cannot be shipped because it closes over a reactive object.
  helper <- function(x) x + input$n
  rv <- shiny::reactiveValues()
  out <- capture.output(print(r))
  expect_true(any(grepl("could not resolve", out)))

  r2 <- reactiveAsync(rv$x <- 1)
  out <- capture.output(print(r2))
  expect_true(any(grepl("assigns to reactive values", out)))
  expect_false(any(grepl("^  worker", out)))
})

test_that("print() lists functions the session cannot resolve", {
  local_plain_cli()
  input <- shiny::reactiveValues(n = 5)
  r <- reactiveAsync(no_such_fn(input$n))
  expect_silent(out <- capture.output(print(r)))
  expect_true(any(grepl("^  missing  no_such_fn\\(\\)", out)))
})

test_that("printing before a reactive is defined does not fix the analysis", {
  local_plain_cli()
  input <- shiny::reactiveValues(n = 5)
  r <- reactiveAsync(later_reactive() + input$n)
  task <- attr(r, "shinyasync")
  out <- capture.output(print(r))
  expect_false("later_reactive()" %in% task$capture$describe()$hoisted)

  later_reactive <- shiny::reactive(1)
  expect_true("later_reactive()" %in% task$capture$describe()$hoisted)
})

test_that("observeAsync and reactiveValAsync print their status", {
  local_plain_cli()
  input <- shiny::reactiveValues(n = 5)
  o <- observeAsync(message(input$n), suspended = TRUE)
  out <- capture.output(print(o))
  expect_match(out[1], "<observeAsync> observeAsync\\(")
  expect_true(any(grepl("input$n", out, fixed = TRUE)))

  v <- reactiveValAsync(1)
  out <- capture.output(print(v))
  expect_length(out, 2L)
  expect_match(out[1], "<reactiveValAsync> v ")
  expect_match(out[2], "^  status   initial$")
})

test_that("in devmode a task prints itself once, on its first run", {
  local_plain_cli()
  local_pool()
  withr::local_envvar(TESTTHAT = "")
  withr::local_options(shiny.devmode = TRUE)
  out <- capture.output(suppressMessages(
    shiny::testServer(function(input, output, session) {
      r <- reactiveAsync(input$n * 2)
      output$x <- shiny::renderText(r())
    }, {
      session$setInputs(n = 1)
      wait_for(function() identical(peek(r), 2), session)
      session$setInputs(n = 2)
      wait_for(function() identical(peek(r), 4), session)
    })
  ))
  expect_equal(sum(grepl("<reactiveAsync>", out)), 1L)
  expect_true(any(grepl("session  input$n", out, fixed = TRUE)))
  # The devmode form leaves the worker expression out.
  expect_false(any(grepl("worker", out)))

  withr::local_options(shinyasync.print = FALSE)
  out <- capture.output(
    shiny::testServer(function(input, output, session) {
      r <- reactiveAsync(input$n * 2)
      output$x <- shiny::renderText(r())
    }, {
      session$setInputs(n = 1)
      wait_for(function() identical(peek(r), 2), session)
    })
  )
  expect_false(any(grepl("<reactiveAsync>", out)))
})

test_that("print lists resident names on their own line", {
  local_plain_cli()
  withr::defer(pool_state$preload <- NULL)
  pool_state$preload <- list(residents = c("sales", "con"))
  input <- shiny::reactiveValues(region = "west")
  sales <- data.frame()
  r <- reactiveAsync(summarise_sales(sales, con, input$region))
  out <- capture.output(print(r))
  expect_true(any(grepl("^  resident sales, con$", out)))
  expect_false(any(grepl("sales <data.frame>", out, fixed = TRUE)))
})
