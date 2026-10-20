# bslib's update_task_button() sends an input message, which the mock session
# does not record, so the calls are captured by mocking it in bslib.
local_button_log <- function(env = parent.frame()) {
  log <- new.env()
  log$states <- character()
  testthat::local_mocked_bindings(
    update_task_button = function(id, ..., state = NULL, session = NULL) {
      log$states <- c(log$states, paste(id, state))
    },
    .package = "bslib",
    .env = env
  )
  log
}

test_that("a bound button is busy while the task runs and ready when it settles", {
  skip_if_not_installed("bslib", "0.7.0")
  local_pool()
  log <- local_button_log()
  shiny::testServer(function(input, output, session) {
    sim <- reactiveAsync({
      Sys.sleep(1)
      input$n * 2
    }) |> bindEvent(input$go) |> bslib::bind_task_button("go")
    output$x <- shiny::renderText(sim())
  }, {
    session$setInputs(n = 2)
    expect_equal(log$states, character())
    session$setInputs(go = 1)
    expect_equal(log$states, "go busy")
    wait_for(function() status_async(sim) == "success", session)
    expect_equal(log$states, c("go busy", "go ready"))
    expect_equal(output$x, "4")
  })
})

test_that("cancelling returns the button to ready", {
  skip_if_not_installed("bslib", "0.7.0")
  local_pool()
  log <- local_button_log()
  shiny::testServer(function(input, output, session) {
    sim <- reactiveAsync({
      Sys.sleep(5)
      input$n
    }) |> bindEvent(input$go) |> bslib::bind_task_button("go")
    output$x <- shiny::renderText(sim())
  }, {
    session$setInputs(n = 1, go = 1)
    expect_equal(log$states, "go busy")
    cancel_async(sim)
    session$flushReact()
    expect_equal(log$states, c("go busy", "go ready"))
  })
})

test_that("the methods return the object and cover every async class", {
  skip_if_not_installed("bslib", "0.7.0")
  local_pool()
  log <- local_button_log()
  shiny::testServer(function(input, output, session) {
    o <- observeAsync({ Sys.sleep(1); input$n }) |> bslib::bind_task_button("obs")
    v <- reactiveValAsync() |> bslib::bind_task_button("val")
    observe(v(mirai_async({ Sys.sleep(1); 1 }))) |> bindEvent(input$set)
  }, {
    expect_s3_class(o, "observeAsync")
    expect_s3_class(v, "reactiveValAsync")
    session$setInputs(n = 1, set = 1)
    expect_setequal(log$states, c("obs busy", "val busy"))
    wait_for(function() status_async(o) == "success" && status_async(v) == "success", session)
    expect_setequal(log$states, c("obs busy", "val busy", "obs ready", "val ready"))
  })
})

test_that("binding a button does not start an unread reactiveAsync()", {
  skip_if_not_installed("bslib", "0.7.0")
  local_pool()
  log <- local_button_log()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync(input$n) |> bslib::bind_task_button("go")
  }, {
    session$setInputs(n = 1)
    expect_equal(log$states, character())
    expect_equal(attr(r, "shinyasync")$current_status(), "idle")
  })
})

test_that("binding outside a session fails at the call", {
  skip_if_not_installed("bslib", "0.7.0")
  r <- reactiveAsync(1)
  expect_error(bslib::bind_task_button(r, "go"), "within a Shiny session")
})
