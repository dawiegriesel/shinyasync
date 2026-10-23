test_that("observeAsync runs side effects in a worker", {
  local_pool()
  path <- withr::local_tempfile()
  shiny::testServer(function(input, output, session) {
    o <- observeAsync(writeLines(as.character(input$n), path))
  }, {
    session$setInputs(n = 7)
    wait_for(function() o$status() == "success", session)
    expect_equal(readLines(path), "7")
    expect_equal(status_async(o), "success")
  })
})

test_that("observeAsync handle can cancel and destroy", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    o <- observeAsync({
      Sys.sleep(5)
      input$n
    })
  }, {
    session$setInputs(n = 1)
    expect_equal(o$status(), "running")
    o$cancel()
    expect_equal(o$status(), "idle")
    o$destroy()
    session$setInputs(n = 2)
    expect_equal(o$status(), "idle")
  })
})

test_that("observeAsync reports worker and session-side errors", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    o <- observeAsync(stop(paste("boom", input$n)), label = "audit")
  }, {
    expect_message(
      {
        session$setInputs(n = 1)
        wait_for(function() o$status() == "error", session)
      },
      "Error in audit: boom 1"
    )
  })

  shiny::testServer(function(input, output, session) {
    o <- observeAsync(no_such_function_xyz(input$n), label = "typo")
  }, {
    expect_message(
      {
        expect_warning(session$setInputs(n = 1), "no_such_function_xyz\\(\\)")
        wait_for(function() o$status() == "error", session)
      },
      "Error in typo: .*no_such_function_xyz"
    )
  })
})
