test_that("pool_async is shared across sessions and released by the last one", {
  skip_if(mirai::daemons_set(), "daemons already set")
  withr::defer(reset_pool())

  s1 <- shiny::MockShinySession$new()
  s2 <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(s1, pool_async(workers = 1))
  expect_true(mirai::daemons_set())
  shiny::withReactiveDomain(s2, pool_async(workers = 1))
  expect_equal(pool_state$refs, 2L)

  s1$close()
  expect_true(mirai::daemons_set())
  s2$close()
  expect_false(mirai::daemons_set())
  expect_false(pool_state$owned)
})

test_that("pool_async leaves user-managed daemons alone", {
  local_pool(1)
  s <- shiny::MockShinySession$new()
  expect_message(res <- shiny::withReactiveDomain(s, pool_async(workers = 3)), "already set")
  expect_false(res)
  s$close()
  expect_true(mirai::daemons_set())
})

test_that("pool_async restarts a pool that was torn down externally", {
  skip_if(mirai::daemons_set(), "daemons already set")
  withr::defer(reset_pool())

  s1 <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(s1, pool_async(workers = 1))
  mirai::daemons(0)

  s2 <- shiny::MockShinySession$new()
  expect_true(shiny::withReactiveDomain(s2, pool_async(workers = 1)))
  expect_true(mirai::daemons_set())
  expect_equal(pool_state$refs, 2L)
  s2$close()
  expect_true(mirai::daemons_set())
  s1$close()
  expect_false(mirai::daemons_set())
})

test_that("the first task starts a default pool held by its session", {
  skip_if(mirai::daemons_set(), "daemons already set")
  withr::defer(reset_pool())
  local_mocked_bindings(default_workers = function() 1L)

  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync(input$n + 1)
    output$x <- shiny::renderText(r())
  }, {
    # setInputs() flushes, which runs the first task.
    expect_message(session$setInputs(n = 1), "starting a pool.*top level of `app.R`")
    expect_true(mirai::daemons_set())
    expect_true(pool_state$owned)
    expect_equal(pool_state$refs, 1L)
    wait_for(function() identical(peek(r), 2), session)
    session$close()
    expect_false(mirai::daemons_set())
  })
})

test_that("every session that runs a task holds the default pool", {
  skip_if(mirai::daemons_set(), "daemons already set")
  withr::defer(reset_pool())
  local_mocked_bindings(default_workers = function() 1L)

  run_task <- function(s) {
    shiny::withReactiveDomain(s, {
      # Dispatched from inside a module, as most tasks are.
      m <- shiny::moduleServer("m", function(input, output, session) {
        r <- reactiveAsync(Sys.getpid())
        shiny::observe(r())
        r
      })
      s$flushReact()
      m
    })
  }
  s1 <- shiny::MockShinySession$new()
  s2 <- shiny::MockShinySession$new()
  r1 <- run_task(s1)
  r2 <- run_task(s2)
  expect_equal(pool_state$refs, 2L)
  expect_true(isTRUE(s2$userData$shinyasync_holds_pool))
  wait_for(function() status_async(r1) == "success" && status_async(r2) == "success", s2)

  s1$close()
  expect_true(mirai::daemons_set())
  expect_equal(pool_state$refs, 1L)
  s2$close()
  expect_false(mirai::daemons_set())
})

test_that("an explicit pool_async() in server() and the first task hold the pool once", {
  skip_if(mirai::daemons_set(), "daemons already set")
  withr::defer(reset_pool())

  shiny::testServer(function(input, output, session) {
    pool_async(workers = 1)
    r <- reactiveAsync(input$n + 1)
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 1)
    wait_for(function() identical(peek(r), 2), session)
    expect_equal(pool_state$refs, 1L)
    session$close()
    expect_false(mirai::daemons_set())
  })
})

test_that("a task on a named compute profile does not start the default pool", {
  skip_if(mirai::daemons_set(), "daemons already set")
  mirai::daemons(1, dispatcher = TRUE, .compute = "other")
  withr::defer(mirai::daemons(0, .compute = "other"))

  m <- mirai_async(Sys.getpid(), .compute = "other")
  expect_false(mirai::daemons_set())
  expect_false(pool_state$owned)
  expect_type(m[], "integer")
})

test_that("everywhere preloads every worker and resident names are not shipped", {
  skip_if(mirai::daemons_set(), "daemons already set")
  withr::defer(reset_pool())
  fx <- 10
  sales <- "session copy"
  pool_async(workers = 2, everywhere = {
    rate <- fx * 2
    sales <- "worker copy"
  })
  expect_setequal(pool_state$preload$residents, c("rate", "sales", "fx"))

  seen <- unlist(lapply(1:4, function(i) mirai_async({ Sys.sleep(0.1); paste(rate, sales, fx) })[]))
  expect_equal(seen, rep("20 worker copy 10", 4))

  d <- new_capture(quote(summary(rate, sales, fx)), environment())$describe()
  expect_setequal(d$resident, c("rate", "sales", "fx"))
  expect_length(d$shipped, 0)
})

test_that("a resident reached through a shipped helper is listed, checked and not shipped", {
  skip_if(mirai::daemons_set(), "daemons already set")
  withr::defer(reset_pool())
  om_base <- "session copy"
  om_get <- function(path) paste(om_base, path)
  fetch_marine <- function(spot) om_get(spot)
  pool_async(workers = 1, everywhere = om_base <- "worker copy")

  capture <- new_capture(quote(fetch_marine("muizenberg")), environment())
  d <- capture$describe()
  expect_equal(d$resident, "om_base")
  expect_equal(names(d$shipped), "fetch_marine")
  helper_env <- environment(d$shipped$fetch_marine)
  expect_true(exists("om_get", envir = helper_env, inherits = FALSE))
  expect_false(exists("om_base", envir = helper_env, inherits = FALSE))

  job <- capture$resolve()
  expect_match(paste(deparse(job$expr), collapse = " "), "om_base.*was not preloaded")
  expect_equal(mirai_async(fetch_marine("muizenberg"))[], "worker copy muizenberg")
})

test_that("a restarted pool is preloaded again", {
  skip_if(mirai::daemons_set(), "daemons already set")
  withr::defer(reset_pool())
  s1 <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(s1, pool_async(workers = 1, everywhere = rate <- 2))
  expect_equal(mirai_async(rate)[], 2)
  mirai::daemons(0)

  s2 <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(s2, pool_async(workers = 1))
  expect_equal(mirai_async(rate)[], 2)
})

test_that("a .Last function in everywhere runs when the pool shuts down", {
  skip_if(mirai::daemons_set(), "daemons already set")
  withr::defer(reset_pool())
  marker <- withr::local_tempfile()
  s <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(s, pool_async(workers = 1, everywhere = {
    con <- "a connection"
    .Last <- function() writeLines(con, marker)
  }))
  expect_true(".Last" %in% pool_state$preload$residents)
  expect_equal(shiny::withReactiveDomain(s, mirai_async(con))[], "a connection")
  expect_equal(pool_state$refs, 1L)

  s$close()
  expect_false(mirai::daemons_set())
  start <- Sys.time()
  while (!file.exists(marker) && difftime(Sys.time(), start, units = "secs") < 10) Sys.sleep(0.05)
  expect_equal(readLines(marker), "a connection")
})

test_that("everywhere is loaded on user-managed daemons when asked", {
  local_pool(1)
  withr::defer(pool_state$preload <- NULL)
  expect_message(res <- pool_async(everywhere = rate <- 3), "already set")
  expect_false(res)
  expect_equal(mirai_async(rate)[], 3)
})

test_that("a second everywhere is refused with a warning and the same one is accepted", {
  skip_if(mirai::daemons_set(), "daemons already set")
  withr::defer(reset_pool())
  pool_async(workers = 1, everywhere = rate <- 1)
  expect_no_warning(pool_async(workers = 1, everywhere = rate <- 1))
  expect_warning(pool_async(workers = 1, everywhere = rate <- 2), "different `everywhere`")
  expect_equal(mirai_async(rate)[], 1)
})

test_that("a failing preload is reported", {
  skip_if(mirai::daemons_set(), "daemons already set")
  withr::defer(reset_pool())
  expect_message(
    {
      pool_async(workers = 1, everywhere = stop("no such file"))
      for (i in 1:50) later::run_now(0.1)
    },
    "preload failed.*no such file"
  )
})

test_that("a task checks for resident names before running", {
  e <- worker_expr(quote(summary(sales)), character(), resident = "sales")
  expect_match(paste(deparse(e), collapse = " "), "was not preloaded on this worker")
  expect_identical(e[[length(e)]], quote(summary(sales)))
  expect_identical(worker_expr(quote(1), character()), quote({ 1 }))
})
