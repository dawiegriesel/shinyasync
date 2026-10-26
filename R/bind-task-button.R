# bslib's bind_task_button() has one method, for ExtendedTask. These give the
# async objects the same treatment, driven by the task status, so a button
# that fires a reactiveAsync() disables itself while the task runs.

#' Bind a task button to an async reactive
#'
#' Methods for [bslib::bind_task_button()] so that a [bslib::input_task_button()]
#' shows its busy state while the task of a [reactiveAsync()], [observeAsync()]
#' or [reactiveValAsync()] is `"running"` and returns to ready when it settles,
#' as it does for an `ExtendedTask`. It also returns to ready when the task is
#' cancelled with [cancel_async()]. Binding does not start the task: a
#' [reactiveAsync()] still waits for its first read, as described in its
#' Details.
#'
#' @param target An object created by [reactiveAsync()], [observeAsync()] or
#'   [reactiveValAsync()].
#' @param task_button_id The id of the [bslib::input_task_button()].
#' @param ... Ignored.
#' @param session The Shiny session, by default the current reactive domain.
#'   Binding outside a session is an error at the call.
#'
#' @return `target`, as [bslib::bind_task_button()] returns an `ExtendedTask`.
#' @name bind_task_button
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bslib)
#'   ui <- page_fluid(
#'     selectInput("spot", "Spot", c("Muizenberg", "Kommetjie")),
#'     input_task_button("go", "Simulate"),
#'     actionButton("cancel", "Cancel"),
#'     plotOutput("band")
#'   )
#'   server <- function(input, output, session) {
#'     marine <- reactiveAsync(fetch_marine(input$spot))
#'     wind <- reactiveAsync(fetch_wind(input$spot))
#'     sim <- reactiveAsync(simulate_band(input$spot, marine(), wind())) |>
#'       bindEvent(input$go) |>
#'       bind_task_button("go")
#'     observe(cancel_async(sim)) |> bindEvent(input$cancel)
#'     output$band <- renderPlot(plot_band(sim()))
#'   }
#'   shinyApp(ui, server)
#' }
NULL

#' @rdname bind_task_button
#' @exportS3Method bslib::bind_task_button
bind_task_button.reactiveAsync <- function(target, task_button_id, ...,
                                           session = shiny::getDefaultReactiveDomain()) {
  bind_task_button_async(target, task_button_id, session)
}

#' @rdname bind_task_button
#' @exportS3Method bslib::bind_task_button
bind_task_button.observeAsync <- function(target, task_button_id, ...,
                                          session = shiny::getDefaultReactiveDomain()) {
  bind_task_button_async(target, task_button_id, session)
}

#' @rdname bind_task_button
#' @exportS3Method bslib::bind_task_button
bind_task_button.reactiveValAsync <- function(target, task_button_id, ...,
                                              session = shiny::getDefaultReactiveDomain()) {
  bind_task_button_async(target, task_button_id, session)
}

bind_task_button_async <- function(target, task_button_id, session) {
  task <- get_task(target, "bind_task_button")
  force(task_button_id)
  if (is.null(session)) {
    stop("`bind_task_button()` must be called from within a Shiny session.", call. = FALSE)
  }
  was_running <- FALSE
  shiny::observe(
    {
      running <- task$current_status() == "running"
      if (running != was_running) {
        was_running <<- running
        bslib::update_task_button(
          task_button_id,
          state = if (running) "busy" else "ready",
          session = session
        )
      }
    },
    label = paste0(task$label, "$button"),
    priority = 1000,
    domain = session
  )
  target
}
