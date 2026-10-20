# bslib's bind_task_button() has one method, for ExtendedTask. These give the
# async objects the same treatment, driven by the task status, so a button
# that fires a reactiveAsync() disables itself while the task runs.

#' Bind a task button to an async reactive
#'
#' Methods for [bslib::bind_task_button()] so that a [bslib::input_task_button()]
#' shows its busy state while the task of a [reactiveAsync()], [observeAsync()]
#' or [reactiveValAsync()] is `"running"` and returns to ready when it settles
#' or is cancelled, as it does for an `ExtendedTask`. Reading the status
#' starts a [reactiveAsync()] that has not run yet, as any status read does.
#'
#' @param target An object created by [reactiveAsync()], [observeAsync()] or
#'   [reactiveValAsync()].
#' @param task_button_id The id of the [bslib::input_task_button()].
#' @param ... Ignored.
#' @param session The Shiny session.
#'
#' @return `target`, invisibly, so the call can be piped like `bindEvent()`.
#' @name bind_task_button
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bslib)
#'   ui <- page_fluid(
#'     selectInput("spot", "Spot", c("Muizenberg", "Kommetjie")),
#'     input_task_button("go", "Simulate"),
#'     plotOutput("band")
#'   )
#'   server <- function(input, output, session) {
#'     sim <- reactiveAsync(simulate_band(input$spot)) |>
#'       bindEvent(input$go) |>
#'       bind_task_button("go")
#'     output$band <- renderPlot(plot(sim()))
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
  get_task(target, "bind_task_button")
  force(task_button_id)
  was_running <- FALSE
  shiny::observe(
    {
      running <- status_async(target) == "running"
      if (running != was_running) {
        was_running <<- running
        bslib::update_task_button(
          task_button_id,
          state = if (running) "busy" else "ready",
          session = session
        )
      }
    },
    label = "update_task_button_state",
    priority = 1000,
    domain = session
  )
  invisible(target)
}
