#' Create an async reactive value
#'
#' Like [shiny::reactiveVal()], but the setter also accepts a `mirai` or a
#' promise. The value updates when the task resolves. Setting a new task while
#' one is running supersedes the old one: a running `mirai` is cancelled and a
#' promise's result is ignored. Use [mirai_async()] to start the task so that
#' helper functions and reactive reads are captured as in [reactiveAsync()].
#'
#' @param value The initial value.
#' @param label A label for debugging.
#'
#' @return A function of class `"reactiveValAsync"`. Call it with no arguments
#'   to read the value, or with one argument to set it. Errors from a failed
#'   task are re-thrown on read.
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   server <- function(input, output, session) {
#'     # Set from two places, so a reactiveVal rather than a reactive.
#'     report <- reactiveValAsync()
#'     observe({
#'       report(mirai_async(build_report(input$region, input$month)))
#'     }) |> bindEvent(input$generate)
#'     observe(report(NULL)) |> bindEvent(input$clear)
#'     output$report <- renderTable(report())
#'   }
#' }
reactiveValAsync <- function(value = NULL, label = NULL) {
  label <- label %||% assign_label(sys.calls(), "reactiveValAsync")
  task <- new_async_task("latest", label)
  task$value(list(value))

  rv <- function(x) {
    if (missing(x)) {
      if (!task$has_value()) stop(task$error())
      return(task$value()[[1]])
    }
    if (inherits(x, "mirai") || promises::is.promising(x)) {
      task$adopt(x)
    } else {
      task$set(x)
    }
    # Returning a mirai would make a calling observe() async in shiny's eyes.
    invisible(NULL)
  }

  structure(rv, class = c("reactiveValAsync", "reactive", "function"), shinyasync = task)
}
