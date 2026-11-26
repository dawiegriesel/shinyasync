library(shiny)
library(shinyasync)

# A module that bootstraps the slope of a response on a chosen predictor,
# mounted twice on different data. Everything inside the module server is
# ordinary Shiny; reactiveAsync() captures the namespaced `input` and the
# module's arguments and local functions like any other reactive.
bootUI <- function(id, dataset, response) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x"), paste("Predictor of", response), setdiff(names(dataset), response)),
    sliderInput(ns("B"), "Bootstrap resamples", 500, 5000, 2000, step = 500),
    plotOutput(ns("slopes")),
    textOutput(ns("status"))
  )
}

bootServer <- function(id, dataset, response) {
  moduleServer(id, function(input, output, session) {
    slope <- function(d, x) coef(lm(reformulate(x, response), d))[[2]]   # shipped to the worker

    slopes <- reactiveAsync(
      replicate(input$B, slope(dataset[sample(nrow(dataset), replace = TRUE), ], input$x))
    )

    output$slopes <- renderPlot(hist(slopes(), main = session$ns(""), xlab = "Slope"))
    output$status <- renderText(status_async(slopes))

    slopes   # return the async reactive so the caller can use it
  })
}

pool_async(workers = 2)   # optional: sizes the pool for the whole app

ui <- fluidPage(
  fluidRow(
    column(6, bootUI("cars", mtcars, "mpg")),
    column(6, bootUI("air", airquality, "Ozone"))
  ),
  textOutput("both")
)

server <- function(input, output, session) {
  cars <- bootServer("cars", mtcars, "mpg")
  air  <- bootServer("air", airquality, "Ozone")

  output$both <- renderText(
    sprintf("Median slopes: %.3f and %.3f", median(cars()), median(air()))
  )
}

shinyApp(ui, server)
