library(shiny)
library(shinyasync)

# Bootstrap the slope of mpg on a chosen predictor. A few thousand model fits
# take seconds, long enough to block every user of a plain reactive().
ui <- fluidPage(
  selectInput("x", "Predictor of mpg", c("wt", "hp", "disp", "qsec")),
  sliderInput("B", "Bootstrap resamples", 500, 5000, 2000, step = 500),
  plotOutput("slopes"),
  textOutput("status")
)

server <- function(input, output, session) {
  slopes <- reactiveAsync({
    f <- reformulate(input$x, "mpg")    # runs in a worker, not the session
    replicate(input$B, {
      resample <- mtcars[sample(nrow(mtcars), replace = TRUE), ]
      coef(lm(f, resample))[[2]]
    })
  })

  output$slopes <- renderPlot(hist(slopes(), main = "Bootstrap slopes", xlab = "Slope"))
  output$status <- renderText(status_async(slopes))
}

shinyApp(ui, server)
