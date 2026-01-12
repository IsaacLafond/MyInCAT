# -------------------------
# Histogram UI
# -------------------------
mod_histogram_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Input: Slider for the number of bins ----
    sliderInput(
      ns("bins"),
      "Number of bins:",
      min = 1,
      max = 50,
      value = 30
    ),
    # Output: Histogram ----
    plotOutput(ns("distPlot"))
  )
}

# -------------------------
# Histogram server
# -------------------------
mod_histogram_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Histogram of the Old Faithful Geyser Data ----
    # with requested number of bins
    # This expression that generates a histogram is wrapped in a call
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    output$distPlot <- renderPlot({
      x <- faithful$waiting
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      hist(
        x,
        breaks = bins,
        col = "#007bc2",
        border = "white",
        xlab = "Waiting time to next eruption (mins)",
        main = "Histogram of waiting times"
      )
    })

  })
}
