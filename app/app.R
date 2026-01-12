library(shiny)
library(bslib)

source("modules/mod_home.R")
source("modules/mod_histogram.R")

addResourcePath(prefix = "www", directoryPath = "www")

ui <- tagList(
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "www/logo.png")
  ),
  page_navbar(
    title = "MyInCAT",
    # title = tags$div(
    #   tags$img(src = "www/logo.png", height = "30px"), # nolint
    #   "MyInCAT",
    #   style = "display: inline-flex; align-items: center; gap: 10px;" # nolint
    # ), # working image/name header but is styled weird (not centered vertically) # nolint
    nav_panel("Home", mod_home_ui("home")),
    nav_panel("Hist", mod_histogram_ui("hist"))
  )
)

server <- function(input, output, session) {
  mod_home_server("home") # nolint
  mod_histogram_server("hist") # nolint
}

shinyApp(ui = ui, server = server)
