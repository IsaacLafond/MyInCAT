library(shiny)
library(bslib)

# Hist Demo App modules
source("modules/mod_histogram.R")

# UI app modules
source("modules/mod_umap.R")

# UI components
source("ui/home_ui.R")
source("ui/coming_soon.R")
source("ui/umap_code.R")

# Set static resource path to www directory
addResourcePath(prefix = "www", directoryPath = "www")


ui <- page_fillable(
  # theme = ___

  page_navbar(
    window_title = "MyInCAT",

    # Head with custom CSS and favicon
    header = tags$head(
      tags$link(rel = "icon", type = "image/png", href = "www/logo.png"),
      tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
    ),

    # Navbar title as logo link to home
    title = tags$a(
      href = "/",
      tags$img(src = "www/logo.png", height = "30px")
    ),

    # Home tab
    nav_panel(
      title = "Home",
      home_ui()
    ),

    # hist demo tab
    nav_panel(
      title = "Hist",
      mod_histogram_ui("hist")
    ),

    # UMAP tab
    nav_panel(
      title = "UMAP",
      mod_umap_ui("umap")
    ),

    # DEGs tab
    nav_panel(
      title = "DEGs",
      coming_soon() # TODO
    ),

    # CellChat tab
    nav_panel(
      title = "CellChat",
      coming_soon() # TODO
    )
  )
)

server <- function(input, output, session) {
  # mod_home_server("home") # nolint
  mod_histogram_server("hist") # nolint
  mod_umap_server("umap") # nolint
}

shinyApp(ui = ui, server = server)
