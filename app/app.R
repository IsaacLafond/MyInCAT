library(shiny)
library(bslib)
library(colourpicker)

# Hist Demo App modules
source("modules/mod_histogram.R")

# UI app modules
source("modules/mod_subset_sidebar.R")
source("modules/mod_umap.R")

# UI components
source("ui/home_ui.R")
source("ui/coming_soon.R")
source("ui/color_picker.R")
source("ui/umap_code.R")

# Set static resource path to www directory
addResourcePath(prefix = "www", directoryPath = "www")


ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    # preset = "minty"
  ),

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

    sidebar = sidebar(
      title = "Options",
      position = "right",
      fillable = TRUE,
      fill = TRUE,
      # content:
      mod_subset_sidebar_ui("subset_sidebar")
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
  sidebar_data <- mod_subset_sidebar_server("subset_sidebar")
  mod_histogram_server("hist")
  mod_umap_server("umap", sidebar_data)
}

shinyApp(ui = ui, server = server)
