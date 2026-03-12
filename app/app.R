# UI
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bslib)
library(colourpicker)
# Analysis
library(Seurat)
library(BPCells)
library(ggplot2)
library(dplyr)

# App modules
source("modules/mod_home.R")

# UI components
source("ui/coming_soon.R")

# Set static resource path to www directory
addResourcePath(prefix = "www", directoryPath = "www")

# -------------------------

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

    # Home tab
    nav_panel(
      title = "Home",
      mod_home_ui("home")
    ),

    # UMAP tab
    nav_panel(
      title = "UMAP",
      coming_soon() # TODO
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
  mod_home_server("home")
}

shinyApp(ui = ui, server = server)
