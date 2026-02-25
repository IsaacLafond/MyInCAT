library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bslib)
library(colourpicker)
library(Seurat)
library(ggplot2)
library(dplyr)

# Utils
source("utils/choices.R")

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

# -------------------------

# load data
sc_combined_tier1 <- readRDS("data/sc_combined_tier1.rds")

# create subset tree
tree_data <- sc_combined_tier1@meta.data %>%
  select(experiment, orig.ident, seurat_clusters, subcluster) %>%
  distinct() %>%
  # Convert factors to character to avoid levels showing up where they don't exist
  mutate(across(everything(), as.character)) %>%
  # Arrange them so the tree looks organized
  arrange(experiment, seurat_clusters, subcluster)


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
      mod_subset_sidebar_ui("subset_sidebar",
        # unique(sc_combined_tier1$seurat_clusters),
        # unique(sc_combined_tier1$subcluster),
        # unique(sc_combined_tier1$orig.ident),
        # unique(sc_combined_tier1$experiment),
        tree_data
      )
    ),

    # Home tab
    nav_panel(
      title = "Home",
      home_ui()
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
  sc_subset <- reactive({
    req(sidebar_data())

    selected_options <- sidebar_data()

    subset(
      sc_combined_tier1,
      subset =  experiment %in% selected_options$experiments &
                orig.ident %in% selected_options$samples &
                seurat_clusters %in% selected_options$clusters &
                subcluster %in% selected_options$subclusters
    )
  })

  sidebar_data <- mod_subset_sidebar_server("subset_sidebar", tree_data)
  mod_umap_server("umap", sidebar_data, sc_subset)
}

shinyApp(ui = ui, server = server)
