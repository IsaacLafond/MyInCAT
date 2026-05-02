# UI
library(shiny)
library(bslib)
# library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(colourpicker)
library(DT)
# Analysis
library(Seurat)
library(BPCells)
library(clusterProfiler)
library(org.Mm.eg.db)
library(monocle3)
library(CellChat)
library(ggplot2)
library(dplyr)

# Utils
source("utils/umap_code.R")
source("utils/deg_code.R")
source("utils/choices.R")

# App modules
source("modules/mod_subset_sidebar.R")
source("modules/mod_umap.R")
source("modules/mod_deg_tab.R")
source("modules/mod_deg.R")
source("modules/mod_deg_go.R")
source("modules/mod_deg_kegg.R")
source("modules/mod_deg_plots.R")
source("modules/mod_dot_plot.R")
source("modules/mod_vln_plot.R")
source("modules/mod_box_plot.R")
source("modules/mod_pseudotime_plot.R")
source("modules/mod_cellchat_tab.R")
source("modules/mod_cellchat_enrichedpathways.R")
source("modules/mod_cellchat_cellinteractions.R")
source("modules/mod_cellchat_plots.R")

# UI components
source("ui/home_ui.R")
source("ui/custom_spinner.R")
source("ui/coming_soon.R")
source("ui/color_picker.R")
source("ui/code_block.R")

# Set static resource path to www directory
addResourcePath(prefix = "www", directoryPath = "www")

# -------------------------

# load data
sc_combined <- readRDS("data/sc_combined_shell.rds")
sc_combined[["RNA"]]$counts <- open_matrix_dir("data/RNA_counts")
sc_combined[["RNA"]]$data <- open_matrix_dir("data/RNA_data")

# create subset tree
tree_data <- sc_combined@meta.data %>%
  select(experiment, orig.ident, seurat_clusters, subcluster) %>%
  distinct() %>%
  # Arrange them to maintain consistency (arrange before string so follow factor order)
  arrange(experiment, orig.ident, seurat_clusters) %>%
  # Convert factors to character
  mutate(across(everything(), as.character))

# load all features (genes) for virtual multiselect
rna_features <- readRDS("data/rna_features.rds")


ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    # preset = "minty"
  ),

  page_navbar(
    id = "main_navbar",
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
      width = 300,
      fillable = TRUE,
      fill = TRUE,
      # content:
      mod_subset_sidebar_ui("subset_sidebar", tree_data)
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
      mod_deg_tab_ui("deg_tab", rna_features)
    ),

    # CellChat tab
    nav_panel(
      title = "CellChat",
      mod_cellchat_tab_ui("cellchat_tab")
    )
  )
)

server <- function(input, output, session) {
  current_tab <- reactive({
    input$main_navbar
  })

  sidebar_data <- mod_subset_sidebar_server("subset_sidebar", current_tab, list(
    experiment = levels(sc_combined$experiment),
    orig.ident = levels(sc_combined$orig.ident),
    seurat_clusters = levels(sc_combined$seurat_clusters),
    subcluster = levels(sc_combined$subcluster)
  ))

  global_state <- reactive({
    req(sidebar_data())
    selected_options <- sidebar_data()

    groupby_choices <- c(
      # "None" = NULL,
      "Experiment" = "experiment",
      "Sample" = "orig.ident",
      "Cluster" = "seurat_clusters",
      "Subcluster" = "subcluster"
    )

    list(
      group_by = groupby_choices[[selected_options$group_by]],
      experiment = selected_options$experiments,
      orig.ident = selected_options$orig.ident,
      seurat_clusters = selected_options$clusters,
      subcluster = selected_options$subclusters,
      sc_subset = subset(
        sc_combined,
        subset =  experiment %in% selected_options$experiments &
                  orig.ident %in% selected_options$orig.ident &
                  seurat_clusters %in% selected_options$clusters &
                  subcluster %in% selected_options$subclusters
      )
    )
  })

  mod_umap_server("umap", global_state)
  mod_deg_tab_server("deg_tab", global_state)
  mod_deg_plots_server("deg_plots", global_state)
  mod_cellchat_tab_server("cellchat_tab")

}

shinyApp(ui = ui, server = server)
