# -------------------------
# UMAP UI
# -------------------------
mod_umap_ui <- function(id) {
  ns <- NS(id)

  page_fluid(

    verbatimTextOutput(ns("output")),

    # Output: UMAP
    plotOutput(
      ns("umapPlot"),
      width = "100%",
      height = "66vh"
    ),

    # Relevant code
    accordion(
      id = ns("umap_code_accordion"),
      accordion_panel(
        title = "Meta Table",
        # Output: Metadata table
        tableOutput(ns("meta_table"))
      ),
      accordion_panel(
        title = "Relevant Code",
        umap_code()
      ),
      open = FALSE,
      class = "mb-5"
    )
  )
}


# -------------------------
# UMAP server
# -------------------------
mod_umap_server <- function(id, sidebar_selections, sc_combined_tier1) {
  moduleServer(id, function(input, output, session) {
    output$output <- renderPrint({
      str(sidebar_selections())
    })

    meta_table <- unique(sc_combined_tier1@meta.data[, c("experiment", "orig.ident", "seurat_clusters", "subcluster")])

    # Populate placeholder UMAP plot
    output$umapPlot <- renderPlot({
      selected_options <- sidebar_selections()
      group_val <- groupby_choices[[selected_options$group_by]]

      DimPlot(
        # sc_combined_tier1,
        subset(
          sc_combined_tier1,
          subset = experiment %in% selected_options$experiments &
                   orig.ident %in% selected_options$samples &
                   seurat_clusters %in% selected_options$clusters &
                   subcluster %in% selected_options$subclusters
        ),
        reduction = "umap",
        group.by = group_val,
        repel = TRUE,
        pt.size = 1
      ) +
      ggplot2::labs(title = "", x = "UMAP1", y = "UMAP2") +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
      )

    })

    # Populate placeholder metadata table
    output$meta_table <- renderTable(
      width = "100%",
      striped = TRUE,
      hover = TRUE,
      bordered = TRUE,
      # content:
      {
        meta_table
      }
    )

  })
}