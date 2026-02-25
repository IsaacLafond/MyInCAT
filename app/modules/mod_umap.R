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
mod_umap_server <- function(id, sidebar_selections, sc_object) {
  moduleServer(id, function(input, output, session) {
    output$output <- renderPrint({
      str(sidebar_selections())
    })

    # Populate placeholder UMAP plot
    output$umapPlot <- renderPlot({
      selected_options <- sidebar_selections()
      group_val <- groupby_choices[[selected_options$group_by]]

      DimPlot(
        sc_object(),
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
        sc_object()@meta.data %>%
          group_by(experiment, orig.ident, seurat_clusters, subcluster) %>%
          summarise(n_cells = n(), .groups = "drop") %>%
          arrange(experiment, orig.ident, seurat_clusters)
      }
    )

  })
}