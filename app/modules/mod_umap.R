# -------------------------
# UMAP UI
# -------------------------
mod_umap_ui <- function(id) {
  ns <- NS(id)

  page_fluid(

    verbatimTextOutput(ns("output")),

    # Output: UMAP
    plotOutput(
      ns("umap_plot"),
      width = "100%",
      height = "66vh"
    ) %>% with_custom_spinner(),

    # Relevant code
    accordion(
      id = ns("umap_code_accordion"),
      accordion_panel(
        title = "Meta Table",
        # Output: Metadata table
        tableOutput(ns("meta_table")) %>% with_custom_spinner()
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
mod_umap_server <- function(id, global_state) {
  moduleServer(id, function(input, output, session) {
    # output$output <- renderPrint({
    #   str(sidebar_selections())
    # })

    # Populate placeholder UMAP plot
    output$umap_plot <- renderPlot({
      req(global_state())

      state <- global_state()

      DimPlot(
        state$sc_subset,
        reduction = "umap",
        group.by = state$group_by,
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
        req(global_state())

        state <- global_state()

        state$sc_subset@meta.data %>%
          group_by(experiment, orig.ident, seurat_clusters, subcluster) %>%
          summarise(n_cells = n(), .groups = "drop") %>%
          arrange(experiment, orig.ident, seurat_clusters)

          # # Optimization: Use data.table or fast dplyr for the summary
          # sc_object()@meta.data %>%
          #   count(experiment, orig.ident, seurat_clusters, subcluster) %>%
          #   arrange(experiment, orig.ident, seurat_clusters)
          # ?????????????????
      }
    )

  })
}