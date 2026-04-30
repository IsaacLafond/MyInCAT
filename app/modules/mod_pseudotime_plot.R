# -------------------------
# Pseudotime Plot UI
# -------------------------
mod_pseudotime_plot_ui <- function(id) {
  ns <- NS(id)

  page_fluid(
    coming_soon() # TODO
  )
}


# -------------------------
# Pseudotime Plot server
# -------------------------
mod_pseudotime_plot_server <- function(id, global_state) {
  moduleServer(id, function(input, output, session) {


#     # Populate placeholder metadata table
#     output$meta_table <- renderTable(
#       width = "100%",
#       striped = TRUE,
#       hover = TRUE,
#       bordered = TRUE,
#       # content:
#       {
#         req(sc_object())

#         sc_object()@meta.data %>%
#           group_by(experiment, orig.ident, seurat_clusters, subcluster) %>%
#           summarise(n_cells = n(), .groups = "drop") %>%
#           arrange(experiment, orig.ident, seurat_clusters)

#           # # Optimization: Use data.table or fast dplyr for the summary
#           # sc_object()@meta.data %>%
#           #   count(experiment, orig.ident, seurat_clusters, subcluster) %>%
#           #   arrange(experiment, orig.ident, seurat_clusters)
#           # ?????????????????
#       }
#     )

  })
}