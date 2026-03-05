# -------------------------
# DEG UI
# -------------------------
mod_deg_ui <- function(id) {
    ns <- NS(id)

    coming_soon() # TODO
}


# -------------------------
# DEG server
# -------------------------
mod_deg_server <- function(id, sidebar_selections, sc_object) {
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