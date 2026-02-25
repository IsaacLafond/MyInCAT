# -------------------------
# DEGs UI
# -------------------------
mod_deg_ui <- function(id, feature_choices) {
  ns <- NS(id)

  page_fillable(
    navset_card_underline(
        nav_panel(
            title = "DEGs",
            # content:
            coming_soon() # TODO
        ),
        nav_panel(
            title = "KEGG",
            # content:
            coming_soon() # TODO
        ),
        nav_panel(
            title = "GO",
            # content:
            coming_soon() # TODO
        ),
        nav_panel(
            title = "Plots",
            # content:
            virtualSelectInput(
                inputId = ns("features"),
                label = "Select features:",
                choices = feature_choices,
                multiple = TRUE,
                search = TRUE,
                showSelectedOptionsFirst = TRUE,
                updateOn = "close",
                inline = FALSE,
                width = NULL
            )
        )
    )
  )
}


# -------------------------
# DEGs server
# -------------------------
mod_deg_server <- function(id, sidebar_selections) {
  moduleServer(id, function(input, output, session) {
#     output$output <- renderPrint({
#       str(sidebar_selections())
#     })

#     # Populate placeholder UMAP plot
#     output$umapPlot <- renderPlot({
#       selected_options <- sidebar_selections()
#       group_val <- groupby_choices[[selected_options$group_by]]

#       DimPlot(
#         sc_object(),
#         reduction = "umap",
#         group.by = group_val,
#         repel = TRUE,
#         pt.size = 1
#       ) +
#       ggplot2::labs(title = "", x = "UMAP1", y = "UMAP2") +
#       theme(
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank()
#       )

#     })

#     # Populate placeholder metadata table
#     output$meta_table <- renderTable(
#       width = "100%",
#       striped = TRUE,
#       hover = TRUE,
#       bordered = TRUE,
#       # content:
#       {
#         sc_object()@meta.data %>%
#           group_by(experiment, orig.ident, seurat_clusters, subcluster) %>%
#           summarise(n_cells = n(), .groups = "drop") %>%
#           arrange(experiment, orig.ident, seurat_clusters)
#       }
#     )

  })
}