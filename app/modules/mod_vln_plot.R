# -------------------------
# Vln Plot UI
# -------------------------
mod_vln_plot_ui <- function(id, feature_choices) {
  ns <- NS(id)

  card(
    fill = TRUE,
    full_screen = TRUE,
    card_header(
      class = "d-flex justify-content-between align-items-center",
      "Violin Plot",

      virtualSelectInput(
        inputId = ns("vln_features"),
        label = NULL,
        placeholder = "Select feature...",
        choices = feature_choices,
        search = TRUE,
        width = "250px"
      ),
      # downloadButton(ns("download_meta"), "Download CSV", class = "btn-sm")
    ),
    card_body(
      div(
        class = "h-100",
        style = "aspect-ratio: 4 / 3; min-width: 500px;",
        # content
        plotOutput(
          ns("vln_plot"),
          width = "100%",
          height = "100%",
          fill = TRUE
        ) %>% with_custom_spinner()
      )
    ),
    # card_footer( # ===== KEEP FOOTER FOR AVGEXPRESSION =====
    #   layout_columns(
    #     col_widths = c(6, 6),
    #     class = "m-0",
    #     actionButton(
    #       ns("show_metadata"),
    #       label = "View Cell Composition Summary",
    #       class = "btn btn-primary"
    #     ),
    #     actionButton(
    #       ns("show_code"),
    #       label = "View Source Code",
    #       class = "btn btn-primary"
    #     )
    #   )
    # )
  )
}


# -------------------------
# Vln Plot server
# -------------------------
mod_vln_plot_server <- function(id, global_state) {
  moduleServer(id, function(input, output, session) {

    output$vln_plot <- renderPlot({
      state <- global_state()
      feature <- input$vln_features

      validate(
        need(!is.null(feature), "Please select a feature to display the plot."),
      )

      # print(
      #   AverageExpression(state$sc_subset, features = gene_name)#$RNA[gene_name, ]
      # )

      VlnPlot(
        state$sc_subset, 
        features = feature,
        pt.size = 0,
        group.by = state$group_by
        # cols = c("#cluster_name_1" = "#colour_1", "#cluster_name_2" = "#colour_2", "#etc.") # LEAVE BLANK FOR DEFAULT COLOURS
      ) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(face = "italic")
      ) +
      labs(x = NULL, y = "Expression", title = feature)

    })


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