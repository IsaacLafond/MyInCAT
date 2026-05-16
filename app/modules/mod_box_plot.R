# -------------------------
# Box Plot UI
# -------------------------
mod_box_plot_ui <- function(id, feature_choices) {
  ns <- NS(id)

  card(
    fill = TRUE,
    full_screen = TRUE,
    card_header(
      class = "d-flex justify-content-between align-items-center",
      "Box Plot",

      virtualSelectInput(
        inputId = ns("box_features"),
        label = NULL,
        choices = feature_choices,
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
          ns("box_plot"),
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
# Box Plot server
# -------------------------
mod_box_plot_server <- function(id, global_state) {
  moduleServer(id, function(input, output, session) {

    output$box_plot <- renderPlot({
      state <- global_state()
      feature <- input$box_features

      validate(
        need(!is.null(feature), "Please select a feature to display the plot."),
      )

      # print(
      #   AverageExpression(state$sc_subset, features = gene_name)#$RNA[gene_name, ]
      # )

      ggplot(FetchData(
        state$sc_subset,
        vars = c(feature, state$group_by)
      ), aes(
        x = !!sym(state$group_by),
        y = !!sym(feature),
        fill = !!sym(state$group_by)
      )) +
      geom_boxplot(outlier.shape = NA, width = 0.6) +
      # scale_fill_manual(
      #   values = c(
      #     "#sample_name_1" = "#colour_1",
      #     "#sample_name_2" = "#colour_2"
      #     # Leave blank for default colors
      #   )
      # ) +
      theme_classic() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "italic")
      ) +
      labs(
        x = NULL,
        y = "Expression",
        title = feature
      )

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