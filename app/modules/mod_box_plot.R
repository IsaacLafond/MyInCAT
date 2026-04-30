# -------------------------
# Box Plot UI
# -------------------------
mod_box_plot_ui <- function(id, feature_choices) {
  ns <- NS(id)

  page_fluid(

    virtualSelectInput(
      inputId = ns("box_features"),
      label = "Select feature:",
      choices = feature_choices,
      search = TRUE,
      inline = FALSE,
      width = NULL
    ),

    plotOutput(
      ns("box_plot"),
      width = "100%",
      height = "66vh"
    ) %>% with_custom_spinner()
    
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