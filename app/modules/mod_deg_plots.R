# -------------------------
# DEG Plots UI
# -------------------------
mod_deg_plots_ui <- function(id, feature_choices) {
  ns <- NS(id)

  page_fillable(
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
    ),
    plotOutput(
      ns("dot_plot"),
      width = "100%",
      height = "66vh"
    )
  )
}


# -------------------------
# DEG Plots server
# -------------------------
mod_deg_plots_server <- function(id, sidebar_selections, sc_object) {
  moduleServer(id, function(input, output, session) {

    output$dot_plot <- renderPlot({
      req(sidebar_selections(), sc_object())

      validate(
        need(length(input$features) > 0, "Please select at least one feature to display the plot."),
        need(length(input$features) <= 75, "Please select no more than 75 features.")
      )

      selected_options <- sidebar_selections()
      group_val <- groupby_choices[[selected_options$group_by]]

      DotPlot(
        sc_object(),
        features = input$features,
        group.by = group_val
      ) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "italic"),
        axis.text.y = element_text(face = "italic")
      ) +
      scale_color_gradientn(colors = c("red", "white", "blue"))
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