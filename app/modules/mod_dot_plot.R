# -------------------------
# Dot Plot UI
# -------------------------
mod_dot_plot_ui <- function(id, feature_choices) {
  ns <- NS(id)

  page_fluid(

    virtualSelectInput(
      inputId = ns("dot_features"),
      label = "Select features:",
      choices = feature_choices,
      multiple = TRUE,
      search = TRUE,
      showSelectedOptionsFirst = TRUE,
      disableSelectAll = TRUE,
      inline = FALSE,
      width = NULL
    ),

    plotOutput(
      ns("dot_plot"),
      width = "100%",
      height = "66vh"
    ) %>% with_custom_spinner()

  )
}


# -------------------------
# Dot Plot server
# -------------------------
mod_dot_plot_server <- function(id, global_state) {
  moduleServer(id, function(input, output, session) {

    features_on_close <- reactiveVal(NULL)

    observeEvent(input$dot_features_open, {
      if (isFALSE(input$dot_features_open)) {
        features_on_close(input$dot_features)
      }
    })

    output$dot_plot <- renderPlot({
      state <- global_state()
      features <- features_on_close()

      validate(
        need(length(features) > 0, "Please select at least one feature to display the plot."),
        need(length(features) <= 75, "Please select no more than 75 features.")
      )

      DotPlot(
        state$sc_subset,
        features = features,
        group.by = state$group_by
      ) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "italic")
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