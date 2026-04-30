# -------------------------
# DEG Plots UI
# -------------------------
mod_deg_plots_ui <- function(id, feature_choices) {
  ns <- NS(id)

  page_fluid(
    # plot type select input
    selectInput(
      ns("plot_type"),
      label = "Select plot type:",
      choices = c(
        "Dot Plot" = "dot",
        "Violin Plot" = "violin",
        "Box Plot" = "box",
        "Pseudotime Plot" = "pseudo"
      ),
      selected = "dot"
    ),

    conditionalPanel(
      condition = "input.plot_type == 'dot'",
      ns = ns,
      # content:
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
    ),

    conditionalPanel(
      condition = "input.plot_type == 'violin'",
      ns = ns,
      # content:
      virtualSelectInput(
        inputId = ns("vln_features"),
        label = "Select feature:",
        choices = feature_choices,
        search = TRUE,
        inline = FALSE,
        width = NULL
      ),
      plotOutput(
        ns("vln_plot"),
        width = "100%",
        height = "66vh"
      ) %>% with_custom_spinner()
    ),

    conditionalPanel(
      condition = "input.plot_type == 'box'",
      ns = ns,
      # content:
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
    ),

    conditionalPanel(
      condition = "input.plot_type == 'pseudo'",
      ns = ns,
      # content:
      coming_soon() # TODO
    )
  )
}


# -------------------------
# DEG Plots server
# -------------------------
mod_deg_plots_server <- function(id, global_state) {
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
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "italic"),
        axis.text.y = element_text(face = "italic")
      ) +
      scale_color_gradientn(colors = c("red", "white", "blue"))

    })

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