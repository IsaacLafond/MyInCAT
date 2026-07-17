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

      div(
        class = "d-flex gap-3",

        virtualSelectInput(
          inputId = ns("vln_features"),
          label = NULL,
          placeholder = "Select feature...",
          choices = feature_choices,
          search = TRUE,
          width = "250px"
        ),

        actionButton(
          ns("show_code"),
          icon = icon("code"),
          label = "View Code",
          class = "btn-sm",
          disabled = TRUE
        ),

        actionButton(
          ns("download_wrapper"),
          label = downloadButton(
            ns("download_vln"),
            class = "btn-sm d-flex justify-content-center align-items-center w-100 h-100",
            style = "gap: 1ch"
          ),
          class = "p-0 border-0 bg-transparent",
          disabled = TRUE
        )
      )
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

    build_vln_plot <- function() {
      state <- global_state()
      feature <- input$vln_features

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
    }

    output$vln_plot <- renderPlot({
      validate(
        need(!is.null(input$vln_features), "Please select a feature to display the plot."),
      )

      # print(
      #   AverageExpression(state$sc_subset, features = gene_name)#$RNA[gene_name, ]
      # )

      build_vln_plot()

    })

    observe({
      valid <- !is.null(global_state()) && !is.null(input$vln_features)
      updateActionButton(
        session,
        "download_wrapper",
        disabled = !valid
      )
      updateActionButton(
        session,
        "show_code",
        disabled = !valid
      )
    })

    output$download_vln <- downloadHandler(
      filename = function() paste0("vln_plot_", Sys.time(), ".png"),
      content = function(file) {
        ggsave(
          file,
          plot = build_vln_plot(),
          device = "png",
          width = 8,
          height = 6
        )
      }
    )

    observeEvent(input$show_code, {
      req(
        global_state(),
        input$vln_features
      )
      vln_code <- generate_vln_plot_code(global_state(), input$vln_features)

      showModal(modalDialog(
        title = "Source Code",
        size = "xl",
        code_block(vln_code),
        easyClose = TRUE
      ))
    })

  })
}