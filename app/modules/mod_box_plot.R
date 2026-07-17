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

      div(
        class = "d-flex gap-3",

        virtualSelectInput(
          inputId = ns("box_features"),
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
            ns("download_box"),
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

    build_box_plot <- function() {
      state <- global_state()
      feature <- input$box_features

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
    }

    output$box_plot <- renderPlot({
      validate(
        need(!is.null(input$box_features), "Please select a feature to display the plot."),
      )

      # print(
      #   AverageExpression(state$sc_subset, features = gene_name)#$RNA[gene_name, ]
      # )

      build_box_plot()

    })

    observe({
      valid <- !is.null(global_state()) && !is.null(input$box_features)
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

    output$download_box <- downloadHandler(
      filename = function() paste0("box_plot_", Sys.time(), ".png"),
      content = function(file) {
        ggsave(
          file,
          plot = build_box_plot(),
          device = "png",
          width = 8,
          height = 6
        )
      }
    )

    observeEvent(input$show_code, {
      req(
        global_state(),
        input$box_features
      )
      box_code <- generate_box_plot_code(global_state(), input$box_features)

      showModal(modalDialog(
        title = "Source Code",
        size = "xl",
        code_block(box_code),
        easyClose = TRUE
      ))
    })

  })
}