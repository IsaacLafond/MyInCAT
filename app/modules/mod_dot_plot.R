# -------------------------
# Dot Plot UI
# -------------------------
mod_dot_plot_ui <- function(id, feature_choices) {
  ns <- NS(id)

  card(
    fill = TRUE,
    full_screen = TRUE,
    card_header(
      class = "d-flex justify-content-between align-items-center",
      "Dot Plot",

      div(
        class = "d-flex gap-3",

        virtualSelectInput(
          inputId = ns("dot_features"),
          label = NULL,
          placeholder = "Select features...",
          choices = feature_choices,
          multiple = TRUE,
          search = TRUE,
          showSelectedOptionsFirst = TRUE,
          disableSelectAll = TRUE,
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
            ns("download_dot"),
            class = "btn-sm"
          ),
          class = "p-0 border-0",
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
          ns("dot_plot"),
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

    plot_is_valid <- reactive({
      state <- global_state()
      features <- features_on_close()

      !is.null(state) && length(features) > 0
    })

    build_dot_plot <- function() {
      state <- global_state()
      features <- features_on_close()

      DotPlot(
        state$sc_subset,
        features = features,
        group.by = state$group_by
      ) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "italic")
      ) +
      scale_color_gradientn(colors = c("red", "white", "blue"))

    }

    output$dot_plot <- renderPlot({
      validate(
        need(length(features_on_close()) > 0, "Please select at least one feature to display the plot."),
        need(length(features_on_close()) <= 75, "Please select no more than 75 features.")
      )
      
      build_dot_plot()
    })

    observe({
      valid <- plot_is_valid()
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

    output$download_dot <- downloadHandler(
      filename = function() paste0("dot_plot_", Sys.time(), ".png"),
      content = function(file) {
        ggsave(
          file,
          plot = build_dot_plot(),
          device = "png",
          width = 8,
          height = 6
        )
      }
    )

    observeEvent(input$show_code, {
      req(plot_is_valid())
      dot_code <- generate_dot_plot_code(global_state(), features_on_close())

      showModal(modalDialog(
        title = "Source Code",
        size = "xl",
        code_block(dot_code),
        easyClose = TRUE
      ))
    })

  })
}