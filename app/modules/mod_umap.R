# -------------------------
# UMAP UI
# -------------------------
mod_umap_ui <- function(id) {
  ns <- NS(id)
  
  card(
    fill = TRUE,
    full_screen = TRUE,
    card_header(
      class = "d-flex justify-content-between align-items-center",
      "UMAP Plot",
      div(
        class = "d-flex gap-3",
        actionButton(
          ns("show_code"),
          icon = icon("code"),
          label = "View Code",
          class = "btn-sm"
        ),
        downloadButton(
          ns("download_plot"),
          class = "btn-sm"
        )
      )
    ),
    card_body(
      div(
        class = "h-100",
        style = "aspect-ratio: 4 / 3; min-width: 500px;",
        # content
        plotOutput(
          ns("umap_plot"),
          width = "100%",
          height = "100%",
          fill = TRUE
        ) %>% with_custom_spinner()
      )
    )
  )

}


# -------------------------
# UMAP server
# -------------------------
mod_umap_server <- function(id, global_state) {
  moduleServer(id, function(input, output, session) {

    # Populate placeholder UMAP plot
    output$umap_plot <- renderPlot({
      req(global_state())

      state <- global_state()

      DimPlot(
        state$sc_subset,
        reduction = "umap",
        group.by = state$group_by,
        repel = TRUE,
        pt.size = 1
      ) +
      ggplot2::labs(title = "", x = "UMAP1", y = "UMAP2") +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
      )

    })

    # implement download handler for UMAP plot
    output$download_plot <- downloadHandler(
      filename = function() {
        paste("umap_plot", Sys.time(), ".png", sep = "")
      },
      content = function(file) {
        req(global_state())
        state <- global_state()

        ggsave(
          file,
          plot = DimPlot(
            state$sc_subset,
            reduction = "umap",
            group.by = state$group_by,
            repel = TRUE,
            pt.size = 1
          ) +
          ggplot2::labs(title = "", x = "UMAP1", y = "UMAP2") +
          theme(
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()
          ),
          device = "png",
          width = 8,
          height = 6
        )
      }
    )

    observeEvent(input$show_code, {
      req(global_state())
      state <- global_state()
      umap_code <- generate_umap_code(state)
      
      showModal(modalDialog(
        title = "Source Code",
        size = "xl",
        code_block(umap_code),
        easyClose = TRUE
      ))
    })

  })
}