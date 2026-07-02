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
      # downloadButton(ns("download_meta"), "Download CSV", class = "btn-sm")
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
    ),
    card_footer(
      actionButton(
        ns("show_code"),
        icon = icon("code"),
        label = "View Source Code",
        class = "btn btn-primary w-100"
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

    observeEvent(input$show_code, {
      showModal(modalDialog(
        title = "Source Code",
        size = "xl",
        code_block(umap_code),
        easyClose = TRUE
      ))
    })

  })
}