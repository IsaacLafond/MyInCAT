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
      layout_columns(
        col_widths = c(6, 6),
        class = "m-0",
        actionButton(
          ns("show_metadata"),
          label = "View Cell Composition Summary",
          class = "btn btn-primary"
        ),
        actionButton(
          ns("show_code"),
          label = "View Source Code",
          class = "btn btn-primary"
        )
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

    # Populate placeholder metadata table
    output$meta_table <- renderDataTable(
      options = datatable_options,
      # content:
      {
        req(global_state())

        state <- global_state()

        state$sc_subset@meta.data %>%
          group_by(experiment, orig.ident, seurat_clusters, subcluster) %>%
          summarise(n_cells = n(), .groups = "drop") %>%
          arrange(experiment, orig.ident, seurat_clusters)

          # # Optimization: Use data.table or fast dplyr for the summary
          # sc_object()@meta.data %>%
          #   count(experiment, orig.ident, seurat_clusters, subcluster) %>%
          #   arrange(experiment, orig.ident, seurat_clusters)
          # ?????????????????
      }
    )

    # Modals
    observeEvent(input$show_metadata, {
      showModal(modalDialog(
        title = "Cell Composition Summary",
        size = "xl",
        dataTableOutput(
          session$ns("meta_table"),
          height = "100%"
        ) %>% with_custom_spinner(),
        easyClose = TRUE
      ))
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