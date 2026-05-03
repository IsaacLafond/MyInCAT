# -------------------------
# CellChat Cell Interactions UI
# -------------------------
mod_cellchat_cellinteractions_ui <- function(id) {
  ns <- NS(id)

  page_fluid(
    layout_columns(
      width = c(6, 6),
      selectInput(
        ns("source_cells"),
        label = "Source Cells:",
        choices = NULL,
        multiple = TRUE
      ),
      selectInput(
        ns("target_cells"),
        label = "Target Cells:",
        choices = NULL,
        multiple = TRUE
      )
    ),

    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Cell Interaction Table",
        # downloadButton(ns("download_meta"), "Download CSV", class = "btn-sm")
      ),
      dataTableOutput(
        ns("cell_interactions_table"),
        height = "450px"
      ) %>% with_custom_spinner()
    )

  )
}

# -------------------------
# CellChat Cell Interactions server
# -------------------------
mod_cellchat_cellinteractions_server <- function(id, cellchat_object) {
  moduleServer(id, function(input, output, session) {

    netP <- reactive({
      x <- subsetCommunication(cellchat_object(), slot.name = "netP")
      unique(x)
    })

    observe({
      req(netP())
      x <- netP()

      updateSelectInput(
        session,
        "source_cells",
        choices = unique(x$source)
      )
      updateSelectInput(
        session,
        "target_cells",
        choices = unique(x$target)
      )
    })

    output$cell_interactions_table <- renderDataTable(
      width = "100%",
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        scrollY = "300px",
        scrollCollapse = TRUE
      ),
      rownames = FALSE,
    {
      x <- netP()

      source_subset <- input$source_cells %||% x$source
      target_subset <- input$target_cells %||% x$target

      netP_subset <- x[
        x$source %in% source_subset &
        x$target %in% target_subset,
      ]

      netP_subset

    })

  })
}