# -------------------------
# CellChat Enriched Pathways UI
# -------------------------
mod_cellchat_enrichedpathways_ui <- function(id) {
  ns <- NS(id)

  interactions_popover <- popover(
    trigger = actionButton(
      ns("settings_trigger"),
      label = NULL,
      icon = icon("gear"),
      class = "btn-light"
    ),
    placement = "auto",
    # options = list(
    #   trigger = "click focus"
    # ),
    # popover content:
    fluidRow(
      pickerInput(
        ns("source_cells"),
        label = "Source Cells:",
        choices = NULL,
        multiple = TRUE
      ),
      pickerInput(
        ns("target_cells"),
        label = "Target Cells:",
        choices = NULL,
        multiple = TRUE
      )
    )
  )

  page_fillable(
    radioGroupButtons(
        inputId = ns("current_table"),
        choices = c("Enriched LR Pairs", "Cell Interactions"),
        selected = "Enriched LR Pairs",
        justified = TRUE
    ),

    card(
      fill = TRUE,
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between align-items-center",
        conditionalPanel(
          condition = "input.current_table == 'Enriched LR Pairs'",
          ns = ns,
          # content:
          "Enriched ligand-receptor pairs",
          # downloadButton(ns("download_meta"), "Download CSV", class = "btn-sm")
        ),
        conditionalPanel(
          condition = "input.current_table == 'Cell Interactions'",
          ns = ns,
          # content:
          "Cell Interaction Table",
          interactions_popover
          # downloadButton(ns("download_meta"), "Download CSV", class = "btn-sm")
        )
      ),
      card_body(
        div(
          class = "d-flex h-100 justify-content-center align-items-center",
          # content
            conditionalPanel(
              condition = "input.current_table == 'Enriched LR Pairs'",
              ns = ns,
              # content:
              dataTableOutput(
                ns("enriched_ligand_receptor_table")
              ) %>% with_custom_spinner()
            ),
            conditionalPanel(
              condition = "input.current_table == 'Cell Interactions'",
              ns = ns,
              # content:
              dataTableOutput(
                ns("cell_interactions_table")
              ) %>% with_custom_spinner()
            )
          )
      )
    )
  )

  # page_fluid(

  #   card(
  #     card_header(
  #       class = "d-flex justify-content-between align-items-center",
  #       "Pathways",
  #       # downloadButton(ns("download_meta"), "Download CSV", class = "btn-sm")
  #     ),
  #     dataTableOutput(
  #       ns("enriched_pathways_table"),
  #       height = "450px"
  #     ) %>% with_custom_spinner()
  #   ),
    


  # )
}

# -------------------------
# CellChat Enriched Pathways server
# -------------------------
mod_cellchat_enrichedpathways_server <- function(id, cellchat_object) {
  moduleServer(id, function(input, output, session) {

    # ==========================
    # Enriched ligand-receptor pairs table
    # ==========================

    # output$enriched_pathways_table <- renderDataTable(
    #   width = "100%",
    #   options = list(
    #     paging = FALSE,
    #     scrollX = TRUE,
    #     scrollY = "300px",
    #     scrollCollapse = TRUE
    #   ),
    #   rownames = FALSE,
    #   {
    #   req(cellchat_object())

    #   df <- as.data.frame(cellchat_object()@netP[["pathways"]])
    #   colnames(df)[1] <- "pathways"

    #   df

    # })

    output$enriched_ligand_receptor_table <- renderDataTable(
      rownames = FALSE,
      options = datatable_options,
      {

      req(cellchat_object())

      cc_obj <- cellchat_object()

      # Extract enriched ligand-receptor pairs per sample/interactions type.
      out <- extractEnrichedLR(
        cc_obj,
        signaling = cc_obj@netP[["pathways"]],
        geneLR.return = TRUE
      )
      out$pairLR

    })

    # ==========================
    # Cell Interactions table
    # ==========================
    netP <- reactive({
      x <- subsetCommunication(cellchat_object(), slot.name = "netP")
      unique(x)
    })

    observe({
      req(netP())
      x <- netP()

      updatePickerInput(
        session,
        "source_cells",
        choices = unique(x$source)
      )
      updatePickerInput(
        session,
        "target_cells",
        choices = unique(x$target)
      )
    })

    output$cell_interactions_table <- renderDataTable(
      rownames = FALSE,
      options = datatable_options,
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