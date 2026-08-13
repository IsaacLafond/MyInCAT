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
      class = "btn-sm btn-light"
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
        status = "outline-primary btn-sm",
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

          div(
            class = "d-flex gap-3",

            actionButton(
              ns("show_lr_code"),
              icon = icon("code"),
              label = "View Code",
              class = "btn-sm",
              disabled = TRUE
            ),

            actionButton(
              ns("lr_download_wrapper"),
              label = downloadButton(
                ns("download_lr"),
                class = "btn-sm",
              ),
              class = "p-0 border-0",
              disabled = TRUE
            )
          )
        ),
        conditionalPanel(
          condition = "input.current_table == 'Cell Interactions'",
          ns = ns,
          # content:
          "Cell Interaction Table",

          div(
            class = "d-flex gap-3",
            interactions_popover,

            actionButton(
              ns("show_interactions_code"),
              icon = icon("code"),
              label = "View Code",
              class = "btn-sm",
              disabled = TRUE
            ),

            actionButton(
              ns("interactions_download_wrapper"),
              label = downloadButton(
                ns("download_interactions"),
                class = "btn-sm",
              ),
              class = "p-0 border-0",
              disabled = TRUE
            )
          )
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
mod_cellchat_enrichedpathways_server <- function(id, cellchat_object, sidebar_selections) {
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

    observe({
      lr_valid <- tryCatch(length(enriched_ligand_receptors()) > 0, error = FALSE)

      updateActionButton(
        session,
        "show_lr_code",
        disabled = !lr_valid
      )
      updateActionButton(
        session,
        "lr_download_wrapper",
        disabled = !lr_valid
      )
    })

    observe({
      interactions_valid <- tryCatch(
        length(netP()) > 0 &&
        length(input$source_cells) > 0 &&
        length(input$target_cells) > 0,
        error = FALSE
      )

      updateActionButton(
        session,
        "show_interactions_code",
        disabled = !interactions_valid
      )
      updateActionButton(
        session,
        "interactions_download_wrapper",
        disabled = !interactions_valid
      )
    })

    enriched_ligand_receptors <- reactive({
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

    output$enriched_ligand_receptor_table <- renderDataTable(
      rownames = FALSE,
      options = datatable_options,
      {
        req(enriched_ligand_receptors)
        enriched_ligand_receptors()
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
        choices = unique(x$source),
        selected = unique(x$source)
      )
      updatePickerInput(
        session,
        "target_cells",
        choices = unique(x$target),
        selected = unique(x$target)
      )
    })

    output$cell_interactions_table <- renderDataTable(
      rownames = FALSE,
      options = datatable_options,
    {
      validate(
        need(length(netP()) > 0, "No results."),
        need(length(input$source_cells) > 0, "Please select at least 1 source cell."),
        need(length(input$target_cells) > 0, "Please select at least 1 target cell.")
      )
      x <- netP()

      netP_subset <- x[
        x$source %in% input$source_cells &
        x$target %in% input$target_cells,
      ]

      netP_subset

    })

    # Downloads
    output$download_lr <- downloadHandler(
      filename = function() {
        paste0("lr_pairs_", Sys.time(), ".csv")
      },
      content = function(file) {
        write.csv(enriched_ligand_receptors(), file)
      }
    )

    output$download_interactions <- downloadHandler(
      filename = function() {
        paste0("interactions_", Sys.time(), ".csv")
      },
      content = function(file) {
        x <- netP()

        write.csv(
          x[x$source %in% input$source_cells & x$target %in% input$target_cells,],
          file
        )
      }
    )

    # Modals
    observeEvent(input$show_lr_code, {
      req(sidebar_selections())
      lr_code <- generate_lr_code(sidebar_selections)

      showModal(modalDialog(
        title = "Source Code",
        size = "xl",
        code_block(lr_code),
        easyClose = TRUE
      ))
    })
    observeEvent(input$show_interactions_code, {
      req(
        sidebar_selections(),
        input$source_cells,
        input$target_cells
      )
      int_code <- generate_interactions_code(sidebar_selections, input$source_cells, input$target_cells)

      showModal(modalDialog(
        title = "Source Code",
        size = "xl",
        code_block(int_code),
        easyClose = TRUE
      ))
    })

  })
}