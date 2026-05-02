# -------------------------
# CellChat Enriched Pathways UI
# -------------------------
mod_cellchat_enrichedpathways_ui <- function(id) {
  ns <- NS(id)

  page_fluid(
    layout_columns(
      col_widths = c(6, 6),
      selectInput(
        ns("sample"),
        label = "Select sample:",
         choices = c(
          "Agca_snLLC_CTL",
          "Agca_snLLC_LLC",
          "Brown_scLLC_CTL",
          "Brown_scLLC_2w",
          "Brown_scLLC_2.5w",
          "Brown_scLLC_3.5w",
          "Kim_scB16F10_CTL",
          "Kim_scB16F10_B16F10",
          "Pryce_scC26_CTL",
          "Pryce_scC26_C26",
          "Pryce_scKPP_CTL",
          "Pryce_scKPP_KPP",
          "Zhang_snKIC_CTL",
          "Zhang_snKIC_KIC"
        )
      ),
      selectInput(
        ns("interaction_type"),
        label = "Select interaction type:",
        choices = c(
          "all",
          "secreted",
          "ECM",
          "ccc",
          "nps"
        )
      )
    ),

    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Pathways",
        # downloadButton(ns("download_meta"), "Download CSV", class = "btn-sm")
      ),
      dataTableOutput(
        ns("enriched_pathways_table"),
        height = "450px"
      ) %>% with_custom_spinner()
    ),
    
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Enriched ligand-receptor pairs",
        # downloadButton(ns("download_meta"), "Download CSV", class = "btn-sm")
      ),
      dataTableOutput(
        ns("enriched_ligand_receptor_table"),
        height = "450px"
      ) %>% with_custom_spinner()
    )


  )
}

# -------------------------
# CellChat Enriched Pathways server
# -------------------------
mod_cellchat_enrichedpathways_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    cellchat_obj <- reactive({
      req(input$sample, input$interaction_type)

      # Get path of the object based on inputs
      obj_name <- paste("cellchat", input$sample, input$interaction_type, sep = "_")
      path <- paste0("data/cellchat/", obj_name, ".rds")
      cc_object <- readRDS(path)

      cc_object

    })

    output$enriched_pathways_table <- renderDataTable(
      width = "100%",
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        scrollY = "300px",
        scrollCollapse = TRUE
      ),
      rownames = FALSE,
      {
      req(cellchat_obj())

      df <- as.data.frame(cellchat_obj()@netP[["pathways"]])
      colnames(df)[1] <- "pathways"

      df

    })

    output$enriched_ligand_receptor_table <- renderDataTable(
      width = "100%",
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        scrollY = "300px",
        scrollCollapse = TRUE
      ),
      rownames = FALSE,
      {

      req(cellchat_obj())

      cc_obj <- cellchat_obj()

      # Extract enriched ligand-receptor pairs per sample/interactions type.
      out <- extractEnrichedLR(
        cc_obj,
        signaling = cc_obj@netP[["pathways"]],
        geneLR.return = TRUE
      )
      print(out)
      out$pairLR

    })
    
  })
}