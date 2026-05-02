# -------------------------
# CellChat Enriched Pathways UI
# -------------------------
mod_cellchat_enrichedpathways_ui <- function(id) {
  ns <- NS(id)

  page_fluid(

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
mod_cellchat_enrichedpathways_server <- function(id, cellchat_object) {
  moduleServer(id, function(input, output, session) {

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
      req(cellchat_object())

      df <- as.data.frame(cellchat_object()@netP[["pathways"]])
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
    
  })
}