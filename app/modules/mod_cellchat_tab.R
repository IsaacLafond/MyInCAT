# -------------------------
# CellChat Tab UI
# -------------------------
mod_cellchat_tab_ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    navset_card_underline(
        nav_panel(
            title = "Enriched Pathways",
            # content:
            mod_cellchat_enrichedpathways_ui(ns("enriched_pathways"))
        ),
        nav_panel(
            title = "Cell Interactions",
            # content:
            mod_cellchat_cellinteractions_ui(ns("cell_interactions"))
        ),
        nav_panel(
            title = "Plots",
            # content:
            mod_cellchat_plots_ui(ns("cellchat_plots"))
        )
    )
  )
}

# -------------------------
# CellChat Tab server
# -------------------------
mod_cellchat_tab_server <- function(id, cellchat_object) {
  moduleServer(id, function(input, output, session) {

    mod_cellchat_enrichedpathways_server("enriched_pathways", cellchat_object)
    mod_cellchat_cellinteractions_server("cell_interactions", cellchat_object)
    mod_cellchat_plots_server("cellchat_plots", cellchat_object)

  })
}