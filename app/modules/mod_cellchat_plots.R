# -------------------------
# CellChat Plots UI
# -------------------------
mod_cellchat_plots_ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    coming_soon() # TODO
  )
}

# -------------------------
# CellChat Plots server
# -------------------------
mod_cellchat_plots_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}