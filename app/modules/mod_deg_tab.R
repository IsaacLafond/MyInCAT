# -------------------------
# DEG Tab UI
# -------------------------
mod_deg_tab_ui <- function(id, feature_choices) {
  ns <- NS(id)

  page_fillable(
    navset_card_underline(
        nav_panel(
            title = "DEGs",
            # content:
            mod_deg_ui(ns("degs"))
        ),
        nav_panel(
            title = "KEGG",
            # content:
            coming_soon() # TODO
        ),
        nav_panel(
            title = "GO",
            # content:
            coming_soon() # TODO
        ),
        nav_panel(
            title = "Plots",
            # content:
            mod_deg_plots_ui(ns("deg_plots"), feature_choices)
        )
    )
  )
}

# -------------------------
# DEG Tab server
# -------------------------
mod_deg_tab_server <- function(id, global_state) {
  moduleServer(id, function(input, output, session) {

    mod_deg_plots_server("deg_plots", global_state)
    mod_deg_server("degs", global_state)
    
  })
}