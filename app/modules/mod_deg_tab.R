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
            mod_deg_ui("degs")
        ),
        nav_panel(
            title = "GO",
            # content:
            mod_deg_go_ui("deg_go")
        ),
        nav_panel(
            title = "KEGG",
            # content:
            mod_deg_kegg_ui("deg_kegg")
        ),
        nav_panel(
            title = "Plots",
            # content:
            mod_deg_plots_ui("deg_plots", feature_choices)
        )
    )
  )
}

# -------------------------
# DEG Tab server
# -------------------------
mod_deg_tab_server <- function(id, global_state) {
  moduleServer(id, function(input, output, session) {

    DEGs <- mod_deg_server("degs", global_state)
    mod_deg_go_server("deg_go", DEGs)
    mod_deg_kegg_server("deg_kegg", DEGs)

    mod_deg_plots_server("deg_plots", global_state)
    
  })
}