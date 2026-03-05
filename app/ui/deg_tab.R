# -------------------------
# DEG Tab UI
# -------------------------
deg_tab_ui <- function(id, feature_choices) {
  ns <- NS(id)

  page_fillable(
    navset_card_underline(
        nav_panel(
            title = "DEGs",
            # content:
            mod_deg_ui("degs")
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
            mod_deg_plots_ui("deg_plots", feature_choices)
        )
    )
  )
}
