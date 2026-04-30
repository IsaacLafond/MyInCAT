# -------------------------
# DEG Plots UI
# -------------------------
mod_deg_plots_ui <- function(id, feature_choices) {
  ns <- NS(id)

  page_fluid(
    # plot type select input
    selectInput(
      ns("plot_type"),
      label = "Select plot type:",
      choices = c(
        "Dot Plot" = "dot",
        "Violin Plot" = "violin",
        "Box Plot" = "box",
        "Pseudotime Plot" = "pseudo"
      ),
      selected = "dot"
    ),

    conditionalPanel(
      condition = "input.plot_type == 'dot'",
      ns = ns,
      # content:
      mod_dot_plot_ui(ns("dot_plot"), feature_choices)
    ),

    conditionalPanel(
      condition = "input.plot_type == 'violin'",
      ns = ns,
      # content:
      mod_vln_plot_ui(ns("vln_plot"), feature_choices)
    ),

    conditionalPanel(
      condition = "input.plot_type == 'box'",
      ns = ns,
      # content:
      mod_box_plot_ui(ns("box_plot"), feature_choices)
    ),

    conditionalPanel(
      condition = "input.plot_type == 'pseudo'",
      ns = ns,
      # content:
      mod_pseudotime_plot_ui(ns("pseudo_plot"))
    )
  )
}


# -------------------------
# DEG Plots server
# -------------------------
mod_deg_plots_server <- function(id, global_state) {
  moduleServer(id, function(input, output, session) {

    mod_dot_plot_server("dot_plot", global_state)
    mod_vln_plot_server("vln_plot", global_state)
    mod_box_plot_server("box_plot", global_state)
    mod_pseudotime_plot_server("pseudo_plot", global_state)

  })
}