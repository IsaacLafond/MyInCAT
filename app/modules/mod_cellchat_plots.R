# -------------------------
# CellChat Plots UI
# -------------------------
mod_cellchat_plots_ui <- function(id) {
  ns <- NS(id)

  page_fluid(
    radioGroupButtons(
        inputId = ns("current_plot"),
        choices = c("Heatmap", "Circle", "Chord"),
        selected = "Heatmap",
        justified = TRUE
    ),

    conditionalPanel(
      condition = "input.current_plot == 'Heatmap'",
      ns = ns,
      # content:
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Interaction Heatmap",
          # downloadButton(ns("download_meta"), "Download CSV", class = "btn-sm")
        ),
        plotOutput(
          ns("interaction_heatmap_plot"),
          width = "100%",
          height = "66vh"
        ) %>% with_custom_spinner()
      ),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Contribution Heatmap",
          # downloadButton(ns("download_meta"), "Download CSV", class = "btn-sm")
        ),
        card_body(
          fill = FALSE,
          selectInput(
            ns("heatmap_pattern"),
            label = "Select Pattern:",
            choices = c(
              "All" = "all",
              "Incoming" = "incoming",
              "Outgoing" = "outgoing"
            )
          ),
          plotOutput(
            ns("contribution_heatmap_plot"),
            width = "100%",
            height = "66vh"
          ) %>% with_custom_spinner()
        )
      )
    ),

    conditionalPanel(
      condition = "input.current_plot == 'Circle'",
      ns = ns,
      # content:
      coming_soon() # TODO
    ),

    conditionalPanel(
      condition = "input.current_plot == 'Chord'",
      ns = ns,
      # content:
      coming_soon() # TODO
    )
  )
}

# -------------------------
# CellChat Plots server
# -------------------------
mod_cellchat_plots_server <- function(id, cellchat_object) {
  moduleServer(id, function(input, output, session) {

    output$interaction_heatmap_plot <- renderPlot({
      req(cellchat_object)

      netVisual_heatmap(
        cellchat_object(),
        slot.name = "net",
        measure = "weight",
        color.heatmap = "Reds"
      )
    })

    output$contribution_heatmap_plot <- renderPlot({
      req(cellchat_object, input$heatmap_pattern)

      netAnalysis_signalingRole_heatmap(
        cellchat_object(),
        pattern = input$heatmap_pattern,
        slot.name = "netP",
        color.heatmap = "Reds",
        # color.use = 
        width = 20,
        height = 20,
        # font.size = 3
      )
    })
  })
}