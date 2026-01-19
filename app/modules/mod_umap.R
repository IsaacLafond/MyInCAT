# -------------------------
# UMAP UI
# -------------------------
mod_umap_ui <- function(id) {
  ns <- NS(id)

  page_fluid(

    selectInput(
      ns("group_by"),
      "Group by:",
      choices = names(choices_data)
    ),

    # verbatimTextOutput(ns("debug_group_by"), placeholder = TRUE),

    # Output: UMAP
    plotOutput(
      ns("umapPlot"),
      width = "100%",
      height = "66vh"
    ),

    # Output: Metadata table
    tableOutput(ns("meta_table")),

    # Relevant code
    accordion(
      id = ns("umap_code_accordion"),
      accordion_panel(
        title = "Relevant Code",
        umap_code()
      ),
      open = FALSE,
      class = "mb-5"
    )
  )
}


# -------------------------
# UMAP server
# -------------------------
mod_umap_server <- function(id, sidebar_selections) {
  moduleServer(id, function(input, output, session) {

    # Populate placeholder UMAP plot
    output$umapPlot <- renderPlot({
      hist(rnorm(100))
    })

    # Populate placeholder metadata table
    output$meta_table <- renderTable(
      width = "100%",
      striped = TRUE,
      hover = TRUE,
      bordered = TRUE,
      {data.frame(
        SampleID = 1:10,
        Metadata = sample(LETTERS, 10)
      )}
    )

    # Debugging output
    # show group by selection in verbatim text and subset choices from subset sidebar
    output$debug_group_by <- renderPrint({
      sidebar_selections()
    })

  })
}