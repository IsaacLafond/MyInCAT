choices_data <- list(
  "cluster" = c(
    "cluster 1",
    "cluster 2",
    "cluster 3",
    "cluster 4",
    "cluster 5"
    ),
  "subcluster" = c(
    "subcluster A",
    "subcluster B",
    "subcluster C"
    ),
  "orig.ident" = c(
    "sample 1",
    "sample 2", 
    "sample 3"
    ),
  "experiment" = c(
    "experiment X",
    "experiment Y"
    )
)

# -------------------------
# UMAP UI
# -------------------------
mod_umap_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    layout_sidebar(
      # Input: Group by selection
      sidebar = sidebar(
        # select input with set of checkboxes for each option based on the selected group by selection
        selectInput(
          ns("group_by"),
          "Group by:",
          choices = names(choices_data)
        ),
        # Placeholder for dynamic checkboxes
        uiOutput(ns("checkboxes_output")),

        open = "always"
      ),

      # Output: UMAP
      plotOutput(ns("umapPlot"))
    ),

    # Output: Metadata table
    tableOutput(ns("meta_table")),

    sidebar = sidebar(umap_code(), position = "right", fillable = TRUE, open = "closed")
  )
}


# -------------------------
# UMAP server
# -------------------------
mod_umap_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Reactive expression to get the relevant choices based on select input
    reactive_choices <- reactive({
      req(input$group_by) # Ensure a category is selected
      choices_data[[input$group_by]]
    })
    
    # 3. Render the checkbox group dynamically
    output$checkboxes_output <- renderUI({
      # Use the reactive value to set the choices for checkboxGroupInput
      checkboxGroupInput(
        inputId = "item_checkboxes",
        label = "Select Items:",
        choices = reactive_choices(),
        selected = NULL # Start with nothing selected
      )
    })

    # Populate placeholder UMAP plot
    output$umapPlot <- renderPlot({
      hist(rnorm(100))
    })

    # Populate placeholder metadata table
    output$meta_table <- renderTable({
      data.frame(
        SampleID = 1:10,
        Metadata = sample(LETTERS, 10)
      )
    },
    # striped = TRUE,
    bordered = TRUE)

  })
}