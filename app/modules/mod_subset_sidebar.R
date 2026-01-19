choices_data <- list(
  "cluster" = c(
    "cluster1",
    "cluster2",
    "cluster3",
    "cluster4",
    "cluster5"
    ),
  "subcluster" = c(
    "subclusterA",
    "subclusterB",
    "subclusterC"
    ),
  "orig.ident" = c(
    "sample1",
    "sample2", 
    "sample3"
    ),
  "experiment" = c(
    "experimentX",
    "experimentY"
    )
)

# -------------------------
# Subset sidebar UI
# -------------------------
mod_subset_sidebar_ui <- function(id) {
  ns <- NS(id)

  accordion(
    accordion_panel(
      title = "Subset Options",
      tagList(
        selectizeInput(
          ns("clusters"),
          label = "Clusters:",
          choices = choices_data$cluster,
          multiple = TRUE
        ),
        div(id = ns("cluster_container")),

        selectizeInput(
          ns("subclusters"),
          label = "Subclusters:",
          choices = choices_data$subcluster,
          multiple = TRUE
        ),
        div(id = ns("subcluster_container")),

        selectizeInput( # AKA orig.ident
          ns("samples"),
          label = "Samples:",
          choices = choices_data$orig.ident,
          multiple = TRUE
        ),
        div(id = ns("sample_container")),

        selectizeInput(
          ns("experiments"),
          label = "Experiments:",
          choices = choices_data$experiment,
          multiple = TRUE
        ),
        div(id = ns("experiment_container"))
      )
    ),

      accordion_panel(
        title = "Download Options",
        p("Download options here")
      ),
  )
}


# -------------------------
# Subset sidebar server
# -------------------------
mod_subset_sidebar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper function to manage dynamic UI additions/removals
    manage_pickers <- function(current_vals, previous_vals, prefix, container_id) {
      # 1. Find which ones to add
      to_add <- setdiff(current_vals, previous_vals)
      for (item in to_add) {
        insertUI(
          selector = paste0("#", ns(container_id)),
          where = "beforeEnd",
          ui = div(
            id = ns(paste0("wrapper_", prefix, "_", item)),
            color_picker_ui(ns(paste0("color_picker_", prefix, "_", item)), label = item)
          )
        )
      }
      
      # 2. Find which ones to remove
      to_remove <- setdiff(previous_vals, current_vals)
      for (item in to_remove) {
        removeUI(selector = paste0("#", ns(paste0("wrapper_", prefix, "_", item))))
      }
    }

    # ReactiveVal to keep track of previous states
    prev_clusters <- reactiveVal(NULL)
    observeEvent(input$clusters, ignoreNULL = FALSE, {
      manage_pickers(input$clusters, prev_clusters(), "cluster", "cluster_container")
      prev_clusters(input$clusters)
    })
    
    prev_subclusters <- reactiveVal(NULL)
    observeEvent(input$subclusters, ignoreNULL = FALSE, {
      manage_pickers(input$subclusters, prev_subclusters(), "subcluster", "subcluster_container")
      prev_subclusters(input$subclusters)
    })

    prev_samples <- reactiveVal(NULL)
    observeEvent(input$samples, ignoreNULL = FALSE, {
      manage_pickers(input$samples, prev_samples(), "sample", "sample_container")
      prev_samples(input$samples)
    })

    prev_experiments <- reactiveVal(NULL)
    observeEvent(input$experiments, ignoreNULL = FALSE, {
      manage_pickers(input$experiments, prev_experiments(), "experiment", "experiment_container")
      prev_experiments(input$experiments)
    })

    # # add a color picker for each selected cluster
    # output$cluster_color_pickers <- renderUI({
    #   req(input$clusters) # Ensure clusters are selected
    #   lapply(input$clusters, function(cluster) {
    #     color_picker_ui(session$ns(paste0("color_picker_cluster_", cluster)), label = cluster)
    #   })
    # })
    # # add a color picker for each selected subcluster
    # output$subcluster_color_pickers <- renderUI({
    #   req(input$subclusters) # Ensure subclusters are selected
    #   lapply(input$subclusters, function(subcluster) {
    #     color_picker_ui(session$ns(paste0("color_picker_subcluster_", subcluster)), label = subcluster)
    #   })
    # })
    # # add a color picker for each selected sample
    # output$sample_color_pickers <- renderUI({
    #   req(input$samples) # Ensure samples are selected
    #   lapply(input$samples, function(sample) {
    #     color_picker_ui(session$ns(paste0("color_picker_sample_", sample)), label = sample)
    #   })
    # })
    # # add a color picker for each selected experiment
    # output$experiment_color_pickers <- renderUI({
    #   req(input$experiments) # Ensure experiments are selected
    #   lapply(input$experiments, function(experiment) {
    #     color_picker_ui(session$ns(paste0("color_picker_experiment_", experiment)), label = experiment)
    #   })
    # })

    return(
      reactive({
        # Helper function to match labels to their dynamic color inputs
        get_colors <- function(selections, prefix) {
          if (is.null(selections) || length(selections) == 0) return(NULL)
          
          # Map selection names to the values in the color picker inputs
          # Note: we use session$input to access inputs within this module
          vals <- sapply(selections, function(x) {
            # This ID must match exactly how you created it in renderUI
            input_id <- paste0("color_picker_", prefix, "_", x)
            val <- input[[input_id]]

            # Default to black if not yet rendered
            if (is.null(val)) {
              return("#000000") # default black
            } else {
              return(val)
            }
          })
          
          names(vals) <- selections
          return(vals)
        }


        list(
          clusters = get_colors(input$clusters, "cluster"),
          subclusters = get_colors(input$subclusters, "subcluster"),
          samples = get_colors(input$samples, "sample"),
          experiments = get_colors(input$experiments, "experiment")
        )
      })
    )

  })
}