# -------------------------
# Subset sidebar UI
# -------------------------
mod_subset_sidebar_ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    accordion(
      accordion_panel(
        title = "Subset Options",
        p("Subset selection options here...")
        # tagList(
        #   selectizeInput(
        #     ns("clusters"),
        #     label = "Clusters:",
        #     choices = names(choices_data$cluster),
        #     multiple = TRUE
        #   ),
        #   div(id = ns("cluster_container")),

        #   selectizeInput(
        #     ns("subclusters"),
        #     label = "Subclusters:",
        #     choices = choices_data$subcluster,
        #     multiple = TRUE
        #   ),
        #   div(id = ns("subcluster_container")),

        #   selectizeInput( # AKA orig.ident
        #     ns("samples"),
        #     label = "Samples:",
        #     choices = choices_data$orig.ident,
        #     multiple = TRUE
        #   ),
        #   div(id = ns("sample_container")),

        #   selectizeInput(
        #     ns("experiments"),
        #     label = "Experiments:",
        #     choices = choices_data$experiment,
        #     multiple = TRUE
        #   ),
        #   div(id = ns("experiment_container"))
        # )
      ),

        accordion_panel(
          title = "Download Options",
          p("Download options here...")
        ),
    )
  )
}


# -------------------------
# Subset sidebar server
# -------------------------
mod_subset_sidebar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Define the categories we want to manage
    categories <- list(
      "Cluster"     = "cluster_container",
      "Subcluster"  = "subcluster_container",
      "Sample"      = "sample_container",
      "Experiment"  = "experiment_container"
    )

    # 2. Create a list of reactiveVals to track the 'previous' state for each category
    # We use 'setNames' to create: list(cluster = reactiveVal(NULL), ...)
    prev_states <- lapply(setNames(names(categories), names(categories)), function(x) reactiveVal(NULL))

    # 3. The Management Loop
    # We use 'lapply' over the names to create an observer for each category
    lapply(names(categories), function(cat_name) {
      
      # Determine the input ID (e.g., input$clusters, input$subclusters)
      # Note: Your UI IDs use plural (clusters), so we append 's'
      input_id <- paste0(cat_name, "s") 
      container_id <- categories[[cat_name]]

      observeEvent(input[[input_id]], ignoreNULL = FALSE, {
        current_vals  <- input[[input_id]]
        previous_vals <- prev_states[[cat_name]]()
        
        # --- Manage Additions ---
        to_add <- setdiff(current_vals, previous_vals)
        for (item in to_add) {
          insertUI(
            selector = paste0("#", ns(container_id)),
            where = "beforeEnd",
            ui = div(
              id = ns(paste0("wrapper_", cat_name, "_", item)),
              # Your color_picker_ui function
              color_picker_ui(ns(paste0("color_picker_", cat_name, "_", item)), label = item)
            )
          )
        }
        
        # --- Manage Removals ---
        to_remove <- setdiff(previous_vals, current_vals)
        for (item in to_remove) {
          removeUI(selector = paste0("#", ns(paste0("wrapper_", cat_name, "_", item))))
        }
        
        # Update the state tracker
        prev_states[[cat_name]](current_vals)
      })
    })

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