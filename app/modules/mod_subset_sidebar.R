# -------------------------
# Subset sidebar UI
# -------------------------
mod_subset_sidebar_ui <- function(id, tree_data) {
  ns <- NS(id)

  page_fillable(
    accordion(
      accordion_panel(
        title = "Subset Options",

        tagList(
          selectInput(
            ns("group_by"),
            "Group by:",
            choices = names(groupby_choices)
          ),

          # pickerInput(
          #   inputId = ns("experiments"),
          #   label = "Experiments:",
          #   choices = experiments,
          #   selected = experiments,
          #   options = pickerOptions(
          #     actionsBox = TRUE,
          #     size = 10,
          #     dropdownAlignRight = 'auto'
          #   ),
          #   multiple = TRUE
          # ),

          # div(id = ns("experiment_container"))

          # pickerInput( # AKA orig.ident
          #   inputId = ns("samples"),
          #   label = "Samples:",
          #   choices = samples,
          #   selected = samples,
          #   options = pickerOptions(
          #     actionsBox = TRUE,
          #     size = 10,
          #     dropdownAlignRight = 'auto'
          #   ),
          #   multiple = TRUE
          # ),

          # div(id = ns("sample_container")),
          
          # pickerInput(
          #   inputId = ns("clusters"),
          #   label = "Clusters:",
          #   choices = clusters,
          #   selected = clusters,
          #   options = pickerOptions(
          #     actionsBox = TRUE,
          #     size = 10,
          #     dropdownAlignRight = 'auto'
          #   ),
          #   multiple = TRUE
          # ),

          # div(id = ns("cluster_container")),
          
          # pickerInput(
          #   inputId = ns("subclusters"),
          #   label = "Subclusters:",
          #   choices = subclusters,
          #   selected = subclusters,
          #   options = pickerOptions(
          #     actionsBox = TRUE,
          #     size = 10,
          #     dropdownAlignRight = 'auto'
          #   ),
          #   multiple = TRUE
          # ),
          
          # div(id = ns("subcluster_container")),

          treeInput(
            inputId = ns("subset_tree"),
            label = "Select Desired Elements:",
            choices = create_tree(tree_data),
            selected = tree_data$experiment,
            # closeDepth = 4,
            returnValue = "all"
            # returnValue = c("text", "id", "all"),
          ),

          actionButton(
            inputId = ns("apply"),
            label = "Apply Changes",
            class = "btn_primary w-100"
          )
        )
      ),

        accordion_panel(
          title = "Download Options",
          p("Download options coming soon...")
        ),
    )
  )
}


# -------------------------
# Subset sidebar server
# -------------------------
mod_subset_sidebar_server <- function(id, tree_map) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # # 1. Define the categories we want to manage
    # categories <- list(
    #   "Cluster"     = "cluster_container",
    #   "Subcluster"  = "subcluster_container",
    #   "Sample"      = "sample_container",
    #   "Experiment"  = "experiment_container"
    # )

    # # 2. Create a list of reactiveVals to track the 'previous' state for each category
    # # We use 'setNames' to create: list(cluster = reactiveVal(NULL), ...)
    # prev_states <- lapply(setNames(names(categories), names(categories)), function(x) reactiveVal(NULL))

    # # 3. The Management Loop
    # # We use 'lapply' over the names to create an observer for each category
    # lapply(names(categories), function(cat_name) {
      
    #   # Determine the input ID (e.g., input$clusters, input$subclusters)
    #   # Note: Your UI IDs use plural (clusters), so we append 's'
    #   input_id <- paste0(cat_name, "s") 
    #   container_id <- categories[[cat_name]]

    #   observeEvent(input[[input_id]], ignoreNULL = FALSE, {
    #     current_vals  <- input[[input_id]]
    #     previous_vals <- prev_states[[cat_name]]()
        
    #     # --- Manage Additions ---
    #     to_add <- setdiff(current_vals, previous_vals)
    #     for (item in to_add) {
    #       insertUI(
    #         selector = paste0("#", ns(container_id)),
    #         where = "beforeEnd",
    #         ui = div(
    #           id = ns(paste0("wrapper_", cat_name, "_", item)),
    #           # Your color_picker_ui function
    #           color_picker_ui(ns(paste0("color_picker_", cat_name, "_", item)), label = item)
    #         )
    #       )
    #     }
        
    #     # --- Manage Removals ---
    #     to_remove <- setdiff(previous_vals, current_vals)
    #     for (item in to_remove) {
    #       removeUI(selector = paste0("#", ns(paste0("wrapper_", cat_name, "_", item))))
    #     }
        
    #     # Update the state tracker
    #     prev_states[[cat_name]](current_vals)
    #   })
    # })

    # eventReactive so downstream modules only update when "Apply Changes" is clicked.
    selections <- eventReactive(input$apply, {
      req(input$group_by, input$subset_tree)
      tree_raw <- input$subset_tree
      selected_labels <- unique(sapply(tree_raw, function(x) x$text[[1]] ))

      list(
        group_by    = input$group_by,
        experiments = intersect(selected_labels, tree_map$experiment),
        samples     = intersect(selected_labels, tree_map$orig.ident),
        clusters    = intersect(selected_labels, tree_map$seurat_clusters),
        subclusters = intersect(selected_labels, tree_map$subcluster)
      )
    }, ignoreNULL = FALSE) # Load defaults on start

    return(selections)

    # ===== Old olor picker code (was in reactive block?) =====
    # # Helper function to match labels to their dynamic color inputs
    # get_colors <- function(selections, prefix) {
    #   if (is.null(selections) || length(selections) == 0) return(NULL)
      
    #   # Map selection names to the values in the color picker inputs
    #   # Note: we use session$input to access inputs within this module
    #   vals <- sapply(selections, function(x) {
    #     # This ID must match exactly how you created it in renderUI
    #     input_id <- paste0("color_picker_", prefix, "_", x)
    #     val <- input[[input_id]]

    #     # Default to black if not yet rendered
    #     if (is.null(val)) {
    #       return("#000000") # default black
    #     } else {
    #       return(val)
    #     }
    #   })
      
    #   names(vals) <- selections
    #   return(vals)
    # }


    # list(
    #   clusters = get_colors(input$clusters, "cluster"),
    #   subclusters = get_colors(input$subclusters, "subcluster"),
    #   samples = get_colors(input$samples, "sample"),
    #   experiments = get_colors(input$experiments, "experiment")
    # )

  })
}