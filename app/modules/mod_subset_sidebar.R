# -------------------------
# Subset sidebar UI
# -------------------------
mod_subset_sidebar_ui <- function(id, tree_data) {
  ns <- NS(id)

  page_fluid(

    conditionalPanel(
      condition = "input.main_navbar != 'cellchat'",
      # content:
      selectInput(
        ns("group_by"),
        "Group by:",
        choices = names(groupby_choices)
      ),

      treeInput(
        inputId = ns("subset_tree"),
        label = "Select Desired Elements:",
        choices = create_tree(tree_data),
        selected = tree_data$experiment,
        # closeDepth = 4,
        returnValue = "all"
        # returnValue = c("text", "id", "all"),
      )
    ),

    conditionalPanel(
      condition = "input.main_navbar == 'cellchat'",
      # content:
      selectInput(
        ns("cellchat_sample"),
        label = "Select sample:",
        choices = c(
          "Agca_snLLC_CTL",
          "Agca_snLLC_LLC",
          "Brown_scLLC_CTL",
          "Brown_scLLC_2w",
          "Brown_scLLC_2.5w",
          "Brown_scLLC_3.5w",
          "Kim_scB16F10_CTL",
          "Kim_scB16F10_B16F10",
          "Pryce_scC26_CTL",
          "Pryce_scC26_C26",
          "Pryce_scKPP_CTL",
          "Pryce_scKPP_KPP",
          "Zhang_snKIC_CTL",
          "Zhang_snKIC_KIC"
        )
      ),

      selectInput(
        ns("cellchat_interaction_type"),
        label = "Select interaction type:",
        choices = c(
          "all",
          "secreted",
          "ECM",
          "ccc",
          "nps"
        )
      )
    ),

    actionButton(
      inputId = ns("apply"),
      label = "Apply Changes",
      class = "btn_primary w-100 mb-3"
    )

    # accordion(
    #   accordion_panel(
    #     title = "Subset Options",

        # pickerInput(
        #   inputId = ns("experiments"),
        #   label = "Experiments:",
        #   choices = experiments,
        #   selected = experiments,
        #   options = pickerOptions(

        # div(id = ns("experiment_container"))

        # div(id = ns("sample_container")),

        # div(id = ns("cluster_container")),
        
        # div(id = ns("subcluster_container")),

      # ),

        # accordion_panel(
        #   title = "Download Options",
        #   p("Download options coming soon...")
        # ),
    # )
  )
}


# -------------------------
# Subset sidebar server
# -------------------------
mod_subset_sidebar_server <- function(id, current_tab, tree_map) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # output$current_tab <- reactive({ current_tab() })
    # outputOptions(output, "current_tab", suspendWhenHidden = FALSE)

    seurat_subset_params <- reactiveVal(NULL)
    seurat_group_by <- reactiveVal(NULL)
    cellchat_state <- reactiveVal(NULL)

    observeEvent(input$apply, {
      req(
        input$group_by,
        input$subset_tree,
        input$cellchat_sample,
        input$cellchat_interaction_type
      )

      get_subset_only <- function() {
        tree_raw <- input$subset_tree
        selected_labels <- unique(sapply(tree_raw, function(x) x$text[[1]] ))

        list(
          experiments = tree_map$experiment[tree_map$experiment %in% selected_labels],
          orig.ident  = tree_map$orig.ident[tree_map$orig.ident %in% selected_labels],
          seurat_clusters = tree_map$seurat_clusters[tree_map$seurat_clusters %in% selected_labels],
          subcluster = tree_map$subcluster[tree_map$subcluster %in% selected_labels]
        )
      }

      get_cellchat_selections <- function() {
        list(
          sample = input$cellchat_sample,
          interaction_type = input$cellchat_interaction_type
        )
      }

      if (input$apply ==0) {
        # Initial load
        seurat_subset_params(get_subset_only())
        seurat_group_by(input$group_by)
        cellchat_state(get_cellchat_selections())
      } else if (current_tab() == "cellchat") {
        # Only update CellChat state
        cellchat_state(get_cellchat_selections())
      } else {
        # Update Seurat subset and group_by
        seurat_subset_params(get_subset_only())
        seurat_group_by(input$group_by)
      }
    }, ignoreNULL = FALSE) # Load defaults on start

    return(list(
      seurat_subset = seurat_subset_params,
      seurat_groupby = seurat_group_by,
      cellchat = cellchat_state
    ))

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