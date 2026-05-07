# -------------------------
# Sidebar UI
# -------------------------
mod_sidebar_ui <- function(id, all_choices) {
  ns <- NS(id)

  picker_input_options <- list(
    "live-search" = TRUE,
    "container" = "body",
    "dropup-auto" = FALSE,
    "dropdown-align-right" = "auto"
  )

  page_fluid(

    conditionalPanel(
      condition = "input.main_navbar != 'cellchat'",
      # content:
      selectInput(
        ns("group_by"),
        "Compare between:",
        choices = c(
          "Sample" = "orig.ident",
          "Cluster" = "seurat_clusters",
          "Subcluster" = "subcluster"
        )
      ),

      hr(),
      h6("Subset Data"),

      pickerInput(
        inputId = ns("subset_samples"),
        label = "1. Sample:",
        choices = all_choices$samples,
        selected = all_choices$samples, # Start with all selected
        multiple = TRUE,
        options = picker_input_options
      ),
      pickerInput(
        inputId = ns("subset_clusters"),
        label = "2. Cluster:",
        choices = all_choices$clusters,
        selected = all_choices$clusters, # Start with all selected
        multiple = TRUE,
        options = picker_input_options
      ),
      pickerInput(
        inputId = ns("subset_subclusters"),
        label = "3. Subcluster:",
        choices = all_choices$subclusters,
        selected = all_choices$subclusters, # Start with all selected
        multiple = TRUE,
        options = picker_input_options
      ),

      actionButton(
        inputId = ns("reset_subset"),
        label = "Reset",
        class = "w-100"
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

    hr(),

    actionButton(
      inputId = ns("apply"),
      label = "Apply Changes",
      class = "btn-primary w-100 mb-3"
    )

    # accordion(
    #   accordion_panel(
    #     title = "Subset Options",

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
# Sidebar server
# -------------------------
mod_sidebar_server <- function(
  id,
  current_tab,
  all_choices,
  map_sample_to_cluster,
  map_sample_cluster_to_subcluster
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Update Clusters when Samples change
    observeEvent(input$subset_samples, {
      # O(1) Lookup: Pull valid clusters from the hash map list
      if(is.null(input$subset_samples)) {
        valid_clusters <- character(0)
      } else {
        # unlist extracts the values from the named list instantly
        valid_clusters <- unique(unlist(map_sample_to_cluster[input$subset_samples], use.names = FALSE))
      }

      # Create a boolean vector for which choices should be disabled
      disabled_flags <- !(all_choices$clusters %in% valid_clusters)
      
      # Retain previously selected clusters if they are still valid
      current_selected <- isolate(input$subset_clusters)
      new_selected <- intersect(current_selected, valid_clusters)
      
      # Failsafe: if intersection is empty, select all valid options
      if(length(new_selected) == 0 && length(valid_clusters) > 0) new_selected <- valid_clusters

      # Update the UI: keep choices static, but update the disabled states
      updatePickerInput(
        session = session,
        inputId = "subset_clusters",
        choices = all_choices$clusters,
        selected = new_selected,
        choicesOpt = list(disabled = disabled_flags)
      )
    }, ignoreNULL = FALSE)

    # 2. Update Subclusters when Samples OR Clusters change
    observeEvent(c(input$subset_samples, input$subset_clusters), {
      
      if(is.null(input$subset_samples) || is.null(input$subset_clusters)) {
        valid_subs <- character(0)
      } else {
        # Create lookup keys using R's highly optimized `outer` function
        # e.g., "SampleA||Cluster1", "SampleB||Cluster1"
        valid_keys <- as.character(outer(input$subset_samples, input$subset_clusters, FUN = paste, sep = "||"))
        
        # O(1) Lookup: Pull valid subclusters from the hash map
        valid_subs <- unique(unlist(map_sample_cluster_to_subcluster[valid_keys], use.names = FALSE))
      }

      # Create boolean vector for disabled options
      disabled_flags <- !(all_choices$subclusters %in% valid_subs)
      
      current_selected <- isolate(input$subset_subclusters)
      new_selected <- intersect(current_selected, valid_subs)
      if(length(new_selected) == 0 && length(valid_subs) > 0) new_selected <- valid_subs

      updatePickerInput(
        session = session,
        inputId = "subset_subclusters",
        choices = all_choices$subclusters,
        selected = new_selected,
        choicesOpt = list(disabled = disabled_flags)
      )
    }, ignoreNULL = FALSE)

    observeEvent(input$reset_subset, {
      updatePickerInput(
        session = session,
        inputId = "subset_samples",
        choices = all_choices$samples,
        selected = all_choices$samples,
      )
      updatePickerInput(
        session = session,
        inputId = "subset_clusters",
        choices = all_choices$clusters,
        selected = all_choices$clusters,
        choicesOpt = list(
          disabled = rep(FALSE, length(all_choices$clusters))
        )
      )
      updatePickerInput(
        session = session,
        inputId = "subset_subclusters",
        choices = all_choices$subclusters,
        selected = all_choices$subclusters,
        choicesOpt = list(
          disabled = rep(FALSE, length(all_choices$subclusters))
        )
      )
    })

    # Return values
    seurat_subset_params <- reactiveVal(NULL)
    seurat_group_by <- reactiveVal(NULL)
    cellchat_state <- reactiveVal(NULL)

    observeEvent(input$apply, {
      # Require all Seurat subset inputs OR cellchat inputs depending on the tab
      if(current_tab() != "cellchat") {
         req(input$group_by, input$subset_samples, input$subset_clusters, input$subset_subclusters)
      } else {
         req(input$cellchat_sample, input$cellchat_interaction_type)
      }

      get_subset_only <- function() {
        list(
          orig.ident  = input$subset_samples,
          seurat_clusters = input$subset_clusters,
          subcluster = input$subset_subclusters
        )
      }

      get_cellchat_selections <- function() {
        list(
          sample = input$cellchat_sample,
          interaction_type = input$cellchat_interaction_type
        )
      }

      if (input$apply == 0) {
        seurat_subset_params(get_subset_only())
        seurat_group_by(input$group_by)
        cellchat_state(get_cellchat_selections())
      } else if (current_tab() == "cellchat") {
        cellchat_state(get_cellchat_selections())
      } else {
        seurat_subset_params(get_subset_only())
        seurat_group_by(input$group_by)
      }
    }, ignoreNULL = FALSE) 

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