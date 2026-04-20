# -------------------------
# DEG UI
# -------------------------
mod_deg_ui <- function(id) {
    ns <- NS(id)

    page_fluid(
      fluidRow(
        column(
          width = 6,
          class = "d-flex justify-content-center",
          selectizeInput(
            ns("ident_1"),
            label = "Comparison Groups (ident.1):",
            choices = NULL,
            multiple = TRUE
          )
        ),
        column(
          width = 6,
          class = "d-flex justify-content-center",
          selectizeInput(
            ns("ident_2"),
            label = "Reference Groups (ident.2):",
            choices = NULL,
            multiple = TRUE
          )
        ),
        column(
          width = 6,
          class = "d-flex justify-content-center",
          numericInput(
            ns("pval_cutoff"),
            label = "Adjusted p-value Cutoff:",
            value = 0.05,
            min = 0,
            max = 1,
            step = 0.01
          )
        ),
        column(
          width = 6,
          class = "d-flex justify-content-evenly",
          numericInput(
            ns("lower_logfc_cutoff"),
            label = "Lower avg_log2FC Cutoff:",
            value = -0.2,
            step = 0.1,
            width = "33%"
          ),
          numericInput(
            ns("upper_logfc_cutoff"),
            label = "Upper avg_log2FC Cutoff:",
            value = 0.2,
            step = 0.1,
            width = "33%"
          )
        )
      ),

      column(
        width = 12,
        class = "d-flex justify-content-center",
        actionButton(
          ns("run_deg"),
          label = "Find DEGs",
          class = "btn-primary",
          icon = icon("play")
        )
      ),

      # Output: DEG results table
      dataTableOutput(
        ns("deg_table"),
        height = "450px"
      ) %>% with_custom_spinner(),

      hr(),

      accordion(
        id = ns("meta_accordion"),
        accordion_panel(
          title = "Included Data Meta Table",
          # Output: Metadata table
          dataTableOutput(ns("meta_table")) %>% with_custom_spinner()
        ),
        accordion_panel(
          title = "Relevant Code",
          code_block(deg_code)
        ),
        open = FALSE,
        class = "mb-5"
      )

    )
}


# -------------------------
# DEG server
# -------------------------
mod_deg_server <- function(id, global_state) {
  moduleServer(id, function(input, output, session) {

    # Update ident.1 and ident.2 choices
    observe({
      req(global_state())
      state <- global_state()

      # Update ident.1 and ident.2 choices based on current group_by levels
      idents <- list(
        Experiments = setNames(
          paste0("experiment::", state$experiment),
          state$experiment
        ),
        Samples = setNames(
          paste0("orig.ident::", state$orig.ident),
          state$orig.ident
        ),
        Clusters = setNames(
          paste0("seurat_clusters::", state$seurat_clusters),
          state$seurat_clusters
        ),
        Subclusters = setNames(
          paste0("subcluster::", state$subcluster),
          state$subcluster
        )
      )

      updateSelectizeInput(
        session,
        "ident_1",
        choices = idents
      )
      updateSelectizeInput(
        session,
        "ident_2",
        choices = idents
      )
    })

    # Run DEG on button press
    # 2. Run the DEG analysis ONLY when the button is clicked
    deg_results <- eventReactive(input$run_deg, {
      # Require the user to have selected at least one group for each
      req(input$ident_1, input$ident_2, global_state()) 

      validate(
        need(input$pval_cutoff >= 0 && input$pval_cutoff <= 1, "Please enter a valid p-value cutoff between 0 and 1."),
        need(input$lower_logfc_cutoff < input$upper_logfc_cutoff, "Please ensure the lower logFC cutoff is less than the upper logFC cutoff.")
      )
      
      # Show a loading notification since FindMarkers takes time
      showNotification("Calculating DEGs... this may take a moment.", id = "deg_calc", duration = NULL, type = "message")
      on.exit(removeNotification("deg_calc"), add = TRUE)
      
      # Get the current subsetted object
      state <- global_state()

      # strip prefixes from idents and remove dupes
      strip_prefix <- function(x) sub("^[^:]+::", "", x)
      ident_1_vals <- unique(strip_prefix(input$ident_1))
      ident_2_vals <- unique(strip_prefix(input$ident_2))
      
      ## EXAMPLE -
      # If we wanted to get DEGs for satellite cells in LLC and C26 models compared to controls.
      # 1. Subset for Satellite cells cluster (during subset step).
      # 2. Set IDENTS as "orig.ident".
      # 3. Then #name_comparison_1 = Agca_snLLC_LLC, #name_comparison_2 = Brown_scLLC_3.5w,
      #    #name_comparison_3 = Pryce_scC26_C26;
      # 4. #name_reference_1 = Agca_snLLC_CTL, #name_reference_2 = Brown_scLLC_CTL,
      #    #name_reference_3 = Pryce_scC26_CTL.
      print("===============================")
      print(paste("Running FindMarkers", Sys.time()))
      print("Comparison groups (ident.1):")
      print(ident_1_vals)
      print("Reference groups (ident.2):")
      print(ident_2_vals)

      # Run FindMarkers (fetching all, filtering happens next)
      raw_degs <- FindMarkers(
        state$sc_subset,
        group.by = state$group_by,
        ident.1 = ident_1_vals,
        ident.2 = ident_2_vals,
        logfc.threshold = 0,
        min.pct = 0.10,
        verbose = TRUE
      )
      print(paste("Finished FindMarkers", Sys.time()))
      

      print(paste("Filtering...", Sys.time()))
      # Apply user-defined cutoffs
      filtered_degs <- subset(
        na.omit(raw_degs), 
        # p_val_adj < input$pval_cutoff & abs(avg_log2FC) > input$logfc_cutoff # old filter
        p_val_adj < input$pval_cutoff &
        avg_log2FC > input$lower_logfc_cutoff &
        avg_log2FC < input$upper_logfc_cutoff
      )
      print(paste("Done!", Sys.time()))
      print("===============================")
      
      return(filtered_degs)
    })

    # 3. Render the output table
    output$deg_table <- renderDataTable(
      width = "100%",
      options = list(
        paging = FALSE,
        scrollY = "300px",
        scrollCollapse = TRUE
      ),
      rownames = TRUE,
      {
      # Extracts the reactive data frame
      # req(deg_results())

      deg_results <- deg_results()

      print(deg_results)

      validate(
        need(!is.null(deg_results), "Run the DEG analysis to see results here."),
        # need(nrow(deg_results) > 0, "No DEGs found with the current cutoffs. Try adjusting the p-value or logFC thresholds.")
      )

      deg_results
    })

    # Populate placeholder metadata table
    output$meta_table <- renderDataTable(
      width = "100%",
      # content:
      {
        req(global_state())

        state <- global_state()

        state$sc_subset@meta.data %>%
          group_by(experiment, orig.ident, seurat_clusters, subcluster) %>%
          summarise(n_cells = n(), .groups = "drop") %>%
          arrange(experiment, orig.ident, seurat_clusters)

          # # Optimization: Use data.table or fast dplyr for the summary
          # sc_object()@meta.data %>%
          #   count(experiment, orig.ident, seurat_clusters, subcluster) %>%
          #   arrange(experiment, orig.ident, seurat_clusters)
          # ?????????????????
      }
    )

  })
}



# # 4. Handle the CSV download
# output$download_deg <- downloadHandler(
#   filename = function() {
#     paste0("DEGs_Comparison_", Sys.Date(), ".csv")
#   },
#   content = function(file) {
#     # write.csv includes rownames by default, which preserves the Gene symbols
#     write.csv(deg_results(), file, row.names = TRUE) 
#   }
# )