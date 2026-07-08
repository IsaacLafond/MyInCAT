# -------------------------
# DEG UI
# -------------------------
mod_deg_ui <- function(id) {
  ns <- NS(id)

  degs_settings_popover <- popover(
    trigger = actionButton(
      ns("settings_trigger"),
      label = NULL,
      icon = icon("gear"),
      class = "btn-sm btn-light"
    ),
    placement = "auto",
    # options = list(
    #   trigger = "click focus"
    # ),
    # popover content:
    fluidRow(
      pickerInput(
        ns("ident_1"),
        label = "Comparison Groups (ident.1):",
        choices = NULL,
        multiple = TRUE,
        width = 250,
        options = list(
          "actions-box" = TRUE,
          "live-search" = TRUE
        )
      ),
      pickerInput(
        ns("ident_2"),
        label = "Reference Groups (ident.2):",
        choices = NULL,
        multiple = TRUE,
        width = 250,
        options = list(
          "actions-box" = TRUE,
          "live-search" = TRUE
        )
      ),
      numericInput(
        ns("pval_cutoff"),
        label = "Adjusted p-value Cutoff:",
        width = 250,
        value = 0.05,
        min = 0,
        max = 1,
        step = 0.01
      ),
      numericInput(
        ns("lower_logfc_cutoff"),
        label = "Lower avg_log2FC Cutoff:",
        value = -0.2,
        step = 0.1,
        width = "50%"
      ),
      numericInput(
        ns("upper_logfc_cutoff"),
        label = "Upper avg_log2FC Cutoff:",
        value = 0.2,
        step = 0.1,
        width = "50%"
      )
    )
  )

  card(
    fill = TRUE,
    full_screen = TRUE,
    card_header(
      class = "d-flex justify-content-between align-items-center",

      "Differentially Expressed Genes",

      div(
        class = "d-flex gap-3",
        # content:
        degs_settings_popover,

        actionButton(
          ns("show_code"),
          icon = icon("code"),
          label = "View Code",
          class = "btn-sm"
        ),
        
        actionButton(
          ns("download_wrapper"),
          label = downloadButton(
            ns("download_table"),
            class = "btn-sm"
          ),
          class = "p-0 border-0",
          disabled = TRUE
        ),

        actionButton(
          ns("run_deg"),
          label = "Find DEGs",
          class = "btn-sm btn-primary",
          icon = icon("play")
        )
      )

    ),
    card_body(
      div(
        class = "d-flex h-100 justify-content-center align-items-center",
        # content
        dataTableOutput(
          ns("deg_table")
        ) %>% with_custom_spinner(
          hide.ui = TRUE,
          caption = "Calculating DEGs... this may take a moment."
        )
      )
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
      idents <- state[[state$group_by]]

      updatePickerInput(
        session,
        "ident_1",
        choices = idents
      )
      updatePickerInput(
        session,
        "ident_2",
        choices = idents
      )
    })

    # Run DEG on button press
    # 2. Run the DEG analysis ONLY when the button is clicked
    deg_results <- eventReactive(input$run_deg, {
      # Require the user to have selected at least one group for each
      req(global_state())

      validate(
        need(length(input$ident_1) > 0, "Please select at least one group for Comparison (ident.1)."),
        need(length(input$ident_2) > 0, "Please select at least one group for Reference (ident.2)."),
        need(input$ident_1 != input$ident_2, "Comparison and Reference groups cannot be the same."),
        need(input$pval_cutoff >= 0 && input$pval_cutoff <= 1, "Please enter a valid p-value cutoff between 0 and 1."),
        need(input$lower_logfc_cutoff < input$upper_logfc_cutoff, "Please ensure the lower logFC cutoff is less than the upper logFC cutoff.")
      )
      
      # Get the current subsetted object
      state <- global_state()

      ident_1_vals <- unique(input$ident_1)
      ident_2_vals <- unique(input$ident_2)
      
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
    }, ignoreNULL = FALSE)

    # 3. Render the output table
    output$deg_table <- renderDataTable(
      rownames = TRUE,
      options = datatable_options,
      {
        deg_results()
    })

    observe({
      updateActionButton(
        session,
        "download_wrapper",
        disabled = TRUE
      )
      updateActionButton(
        session,
        "show_code",
        disabled = TRUE
      )
      if (!is.null(deg_results()) && nrow(deg_results()) > 0) {
        updateActionButton(
          session,
          "download_wrapper",
          disabled = FALSE
        )
        updateActionButton(
          session,
          "show_code",
          disabled = FALSE
        )
      }
    })

    output$download_table <- downloadHandler(
      filename = function() {
        paste0("DEGs", Sys.time(), ".csv")
      },
      content = function(file) {
        write.csv(deg_results(), file)
      }
    )

    # Modals
    observeEvent(input$show_code, {
      req(
        global_state(),
        input
      )
      deg_code <- generate_deg_code(global_state(), input)

      showModal(modalDialog(
        title = "Source Code",
        size = "xl",
        code_block(deg_code),
        easyClose = TRUE
      ))
    })

    DEGs <- reactive({
      req(deg_results())
      results <- deg_results()

      list(
        up = results %>% filter(avg_log2FC > 0) %>% rownames(),
        down = results %>% filter(avg_log2FC < 0) %>% rownames()
      )
    })

    return(DEGs)

  })
}
