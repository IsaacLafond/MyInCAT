# -------------------------
# CellChat Plots UI
# -------------------------
mod_cellchat_plots_ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    radioGroupButtons(
      inputId = ns("current_plot"),
      status = "outline-primary btn-sm",
      choices = c("Heatmap", "Circle", "Chord"),
      selected = "Heatmap",
      justified = TRUE
    ),

    conditionalPanel(
      condition = "input.current_plot == 'Heatmap'",
      ns = ns,
      # content:
      card(
        fill = TRUE,
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center",
          # title if interaction
          conditionalPanel(
            condition = "input.current_heatmap == 'Interaction'",
            ns = ns,
            "Interaction Heatmap",
          ),
          # title if contribution
          conditionalPanel(
            condition = "input.current_heatmap == 'Contribution'",
            ns = ns,
            "Contribution Heatmap",
          ),
          div(
            class = "d-flex gap-3",

            # select between active heatmap
            radioGroupButtons(
              inputId = ns("current_heatmap"),
              choices = c("Interaction", "Contribution"),
              selected = "Interaction",
              justified = FALSE,
              size = "sm"
            ),

            actionButton(
              ns("show_heatmap_code"),
              icon = icon("code"),
              label = "View Code",
              class = "btn-sm",
              disabled = TRUE
            ),

            actionButton(
              ns("heatmap_download_wrapper"),
              label = downloadButton(
                ns("download_heatmap"),
                class = "btn-sm",
              ),
              class = "p-0 border-0",
              disabled = TRUE
            )
          )
        ),
        card_body(
          div(
            class = "h-100",
            style = "aspect-ratio: 1 / 1; min-width: 500px;",
            # content
            conditionalPanel(
              condition = "input.current_heatmap == 'Interaction'",
              ns = ns,
              # content:
              plotOutput(
                ns("interaction_heatmap_plot"),
                width = "100%",
                height = "100%",
                fill = TRUE
              ) %>% with_custom_spinner()
            ),
            conditionalPanel(
              condition = "input.current_heatmap == 'Contribution'",
              ns = ns,
              # content:
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
                height = "100%",
                fill = TRUE
              ) %>% with_custom_spinner()
            )
          )
        )
      )
    ),

    conditionalPanel(
      condition = "input.current_plot == 'Circle'",
      ns = ns,
      # content:
      card(
        fill = TRUE,
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center",
          # title if interaction
          "Circle Plot",

          div(
            class = "d-flex gap-3",

            popover(
              trigger = actionButton(
                ns("circle_settings_trigger"),
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
                  ns("circle_source_cells"),
                  label = "Source Cells:",
                  stateInput = TRUE,
                  choices = NULL,
                  multiple = TRUE
                ),
                pickerInput(
                  ns("circle_target_cells"),
                  label = "Target Cells:",
                  stateInput = TRUE,
                  choices = NULL,
                  multiple = TRUE
                ),
                # checkbox to set label.edge T/F for circle plot
                checkboxInput(
                  ns("circle_label_edge"),
                  label = "Show Edge Labels",
                  value = TRUE
                )
              )
            ),

            actionButton(
              ns("show_circle_code"),
              icon = icon("code"),
              label = "View Code",
              class = "btn-sm",
              disabled = TRUE
            ),

            actionButton(
              ns("circle_download_wrapper"),
              label = downloadButton(
                ns("download_circle"),
                class = "btn-sm",
              ),
              class = "p-0 border-0",
              disabled = TRUE
            )
          )
        ),
        card_body(
          div(
            class = "h-100",
            style = "aspect-ratio: 1 / 1; min-width: 500px;",
            # content:
            plotOutput(
              ns("circle_plot"),
              width = "100%",
              height = "100%",
              fill = TRUE
            ) %>% with_custom_spinner()
          )
        )
      )
    ),

    conditionalPanel(
      condition = "input.current_plot == 'Chord'",
      ns = ns,
      # content:
      card(
        fill = TRUE,
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center",
          # title if interaction
          "Chord Plot",

          div(
            class = "d-flex gap-3",

            popover(
              trigger = actionButton(
                ns("chord_settings_trigger"),
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
                  ns("chord_pathway"),
                  label = "Select Pathway:",
                  stateInput = TRUE,
                  choices = NULL,
                  multiple = FALSE
                ),
                pickerInput(
                  ns("chord_source_cells"),
                  label = "Source Cells:",
                  stateInput = TRUE,
                  choices = NULL,
                  multiple = TRUE
                ),
                pickerInput(
                  ns("chord_target_cells"),
                  label = "Target Cells:",
                  stateInput = TRUE,
                  choices = NULL,
                  multiple = TRUE
                )
              )
            ),

            actionButton(
              ns("show_chord_code"),
              icon = icon("code"),
              label = "View Code",
              class = "btn-sm",
              disabled = TRUE
            ),

            actionButton(
              ns("chord_download_wrapper"),
              label = downloadButton(
                ns("download_chord"),
                class = "btn-sm",
              ),
              class = "p-0 border-0",
              disabled = TRUE
            )
          )
        ),
        card_body(
          div(
            class = "h-100",
            style = "aspect-ratio: 1 / 1; min-width: 500px;",
            # content:
            plotOutput(
              ns("chord_plot"),
              width = "100%",
              height = "100%",
              fill = TRUE
            ) %>% with_custom_spinner()
          )
        )
      )
    )
  )
}

# -------------------------
# CellChat Plots server
# -------------------------
mod_cellchat_plots_server <- function(id, cellchat_object, sidebar_selections) {
  moduleServer(id, function(input, output, session) {

    circle_src_on_close <- reactiveVal(NULL)
    circle_tgt_on_close <- reactiveVal(NULL)
    chord_src_on_close <- reactiveVal(NULL)
    chord_tgt_on_close <- reactiveVal(NULL)

    # -------------------------
    # Validity + button toggling
    # -------------------------

    # Heatmap: one pair of buttons covers whichever sub-view is active
    interaction_heatmap_valid <- reactive({
      tryCatch(!is.null(cellchat_object()@net$weight), error = function(e) FALSE)
    })
    contribution_heatmap_valid <- reactive({
      tryCatch(!is.null(cellchat_object()) && length(input$heatmap_pattern) > 0, error = function(e) FALSE)
    })
    heatmap_valid <- reactive({
      if (identical(input$current_heatmap, "Contribution")) {
        contribution_heatmap_valid()
      } else {
        interaction_heatmap_valid()
      }
    })
    observe({
      updateActionButton(session, "show_heatmap_code", disabled = !heatmap_valid())
      updateActionButton(session, "heatmap_download_wrapper", disabled = !heatmap_valid())
    })

    circle_valid <- reactive({
      tryCatch(
        !is.null(cellchat_object()@net$count) &&
          length(circle_src_on_close()) > 0 &&
          length(circle_tgt_on_close()) > 0,
        error = function(e) FALSE
      )
    })
    observe({
      updateActionButton(session, "show_circle_code", disabled = !circle_valid())
      updateActionButton(session, "circle_download_wrapper", disabled = !circle_valid())
    })

    chord_valid <- reactive({
      tryCatch(
        length(input$chord_pathway) > 0 &&
          length(chord_src_on_close()) > 0 &&
          length(chord_tgt_on_close()) > 0,
        error = function(e) FALSE
      )
    })
    observe({
      updateActionButton(session, "show_chord_code", disabled = !chord_valid())
      updateActionButton(session, "chord_download_wrapper", disabled = !chord_valid())
    })

    observe({
      req(cellchat_object())
      x <- cellchat_object()

      idents_levels <- levels(x@idents)
      pathway_default <- x@netP$pathways[1]
      idx_25 <- ceiling(length(idents_levels) * 0.25)
      idx_50 <- ceiling(length(idents_levels) * 0.5)

      updatePickerInput(
        session,
        "circle_source_cells",
        choices = idents_levels,
        selected = idents_levels[1:idx_50]
      )
      updatePickerInput(
        session,
        "circle_target_cells",
        choices = idents_levels,
        selected = idents_levels[1:idx_50]
      )

      updatePickerInput(
        session,
        "chord_pathway",
        choices = x@netP$pathways,
        selected = pathway_default
      )
      updatePickerInput(
        session,
        "chord_source_cells",
        choices = idents_levels,
        selected = idents_levels[1:idx_25]
      )
      updatePickerInput(
        session,
        "chord_target_cells",
        choices = idents_levels,
        selected = idents_levels[1:idx_25]
      )

      circle_src_on_close(idents_levels[1:idx_50])
      circle_tgt_on_close(idents_levels[1:idx_50])
      chord_src_on_close(idents_levels[1:idx_25])
      chord_tgt_on_close(idents_levels[1:idx_25])

    })

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

      # get plot dimmensions (in px)
      width_px <- session$clientData[[paste("output", session$ns("contribution_heatmap_plot_width"), sep = "_")]]
      height_px <- session$clientData[[paste("output", session$ns("contribution_heatmap_plot_height"), sep = "_")]]
      # convert pixels to cm
      width_cm <- (width_px / 72) * 2.54 # 72 dpi renderPlot default, 2.54 cm per inch
      height_cm <- (height_px / 72) * 2.54 # 72 dpi renderPlot default, 2.54 cm per inch
      
      netAnalysis_signalingRole_heatmap(
        cellchat_object(),
        pattern = input$heatmap_pattern,
        slot.name = "netP",
        color.heatmap = "Reds",
        # color.use = 
        width = width_cm - 6,
        height = height_cm - 6,
        # font.size = 3
      )
    })

    observeEvent(input$circle_source_cells_open, {
      if (isFALSE(input$circle_source_cells_open)) {
        circle_src_on_close(input$circle_source_cells)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$circle_target_cells_open, {
      if (isFALSE(input$circle_target_cells_open)) {
        circle_tgt_on_close(input$circle_target_cells)
      }
    }, ignoreInit = TRUE)

    output$circle_plot <- renderPlot({
      req(
        cellchat_object()@net$count,
        circle_src_on_close(),
        circle_tgt_on_close(),
        !is.null(input$circle_label_edge)
      )

      validate(
        need(
          length(circle_src_on_close()) > 0,
          "Please select at least one source cell type."
        ),
        need(
          length(circle_tgt_on_close()) > 0,
          "Please select at least one target cell type."
        )
      )

      netVisual_circle(
        cellchat_object()@net$count,
        weight.scale = TRUE,
        label.edge = input$circle_label_edge,
        title.name = "Number of interactions",
        arrow.size = 0.05,
        # color.use = cluster_colours, 
        sources.use = circle_src_on_close(), # Specify cell types SENDING signals or leave blank for all
        targets.use = circle_tgt_on_close() # Specify cell types RECIEVING signals or leave blank for all
      )

    })

    observeEvent(input$chord_source_cells_open, {
      if (isFALSE(input$chord_source_cells_open)) {
        chord_src_on_close(input$chord_source_cells)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$chord_target_cells_open, {
      if (isFALSE(input$chord_target_cells_open)) {
        chord_tgt_on_close(input$chord_target_cells)
      }
    }, ignoreInit = TRUE)

    output$chord_plot <- renderPlot({
      req(
        cellchat_object(),
        input$chord_pathway,
        chord_src_on_close(),
        chord_tgt_on_close()
      )
      cellchat_obj <- cellchat_object()

      validate(
        need(
          length(chord_src_on_close()) > 0,
          "Please select at least one source cell type."
        ),
        need(
          length(chord_tgt_on_close()) > 0,
          "Please select at least one target cell type."
        )
      )

      # par(mfrow = c(1, 1), xpd = TRUE)

      netVisual_chord_gene(
        cellchat_obj,
        signaling = input$chord_pathway,
        sources.use = chord_src_on_close(),
        targets.use = chord_tgt_on_close(),
        # color.use = cluster_colours,
        show.legend = TRUE,
        lab.cex = 0.5
      )
    })

    # Download handlers
    output$download_heatmap <- downloadHandler(
      filename = function() {
        filename_map <- c(
          "Interaction" = paste0("cellchat_interaction_heatmap_", Sys.Date(), ".png"),
          "Contribution" = paste0("cellchat_contribution_heatmap_", Sys.Date(), ".png")
        )
        filename_map[[input$current_heatmap]]
      },
      content = function(file) {
        if (input$current_heatmap == "Interaction") {
          png(file, width = 2000, height = 2000, res = 300)
          print(netVisual_heatmap(
            cellchat_object(),
            slot.name = "net",
            measure = "weight",
            color.heatmap = "Reds"
          ))
          dev.off()
        } else if (input$current_heatmap == "Contribution") {
          png(file, width = 3000, height = 3000, res = 300)
          print(netAnalysis_signalingRole_heatmap(
            cellchat_object(),
            pattern = input$heatmap_pattern,
            slot.name = "netP",
            color.heatmap = "Reds",
            width = 20,
            height = 20
          ))
          dev.off()
        }
      }
    )

    output$download_circle <- downloadHandler(
      filename = function() {
        paste0("cellchat_circle_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        png(file, width = 2000, height = 2000, res = 300)
        print(netVisual_circle(
          cellchat_object()@net$count,
          weight.scale = TRUE,
          label.edge = input$circle_label_edge,
          title.name = "Number of interactions",
          arrow.size = 0.05,
          sources.use = circle_src_on_close(),
          targets.use = circle_tgt_on_close()
        ))
        dev.off()
      }
    )

    output$download_chord <- downloadHandler(
      filename = function() {
        paste0("cellchat_chord_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        png(file, width = 3000, height = 2000, res = 300)
        print(netVisual_chord_gene(
          cellchat_object(),
          signaling = input$chord_pathway,
          sources.use = chord_src_on_close(),
          targets.use = chord_tgt_on_close(),
          show.legend = TRUE,
          lab.cex = 0.5
        ))
        dev.off()
      }
    )

    # Modals for code viewing
    observeEvent(input$show_heatmap_code, {
      req(
        sidebar_selections(),
        input$current_heatmap
      )
      heatmap_code_map <- c(
        "Interaction" = generate_interaction_heatmap_code(sidebar_selections),
        "Contribution" = generate_contribution_heatmap_code(sidebar_selections, input$heatmap_pattern)
      )
      heatmap_code <- heatmap_code_map[[input$current_heatmap]]

      showModal(modalDialog(
        title = "Source Code",
        size = "xl",
        code_block(heatmap_code),
        easyClose = TRUE
      ))
    })

    observeEvent(input$show_circle_code, {
      req(
        sidebar_selections(),
        circle_src_on_close(),
        circle_tgt_on_close(),
        !is.null(input$circle_label_edge)
      )
      circle_code <- generate_circle_code(
        sidebar_selections,
        input$circle_label_edge,
        circle_src_on_close(),
        circle_tgt_on_close()
      )

      showModal(modalDialog(
        title = "Source Code",
        size = "xl",
        code_block(circle_code),
        easyClose = TRUE
      ))
    })

    observeEvent(input$show_chord_code, {
      req(
        sidebar_selections(),
        input$chord_pathway,
        chord_src_on_close(),
        chord_tgt_on_close()
      )
      chord_code <- generate_chord_code(
        sidebar_selections,
        input$chord_pathway,
        chord_src_on_close(),
        chord_tgt_on_close()
      )

      showModal(modalDialog(
        title = "Source Code",
        size = "xl",
        code_block(chord_code),
        easyClose = TRUE
      ))
    })

  })
}