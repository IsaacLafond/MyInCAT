# -------------------------
# DEG GO UI
# -------------------------
mod_deg_go_ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    radioGroupButtons(
        inputId = ns("term_type"),
        status = "outline-primary btn-sm",
        choices = c(
          "<i class='fas fa-arrow-up me-1'></i> Up" = "Up",
          "<i class='fas fa-arrow-down me-1'></i> Down" = "Down"
        ),
        selected = "Up",
        justified = TRUE
    ),

    card(
      fill = TRUE,
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between align-items-center",
        conditionalPanel(
          condition = "input.term_type == 'Up'",
          ns = ns,
          "Up GO Terms"
        ),
        conditionalPanel(
          condition = "input.term_type == 'Down'",
          ns = ns,
          "Down GO Terms"
        ),

        div(
          class = "d-flex gap-3",

          radioGroupButtons(
            inputId = ns("output_type"),
            status = "outline-primary btn-sm",
            choices = c(
              "<i class='fas fa-table me-1'></i> Table" = "table",
              "<i class='fas fa-chart-line me-1'></i> Plot" = "plot"
            ),
            selected = "table"
          ),

          actionButton(
            ns("show_code"),
            icon = icon("code"),
            label = "View Code",
            class = "btn-sm"
          ),

          actionButton(
            ns("download_wrapper"),
            label = downloadButton(
              ns("download_go"),
              class = "btn-sm"
            ),
            class = "p-0 border-0",
            disabled = TRUE
          ),

          conditionalPanel(
            condition = "input.term_type == 'Up'",
            ns = ns,
            actionButton(
              ns("run_go_up"),
              label = "Find Terms",
              class = "btn-primary btn-sm",
              icon = icon("play")
            )
          ),
          conditionalPanel(
            condition = "input.term_type == 'Down'",
            ns = ns,
            actionButton(
              ns("run_go_down"),
              label = "Find Terms",
              class = "btn-primary btn-sm",
              icon = icon("play")
            )
          ),
        )
        # downloadButton(ns("download_meta"), "Download CSV", class = "btn-sm")
      ),
      card_body(
        conditionalPanel(
          condition = "input.term_type == 'Up'",
          ns = ns,
          conditionalPanel(
            condition = "input.output_type == 'table'",
            ns = ns,
            div(
              class = "h-100",
              # content
              dataTableOutput(
                ns("go_table_up")
              ) %>% with_custom_spinner(
                hide.ui = TRUE,
                caption = "Calculating up GO terms... this may take a moment."
              )
            )
          ),
          conditionalPanel(
            condition = "input.output_type == 'plot'",
            ns = ns,
            div(
              class = "h-100",
              style = "aspect-ratio: 4 / 3; min-width: 500px;",
              # content
              uiOutput(
                ns("go_plot_up_ui")
              ),

              plotOutput(
                ns("go_plot_up"),
                width = "100%",
                height = "100%",
                fill = TRUE
              ) %>% with_custom_spinner()
            )
          )
        ),
        conditionalPanel(
          condition = "input.term_type == 'Down'",
          ns = ns,
          conditionalPanel(
            condition = "input.output_type == 'table'",
            ns = ns,
            div(
              class = "h-100",
              # content
              dataTableOutput(
                ns("go_table_down")
              ) %>% with_custom_spinner(
                hide.ui = TRUE,
                caption = "Calculating down GO terms... this may take a moment."
              )
            )
          ),
          conditionalPanel(
            condition = "input.output_type == 'plot'",
            ns = ns,
            div(
              class = "h-100",
              style = "aspect-ratio: 4 / 3; min-width: 500px;",
              # content
              uiOutput(
                ns("go_plot_down_ui")
              ),

              plotOutput(
                ns("go_plot_down"),
                width = "100%",
                height = "100%",
                fill = TRUE
              ) %>% with_custom_spinner()
            )
          )
        )
      )
    )
  )
}

# -------------------------
# DEG GO server
# -------------------------
mod_deg_go_server <- function(id, DEGs) {
  moduleServer(id, function(input, output, session) {
    # Validate DEGs results passed
    degs_up_valid <- reactive({
      tryCatch(length(DEGs()$up) > 0, error = function(e) FALSE)
    })
    degs_down_valid <- reactive({
      tryCatch(length(DEGs()$down) > 0, error = function(e) FALSE)
    })

    # Validate GO results
    go_up_valid <- reactive({
      tryCatch(length(go_results_up()@result) > 0, error = function(e) FALSE)
    })
    go_down_valid <- reactive({
      tryCatch(length(go_results_down()@result) > 0, error = function(e) FALSE)
    })

    # show code/download disabling logic
    observe({
      req(input$term_type)
      is_disabled <- TRUE

      if (input$term_type == "Up") {
        is_disabled <- !go_up_valid()
      } else if (input$term_type == "Down") {
        is_disabled <- !go_down_valid()
      } else { # catch all disable for smt weird
        is_disabled <- TRUE
      }
      # update buttons
      updateActionButton(
        session,
        "download_wrapper",
        disabled = is_disabled
      )
      updateActionButton(
        session,
        "show_code",
        disabled = is_disabled
      )

    })

    # UP
    go_results_up <- eventReactive(input$run_go_up, {
      validate(
        need(degs_up_valid(), "Please ensure you have a valid DEGs result.")
      )
      DEGs <- DEGs()

      print("===============================")
      print("Starting GO Up...")

      print(paste("EnrichGO up...", Sys.time()))
      DEGs_upgo <- enrichGO(gene = DEGs$up, OrgDb = "org.Mm.eg.db", keyType = "SYMBOL", ont = "BP")
      print(paste("EnrichGO up... done!", Sys.time()))

      print("===============================")

      return(DEGs_upgo)

    })

    # DOWN
    go_results_down <- eventReactive(input$run_go_down, {
      validate(
        need(degs_down_valid(), "Please ensure you have a valid DEGs result.")
      )
      DEGs <- DEGs()

      print("===============================")
      print("Starting GO Down...")

      print(paste("EnrichGO down...", Sys.time()))
      DEGs_downgo <- enrichGO(gene = DEGs$down, OrgDb = "org.Mm.eg.db", keyType = "SYMBOL", ont = "BP")
      print(paste("EnrichGO down... done!", Sys.time()))

      print("===============================")

      return(DEGs_downgo)

    })

    output$go_table_up <- renderDataTable(
      rownames = FALSE,
      options = datatable_options,
      # content:
      { go_results_up()@result }
      )

    output$go_table_down <- renderDataTable(
      rownames = FALSE,
      options = datatable_options,
      # content:
      { go_results_down()@result }
    )

    # Update UI outputs with select inputs
    output$go_plot_up_ui <- renderUI({
      req(
        degs_up_valid(),
        go_results_up()
      )
      results <- go_results_up()@result

      if (nrow(results) == 0) {
        return(NULL)
      } else {
        return(
          fluidRow(
            class = "my-3",
            column(
              width = 6,
              # content:
              textInput(
                session$ns("go_up_title"),
                label = "Plot Title:",
                placeholder = "Enter plot title..."
              ),
              virtualSelectInput(
                inputId = session$ns("go_up_terms"),
                label = "Select Terms:",
                choices = results$Description,
                selected = results$Description[1:5], # Default to first 5 terms
                showSelectedOptionsFirst = TRUE,
                multiple = TRUE,
                search = TRUE,
                disableSelectAll = TRUE,
                inline = FALSE,
                optionsCount = 7,
                position = "bottom"
              )
            ),
            column(
              width = 6,
              # content:
              selectInput(
                session$ns("go_up_x"),
                label = "X-axis:",
                choices = colnames(results),
                selected = "FoldEnrichment"
              ),
              selectInput(
                session$ns("go_up_size"),
                label = "Size:",
                choices = colnames(results),
                selected = "Count"
              ),
              selectInput(
                session$ns("go_up_color"),
                label = "Color:",
                choices = colnames(results),
                selected = "p.adjust"
              )
            )
          )
        )
      }
    })
    output$go_plot_down_ui <- renderUI({
      req(
        degs_down_valid(),
        go_results_down()
      )
      results <- go_results_down()@result

      if (nrow(results) == 0) {
        return(NULL)
      } else {
        return(
          fluidRow(
            class = "my-3",
            column(
              width = 6,
              # content:
              textInput(
                session$ns("go_down_title"),
                label = "Plot Title:",
                placeholder = "Enter plot title..."
              ),
              virtualSelectInput(
                inputId = session$ns("go_down_terms"),
                label = "Select Terms:",
                choices = results$Description,
                selected = results$Description[1:5], # Default to first 5 terms
                showSelectedOptionsFirst = TRUE,
                multiple = TRUE,
                search = TRUE,
                disableSelectAll = TRUE,
                inline = FALSE,
                optionsCount = 7,
                position = "bottom"
              )
            ),
            column(
              width = 6,
              # content:
              selectInput(
                session$ns("go_down_x"),
                label = "X-axis:",
                choices = colnames(results),
                selected = "FoldEnrichment"
              ),
              selectInput(
                session$ns("go_down_size"),
                label = "Size:",
                choices = colnames(results),
                selected = "Count"
              ),
              selectInput(
                session$ns("go_down_color"),
                label = "Color:",
                choices = colnames(results),
                selected = "p.adjust"
              )
            )
          )
        )
      }
    })

    # UP Plot
    output$go_plot_up <- renderPlot({
      req(
        go_results_up(),
        input$go_up_terms,
        input$go_up_x,
        input$go_up_size,
        input$go_up_color
      )
      
      make_go_plot(
        df = go_results_up()@result,
        plot_title = input$go_up_title,
        selected_terms = input$go_up_terms,
        x_col = input$go_up_x,
        size_col = input$go_up_size,
        color_col = input$go_up_color
      )

    })

    # DOWN Plot
    output$go_plot_down <- renderPlot({
      req(
        go_results_down(),
        input$go_down_terms,
        input$go_down_x,
        input$go_down_size,
        input$go_down_color
      )

      make_go_plot(
        df = go_results_down()@result,
        plot_title = input$go_down_title,
        selected_terms = input$go_down_terms,
        x_col = input$go_down_x,
        size_col = input$go_down_size,
        color_col = input$go_down_color
      )

    })

    make_go_plot <- function(
      df,
      plot_title,
      selected_terms,
      x_col,
      size_col,
      color_col
    ) {
      validate(
        need(nrow(df) > 0, "No GO terms to plot."),
        need(selected_terms %in% df$Description, "Selected terms not found in results."),
        need(x_col %in% colnames(df), "Invalid x-axis column."),
        need(size_col %in% colnames(df), "Invalid size column."),
        need(color_col %in% colnames(df), "Invalid color column.")
      )

      df <- df %>%
        filter(Description %in% selected_terms) %>%
        mutate(Description = gsub(" - Mus musculus \\(house mouse\\)$", "", Description))
      
      ggplot(
        df,
        aes(
          x = .data[[x_col]],
          y = reorder(.data[["Description"]], .data[[x_col]]),
          size = .data[[size_col]], 
          color = .data[[color_col]]
        )
      ) +
      geom_point() +
      scale_color_gradient(
        low = "blue",
        high = "red",
        name = color_col
      ) +
      labs(
        x = x_col,
        y = NULL, 
        title = plot_title
      ) +
      theme_minimal() +
      theme(
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black")
      )
    }

    # Downloads
    output$download_go <- downloadHandler(
      filename = function() {
        req(input$term_type, input$output_type)

        if (input$output_type == "plot") { # want plot
          paste0(input$term_type, "_GO_plot", Sys.time(), ".png")
        } else if (input$output_type == "table") { # want table
          paste0(input$term_type, "_GO_terms", Sys.time(), ".csv")
        } else { # catch all error
          paste0(input$term_type, "_GO_", input$output_type, "_error", ".txt")
        }
      },
      content = function(file) {
        req(input$term_type, input$output_type)

        if (input$term_type == "Up") { # Up
          if (input$output_type == "table") { # want table
            write.csv(go_results_up()@result, file, row.names = FALSE)
          } else if (input$output_type == "plot") { # want plot
            # build plot
            ggsave(
              file,
              plot = make_go_plot(
                df = go_results_up()@result,
                plot_title = input$go_up_title,
                selected_terms = input$go_up_terms,
                x_col = input$go_up_x,
                size_col = input$go_up_size,
                color_col = input$go_up_color
              ),
              device = "png",
              width = 8,
              height = 6
            )
          } else { # up catch error
            writeLines("Up GO Download error.", file)
          }

        } else if (input$term_type == "Down") { # Down
          if (input$output_type == "table") { # want table
            write.csv(go_results_down()@result, file, row.names = FALSE)
          } else if (input$output_type == "plot") { # want plot
            # build plot
            ggsave(
              file,
              plot = make_go_plot(
                df = go_results_down()@result,
                plot_title = input$go_down_title,
                selected_terms = input$go_down_terms,
                x_col = input$go_down_x,
                size_col = input$go_down_size,
                color_col = input$go_down_color
              ),
              device = "png",
              width = 8,
              height = 6
            )
          } else { # down catch error
            writeLines("Down GO Download error.", file)
          }
        } else { # catch all error
          writeLines("GO Download error.", file)
        }
      }
    )

    # Modals
    observeEvent(input$show_code, {
      req(
        input
      )
      go_code <- generate_go_code(
        input,
        input$term_type,
        input$output_type == "plot"
      )

      showModal(modalDialog(
        title = "Source Code",
        size = "xl",
        code_block(go_code),
        easyClose = TRUE
      ))
    })

  })
}
