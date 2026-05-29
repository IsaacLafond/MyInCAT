# -------------------------
# DEG KEGG UI
# -------------------------
mod_deg_kegg_ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    radioGroupButtons(
        inputId = ns("term_type"),
        choices = c("Up", "Down"),
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
          "Up Terms"
        ),
        conditionalPanel(
          condition = "input.term_type == 'Down'",
          ns = ns,
          "Down Terms"
        ),

        div(
          class = "d-flex gap-3",
          popover(
            trigger = actionButton(
              ns("settings_trigger"),
              label = NULL,
              icon = icon("gear"),
              class = "btn-light"
            ),
            placement = "auto",
            # options = list(
            #   trigger = "click focus"
            # ),
            # popover content:
            conditionalPanel(
              condition = "input.term_type == 'Up'",
              ns = ns,
              # content:
              numericInput(
                ns("kegg_up_pval_cutoff"),
                label = "Up Terms Adjusted p-value Cutoff:",
                value = 0.05,
                min = 0,
                max = 1,
                step = 0.01
              ),
            ),
            conditionalPanel(
              condition = "input.term_type == 'Down'",
              ns = ns,
              # content:
              numericInput(
                ns("kegg_down_pval_cutoff"),
                label = "Down Terms Adjusted p-value Cutoff:",
                value = 0.05,
                min = 0,
                max = 1,
                step = 0.01
              ),
            )
          ),

          radioGroupButtons(
            inputId = ns("output_type"),
            choiceNames = list(icon("table"), icon("chart-line")),
            choiceValues = c("table", "plot"),
            selected = "table"
          ),

          conditionalPanel(
            condition = "input.term_type == 'Up'",
            ns = ns,
            actionButton(
              ns("run_kegg_up"),
              label = "Find Up KEGG Terms",
              class = "btn-primary",
              icon = icon("play")
            )
          ),
          conditionalPanel(
            condition = "input.term_type == 'Down'",
            ns = ns,
            actionButton(
              ns("run_kegg_down"),
              label = "Find Down KEGG Terms",
              class = "btn-primary",
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
                ns("kegg_table_up")
              ) %>% with_custom_spinner()
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
                ns("kegg_plot_up_ui")
              ),

              plotOutput(
                ns("kegg_plot_up"),
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
                ns("kegg_table_down")
              ) %>% with_custom_spinner()
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
                ns("kegg_plot_down_ui")
              ),

              plotOutput(
                ns("kegg_plot_down"),
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
# DEG KEGG server
# -------------------------
mod_deg_kegg_server <- function(id, DEGs) {
  moduleServer(id, function(input, output, session) {
    # UP
    kegg_results_up <- eventReactive(input$run_kegg_up, {
      validate(
        need(0 < input$kegg_up_pval_cutoff && input$kegg_up_pval_cutoff <= 1, "Please enter a valid p-value cutoff between 0 and 1.")
      )

      DEGs <- DEGs()

      # Show a loading notification since enrichKEGG takes time
      showNotification("Calculating KEGG up terms... this may take a moment.", id = "kegg_up_calc", duration = NULL, type = "message")
      on.exit(removeNotification("kegg_up_calc"), add = TRUE)

      print("===============================")
      print("Starting KEGG Up...")

      print(paste("EnrichKEGG up...", Sys.time()))
      DEGs_upIDs <- bitr(
        DEGs$up,
        fromType = "SYMBOL",
        toType = "ENTREZID",
        OrgDb = "org.Mm.eg.db"
      )

      DEGs_upkegg <- enrichKEGG(
        gene = DEGs_upIDs$ENTREZID,
        organism = "mmu",
        keyType = "kegg",
        pvalueCutoff = input$kegg_up_pval_cutoff
      )
      print(paste("EnrichKEGG up... done!", Sys.time()))

      print("===============================")

      return(DEGs_upkegg)

    })
    # DOWN
    kegg_results_down <- eventReactive(input$run_kegg_down, {
      validate(
        need(0 < input$kegg_down_pval_cutoff && input$kegg_down_pval_cutoff <= 1, "Please enter a valid p-value cutoff between 0 and 1.")
      )

      DEGs <- DEGs()

      # Show a loading notification since enrichKEGG takes time
      showNotification("Calculating KEGG down terms... this may take a moment.", id = "kegg_down_calc", duration = NULL, type = "message")
      on.exit(removeNotification("kegg_down_calc"), add = TRUE)

      print("===============================")
      print("Starting KEGG Down...")

      print(paste("EnrichKEGG down...", Sys.time()))
      DEGs_downIDs <- bitr(
        DEGs$down,
        fromType = "SYMBOL",
        toType = "ENTREZID",
        OrgDb = "org.Mm.eg.db"
      )

      DEGs_downkegg <- enrichKEGG(
        gene = DEGs_downIDs$ENTREZID,
        organism = "mmu",
        keyType = "kegg",
        pvalueCutoff = input$kegg_down_pval_cutoff
      )
      print(paste("EnrichKEGG down... done!", Sys.time()))

      print("===============================")

      return(DEGs_downkegg)

    })

    output$kegg_table_up <- renderDataTable(
      rownames = FALSE,
      options = datatable_options,
      # content:
      { kegg_results_up()@result }
      )

    output$kegg_table_down <- renderDataTable(
      rownames = FALSE,
      options = datatable_options,
      # content:
      { kegg_results_down()@result }
    )

    # Update UI outputs with select inputs
    output$kegg_plot_up_ui <- renderUI({
      req(kegg_results_up())
      results <- kegg_results_up()@result

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
                session$ns("kegg_up_title"),
                label = "Plot Title:",
                placeholder = "Enter plot title..."
              ),
              virtualSelectInput(
                inputId = session$ns("kegg_up_terms"),
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
                session$ns("kegg_up_x"),
                label = "X-axis:",
                choices = colnames(results),
                selected = "FoldEnrichment"
              ),
              selectInput(
                session$ns("kegg_up_size"),
                label = "Size:",
                choices = colnames(results),
                selected = "Count"
              ),
              selectInput(
                session$ns("kegg_up_color"),
                label = "Color:",
                choices = colnames(results),
                selected = "p.adjust"
              )
            )
          )
        )
      }
    })
    output$kegg_plot_down_ui <- renderUI({
      req(kegg_results_down())
      results <- kegg_results_down()@result

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
                session$ns("kegg_down_title"),
                label = "Plot Title:",
                placeholder = "Enter plot title..."
              ),
              virtualSelectInput(
                inputId = session$ns("kegg_down_terms"),
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
                session$ns("kegg_down_x"),
                label = "X-axis:",
                choices = colnames(results),
                selected = "FoldEnrichment"
              ),
              selectInput(
                session$ns("kegg_down_size"),
                label = "Size:",
                choices = colnames(results),
                selected = "Count"
              ),
              selectInput(
                session$ns("kegg_down_color"),
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
    output$kegg_plot_up <- renderPlot({
      req(
        kegg_results_up(),
        input$kegg_up_terms,
        input$kegg_up_x,
        input$kegg_up_size,
        input$kegg_up_color
      )
      
      make_kegg_plot(
        df = kegg_results_up()@result,
        plot_title = input$kegg_up_title,
        selected_terms = input$kegg_up_terms,
        x_col = input$kegg_up_x,
        size_col = input$kegg_up_size,
        color_col = input$kegg_up_color
      )

    })

    # DOWN Plot
    output$kegg_plot_down <- renderPlot({
      req(
        kegg_results_down(),
        input$kegg_down_terms,
        input$kegg_down_x,
        input$kegg_down_size,
        input$kegg_down_color
      )

      make_kegg_plot(
        df = kegg_results_down()@result,
        plot_title = input$kegg_down_title,
        selected_terms = input$kegg_down_terms,
        x_col = input$kegg_down_x,
        size_col = input$kegg_down_size,
        color_col = input$kegg_down_color
      )

    })

    make_kegg_plot <- function(
      df,
      plot_title,
      selected_terms,
      x_col,
      size_col,
      color_col
    ) {
      validate(
        need(nrow(df) > 0, "No KEGG terms to plot."),
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

    
  })
}