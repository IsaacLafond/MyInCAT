# -------------------------
# DEG GO UI
# -------------------------
mod_deg_go_ui <- function(id) {
  ns <- NS(id)

  page_fluid(

    h3("Up Terms"),

    actionButton(
      ns("run_go_up"),
      label = "Find GO Up Terms",
      class = "btn-primary",
      icon = icon("play")
    ),

    dataTableOutput(
      ns("go_table_up"),
      width = "100%",
      height = "450px"
    ) %>% with_custom_spinner(),

    uiOutput(
      ns("go_plot_up_ui")
    ),

    plotOutput(
      ns("go_plot_up"),
      width = "100%",
      height = "66vh"
    ) %>% with_custom_spinner(),

    # =========================

    h3("Down Terms"),

    actionButton(
      ns("run_go_down"),
      label = "Find GO Down Terms",
      class = "btn-primary",
      icon = icon("play")
    ),

    dataTableOutput(
      ns("go_table_down"),
      width = "100%",
      height = "450px"
    ) %>% with_custom_spinner(),

    uiOutput(
      ns("go_plot_down_ui")
    ),

    plotOutput(
      ns("go_plot_down"),
      width = "100%",
      height = "66vh"
    ) %>% with_custom_spinner()

  )
}

# -------------------------
# DEG GO server
# -------------------------
mod_deg_go_server <- function(id, DEGs) {
  moduleServer(id, function(input, output, session) {
    # UP
    go_results_up <- eventReactive(input$run_go_up, {
      DEGs <- DEGs()

      # Show a loading notification since enrichGO takes time
      showNotification("Calculating GO up terms... this may take a moment.", id = "go_up_calc", duration = NULL, type = "message")
      on.exit(removeNotification("go_up_calc"), add = TRUE)

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
      DEGs <- DEGs()

      # Show a loading notification since enrichGO takes time
      showNotification("Calculating GO down terms... this may take a moment.", id = "go_down_calc", duration = NULL, type = "message")
      on.exit(removeNotification("go_down_calc"), add = TRUE)

      print("===============================")
      print("Starting GO Down...")

      print(paste("EnrichGO down...", Sys.time()))
      DEGs_downgo <- enrichGO(gene = DEGs$down, OrgDb = "org.Mm.eg.db", keyType = "SYMBOL", ont = "BP")
      print(paste("EnrichGO down... done!", Sys.time()))

      print("===============================")

      return(DEGs_downgo)

    })

    output$go_table_up <- renderDataTable(
      width = "100%",
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        scrollY = "300px",
        scrollCollapse = TRUE
      ),
      rownames = FALSE,
      # content:
      { go_results_up()@result }
      )

    output$go_table_down <- renderDataTable(
      width = "100%",
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        scrollY = "300px",
        scrollCollapse = TRUE
      ),
      rownames = FALSE,
      # content:
      { go_results_down()@result }
    )

    # Update UI outputs with select inputs
    output$go_plot_up_ui <- renderUI({
      req(go_results_up())
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
      req(go_results_down())
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

    output$go_plot_up <- renderPlot({
      # UP
      # up_terms <- c(#List of row numbers from DEGs_upgo for the terms that you want to plot. Is there a way to make a checklist of the enriched terms that come up so that they can pick and choose which terms to plot?)
      # go_up <- DEGs_upgo[up_terms, ]
      go_up <- go_results_up()@result %>% filter(Description %in% input$go_up_terms)

      go_up$Description <- gsub(" - Mus musculus \\(house mouse\\)$", "", go_up$Description)

      ggplot(
        go_up,
        aes(
          x = .data[[input$go_up_x]], 
          y = reorder(.data[["Description"]], .data[[input$go_up_x]]),
          size = .data[[input$go_up_size]], 
          color = .data[[input$go_up_color]]
        )
      ) +
      geom_point() +
      scale_color_gradient(
        low = "blue",
        high = "red",
        name = input$go_up_color
      ) +
      labs(
        x = input$go_up_x,
        y = NULL, 
        title = input$go_up_title
      ) +
      theme_minimal() +
      theme(
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black")
      )

    })

    output$go_plot_down <- renderPlot({
      # DOWN
      # down_terms <- c(#List of row numbers from DEGs_downgo for the terms that you want to plot. Is there a way to make a checklist of the enriched terms that come up so that they can pick and choose which terms to plot?)
      # go_down <- DEGs_downgo[down_terms, ]
      go_down <- go_results_down()@result %>% filter(Description %in% input$go_down_terms)

      go_down$Description <- gsub(" - Mus musculus \\(house mouse\\)$", "", go_down$Description)

      ggplot(
        go_down,
        aes(
          x = .data[[input$go_down_x]],
          y = reorder(.data[["Description"]], .data[[input$go_down_x]]),
          size = .data[[input$go_down_size]], 
          color = .data[[input$go_down_color]]
        )
      ) +
      geom_point() +
      scale_color_gradient(
        low = "blue",
        high = "red",
        name = input$go_down_color
      ) +
      labs(
        x = input$go_down_x,
        y = NULL, 
        title = input$go_down_title
      ) +
      theme_minimal() +
      theme(
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black")
      )
    })

  })
}

# ### GO terms

# ```{r, include=FALSE}

# ### View GO terms on website
# view(DEGs_upgo)
# view(DEGs_downgo)

# ### Download GO terms spreadsheet
# write.csv(DEGs_upgo, file = "DEGs_upgo.csv", row.names = FALSE)
# write.csv(DEGs_downgo, file = "DEGs_downgo.csv", row.names = FALSE)
# ```

### NOTE - For specifying FoldEnrichment, Count, p.adjust, I want these as the default setting. Is there a way to pick and choose which values you want to customize? Where I specified these values, I would like the option of specifying GeneRatio, BgRatio, RichFactor, FoldEnrichment, zScore, pvalue, p.adjust, qvalue, Count. Then the labels would need to adjust accordingly.
