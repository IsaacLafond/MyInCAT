# -------------------------
# Pseudotime Plot UI
# -------------------------
mod_pseudotime_plot_ui <- function(id) {
  ns <- NS(id)

  page_fluid(

    # --- Settings card ---
    card(
      card_header("Pseudotime Settings"),

      # Row 1: root subcluster + trajectory colour
      layout_columns(
        col_widths = c(6, 6),
        selectInput(
          ns("root_subcluster"),
          label   = "Root Subcluster:",
          choices = NULL    # populated server-side from global_state
        ),
        colourInput(
          ns("traj_color"),
          label = "Trajectory Color:",
          value = "#00FF00"
        )
      ),

      # Row 2: label toggles
      layout_columns(
        col_widths = c(4, 4, 4),
        checkboxInput(
          ns("label_branch_points"),
          "Label Branch Points",
          value = FALSE
        ),
        checkboxInput(
          ns("label_roots"),
          "Label Roots",
          value = FALSE
        ),
        checkboxInput(
          ns("label_leaves"),
          "Label Leaves",
          value = FALSE
        )
      ),

      # Run button
      actionButton(
        ns("run_pseudotime"),
        label = "Run Pseudotime",
        class = "btn-primary w-100"
      )
    ),

    # --- Cell composition summary ---
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Cell Composition Summary",
        # downloadButton(ns("download_meta"), "Download CSV", class = "btn-sm")
      ),
      dataTableOutput(ns("meta_table"))
    ),

    # --- Pseudotime UMAP ---
    card(
      card_header("Pseudotime UMAP"),
      plotOutput(
        ns("pseudo_umap"),
        width  = "100%",
        height = "66vh"
      ) %>% with_custom_spinner()
    ),

    # --- Pseudotime boxplot by subcluster ---
    card(
      card_header("Pseudotime by Subcluster"),
      plotOutput(
        ns("pseudo_box"),
        width  = "100%",
        height = "66vh"
      ) %>% with_custom_spinner()
    )

  )
}


# -------------------------
# Pseudotime Plot server
# -------------------------
mod_pseudotime_plot_server <- function(id, global_state) {
  moduleServer(id, function(input, output, session) {

    # Populate root subcluster choices whenever the global subset changes.
    # Runs immediately so the selector is ready before the user clicks Run.
    observe({
      state <- global_state()
      updateSelectInput(session, "root_subcluster", choices = state$subcluster)
      # updateSelectInput(session, "root_subcluster", choices = state$seurat_clusters)
    })

    # Meta summary table — reactive to the global subset, independent of
    # pseudotime computation so users can inspect cell counts before running.
    pseudo_meta <- reactive({
      state <- global_state()
      state$sc_subset@meta.data %>%
        group_by(experiment, orig.ident, seurat_clusters, subcluster) %>%
        summarise(n_cells = n(), .groups = "drop") %>%
        arrange(experiment, orig.ident, seurat_clusters)
    })

    output$meta_table <- renderDataTable(
      width = "100%",
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        scrollY = "300px",
        scrollCollapse = TRUE
      ),
      rownames = TRUE,
      # content:
      { pseudo_meta() }
    )

    # output$download_meta <- downloadHandler(
    #   filename = function() paste0("meta_table_", Sys.Date(), ".csv"),
    #   content  = function(file) write.csv(pseudo_meta(), file, row.names = FALSE)
    # )

    # Core pseudotime reactive — only fires when "Run Pseudotime" is clicked.
    # All expensive monocle3 steps live here so rerenders of the two plot
    # outputs are cheap (they just read the cached CDS).
    pseudo_cds <- eventReactive(input$run_pseudotime, {
      req(input$root_subcluster, global_state())

      state    <- global_state()
      root_sub <- input$root_subcluster

      validate(
        need(!is.null(root_sub) && root_sub != "", "Please select a root subcluster."),
        need(ncol(state$sc_subset) > 0, "The current subset contains no cells.")
      )

      withProgress(message = "Running pseudotime analysis\u2026", value = 0, {

        sc_sub     <- state$sc_subset
        cell_names <- colnames(sc_sub)
        n_cells    <- length(cell_names)

        # 1. Build a 1-gene dummy sparse matrix (genes \u00d7 cells).
        #
        #    as.cell_data_set() is intentionally avoided: it calls
        #    as(counts, "dgCMatrix") internally, which would force the full
        #    BPCells file-backed matrix into RAM.
        #
        #    monocle3 only needs the expression matrix for preprocess_cds /
        #    reduce_dimension — steps we skip entirely because we inject
        #    Seurat's pre-computed UMAP directly. A 1-row all-zero sparse
        #    matrix satisfies new_cell_data_set() without touching the
        #    BPCells layers at all.
        #
        #    Confirmed safe by reading the source of all four downstream
        #    functions (learn_graph, order_cells, pseudotime, plot_cells):
        #    none of them access the expression matrix when
        #    color_cells_by = "pseudotime" and genes = NULL.
        incProgress(0.10, detail = "Building CDS\u2026")
        dummy_mat <- Matrix::sparseMatrix(
          i        = integer(0),
          j        = integer(0),
          x        = numeric(0),
          dims     = c(1L, n_cells),
          dimnames = list("dummy_gene", cell_names)
        )

        gene_meta <- data.frame(
          gene_short_name = "dummy_gene",
          row.names       = "dummy_gene"
        )

        # Cell metadata comes straight from Seurat — the object is never
        # mutated.
        cell_meta <- sc_sub@meta.data

        cds <- monocle3::new_cell_data_set(
          # expression_data = LayerData(sc_sub, assay = "RNA", layer = "counts"),
          expression_data = dummy_mat,
          cell_metadata   = cell_meta,
          gene_metadata   = gene_meta
        )        

        # 2. Assign a single partition (treat all cells as one trajectory).
        incProgress(0.10, detail = "Assigning partitions\u2026")
        partition_vec <- setNames(
          factor(rep(1L, n_cells), levels = "1"),
          cell_names
        )
        cds@clusters$UMAP$partitions <- partition_vec

        # 3. Derive subcluster labels directly from Seurat metadata.
        #
        #    Idents(sc_sub) <- "subcluster" is intentionally avoided: setting
        #    the active identity even on a local copy can mutate shared state
        #    and break determinism for other modules that read sc_combined.
        incProgress(0.10, detail = "Transferring cluster labels\u2026")
        cluster_labels <- setNames(
          factor(sc_sub@meta.data$subcluster),
          rownames(sc_sub@meta.data)
        )
        cds@clusters$UMAP$clusters <- cluster_labels

        # 4. Transfer UMAP cell embeddings from the Seurat reduction.
        incProgress(0.10, detail = "Transferring UMAP coordinates\u2026")
        cds@int_colData@listData$reducedDims$UMAP <- sc_sub@reductions$umap@cell.embeddings
        
        # 5. Learn the trajectory graph.
        incProgress(0.20, detail = "Learning trajectory graph\u2026")
        print("===============================")
        print(paste("Starting learn_graph", Sys.time()))
        cds <- monocle3::learn_graph(cds, use_partition = FALSE, verbose = TRUE)
        print(paste("Finished learn_graph", Sys.time()))
        print("===============================")

        # 6. Order cells from all cells belonging to the chosen root subcluster.
        incProgress(0.20, detail = "Ordering cells in pseudotime\u2026")
        root_cells <- names(cluster_labels[cluster_labels == root_sub])
        print(paste("Root cells:", root_cells))

        print("===============================")
        print(paste("Starting order_cells", Sys.time()))
        cds <- monocle3::order_cells(
          cds,
          reduction_method = "UMAP",
          root_cells       = root_cells,
          verbose = TRUE
        )
        print(paste("Finished order_cells", Sys.time()))
        print("===============================")

        # 7. Store pseudotime values in CDS colData for downstream plots.
        incProgress(0.10, detail = "Finalising\u2026")
        cds$monocle3_pseudotime <- monocle3::pseudotime(cds)

        cds
      })
    })

    # Pseudotime UMAP plot
    output$pseudo_umap <- renderPlot({
      cds <- pseudo_cds()

      monocle3::plot_cells(
        cds,
        color_cells_by         = "pseudotime",
        label_branch_points    = input$label_branch_points,
        label_roots            = input$label_roots,
        label_leaves           = input$label_leaves,
        group_label_size       = 3,
        cell_size              = 1.5,
        trajectory_graph_color = input$traj_color
      ) +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks  = element_blank()
        )
    })

    # Pseudotime boxplot by subcluster
    output$pseudo_box <- renderPlot({
      cds <- pseudo_cds()

      pseudo_data <- as.data.frame(cds@colData)

      validate(
        need("subcluster"          %in% colnames(pseudo_data), "Subcluster column not found in pseudotime data."),
        need("monocle3_pseudotime" %in% colnames(pseudo_data), "Pseudotime values not found in CDS colData.")
      )

      ggplot(
        pseudo_data,
        aes(x = subcluster, y = monocle3_pseudotime, fill = subcluster)
      ) +
        geom_boxplot(show.legend = FALSE, outlier.shape = NA) +
        labs(x = NULL, y = "Pseudotime") +
        theme_classic() +
        theme(
          legend.position = "none",
          axis.text.x     = element_text(angle = 45, hjust = 1, vjust = 1)
        )
    })

  })
}
