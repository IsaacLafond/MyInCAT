generate_deg_code <- function(state, input) {
  format_vec <- function(vec) {
    paste0('c(', paste0('"', vec, '"', collapse = ", "), ')')
  }

  samples     <- format_vec(state$orig.ident)
  clusters    <- format_vec(state$seurat_clusters)
  subclusters <- format_vec(state$subcluster)
  group_by    <- state$group_by

  ident_1 <- format_vec(unique(input$ident_1))
  ident_2 <- format_vec(unique(input$ident_2))

  pval_cutoff        <- input$pval_cutoff
  lower_logfc_cutoff <- input$lower_logfc_cutoff
  upper_logfc_cutoff <- input$upper_logfc_cutoff

  sprintf(r"(# -------------------------
# DEG Code
# -------------------------
# Notes: The .RData file is large, ensure your computer has sufficient resources to process it.

library(Seurat)
library(dplyr)

# -------------------------
# 1. Read the Seurat object
# -------------------------
load("muscle_integrated.RData") # Ensure this is the correct path to the .RData file

# -------------------------
# 2. Subset to the currently selected samples, clusters, and subclusters
# -------------------------
sc_subset <- subset(
  sc_combined,
  subset = orig.ident %%in%% %s &
           seurat_clusters %%in%% %s &
           subcluster %%in%% %s
)

# -------------------------
# 3. Find differentially expressed genes
# -------------------------
raw_degs <- FindMarkers(
  sc_subset,
  group.by = "%s",
  ident.1 = %s,
  ident.2 = %s,
  logfc.threshold = 0,
  min.pct = 0.10,
  verbose = TRUE
)

# -------------------------
# 4. Filter by adjusted p-value and log2FC cutoffs
# -------------------------
filtered_degs <- subset(
  na.omit(raw_degs),
  p_val_adj < %s &
  avg_log2FC > %s &
  avg_log2FC < %s
)

filtered_degs
)", samples,
    clusters,
    subclusters,
    group_by,
    ident_1,
    ident_2,
    pval_cutoff,
    lower_logfc_cutoff, 
    upper_logfc_cutoff
  )
}