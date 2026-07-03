generate_umap_code <- function(state) {
  format_vec <- function(vec) {
    paste0('c(', paste0('"', vec, '"', collapse = ", "), ')')
  }

  samples <- format_vec(state$orig.ident)
  clusters <- format_vec(state$seurat_clusters)
  subclusters <- format_vec(state$subcluster)
  group_by <- state$group_by

  sprintf(r"(# -------------------------
# UMAP Plot Code
# -------------------------
# Notes: The .RData file is large, ensure your computer has sufficient resources to process it.

library(Seurat)
library(ggplot2)
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
# 3. Plot UMAP
# -------------------------
DimPlot(
  sc_subset,
  reduction = "umap",
  group.by = "%s",
  repel = TRUE,
  pt.size = 1
) +
  ggplot2::labs(title = "", x = "UMAP1", y = "UMAP2") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )
)", samples, clusters, subclusters, group_by)
}