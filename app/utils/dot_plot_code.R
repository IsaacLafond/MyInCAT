generate_dot_plot_code <- function(state, features) {
  format_vec <- function(vec) {
    paste0('c(', paste0('"', vec, '"', collapse = ", "), ')')
  }

  samples     <- format_vec(state$orig.ident)
  clusters    <- format_vec(state$seurat_clusters)
  subclusters <- format_vec(state$subcluster)
  group_by    <- state$group_by
  feature_vec <- format_vec(features)

  sprintf(r"(# -------------------------
# Dot Plot Code
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
# 3. Dot plot of selected features
# -------------------------
DotPlot(
  sc_subset,
  features = %s,
  group.by = "%s"
) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "italic")
  ) +
  scale_color_gradientn(colors = c("red", "white", "blue"))
)", samples, clusters, subclusters, feature_vec, group_by)
}