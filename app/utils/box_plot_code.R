generate_box_plot_code <- function(state, feature) {
  format_vec <- function(vec) {
    paste0('c(', paste0('"', vec, '"', collapse = ", "), ')')
  }

  samples     <- format_vec(state$orig.ident)
  clusters    <- format_vec(state$seurat_clusters)
  subclusters <- format_vec(state$subcluster)
  group_by    <- state$group_by

  sprintf(r"(# -------------------------
# Box Plot Code
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
# 3. Box plot of selected feature
# -------------------------
ggplot(FetchData(
  sc_subset,
  vars = c("%s", "%s")
), aes(
  x = !!sym("%s"),
  y = !!sym("%s"),
  fill = !!sym("%s")
)) +
geom_boxplot(outlier.shape = NA, width = 0.6) +
theme_classic() +
theme(
  legend.position = "none",
  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
  plot.title = element_text(hjust = 0.5, face = "italic")
) +
labs(
  x = NULL,
  y = "Expression",
  title = "%s"
)
)", samples, clusters, subclusters,
    feature, group_by,
    group_by,
    feature,
    group_by,
    feature)
}
