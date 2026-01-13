umap_code <- function() {
  tags$pre(
    tags$code(r"(# UMAPs

## UMAP by cluster, subcluster, sample, or experiment

```{r}
DimPlot(sc_combined, reduction = "umap", group.by = "#seurat_cluster OR subcluster OR orig.ident OR experiment", repel = TRUE, pt.size = 1) +
  ggplot2::labs(title = "", x = "UMAP1", y = "UMAP2") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
```

### UMAP by cluster/subcluster/sample/experiment, only including certain ones. NOTE - this will also allow you to colour by cluster OR sample OR experiment, so you can get different combinations (i.e. all/certain experiments highlighted by sample, all/certain clusters highlighted by sample, etc.)

```{r}
DimPlot(
  subset(sc_combined, subset = #seurat_clusters OR subcluster OR orig.ident OR experiment %in% c("#name_1", "#name_2", "#etc.")),
  reduction = "umap",
  group.by = "#seurat_clusters OR subcluster OR orig.ident OR experiment", # THIS IS WHAT CELLS WILL BE COLOURED BY
  repel = TRUE,
  pt.size = 1,
  cols = c(
    "#name_1" = "#colour_1",
    "#name_2"       = "#colour_2",
    "#etc." # NAMES SHOULD MATCH THE group.by FUNCTION. REMOVE cols FUNCTION IF YOU DO NOT WANT TO HIGHLIGHT CERTAIN FEATURES
  )
) +
  ggplot2::labs(title = "", x = "UMAP1", y = "UMAP2") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )
```

### NOTE - When each plot is generated, include a meta table to include the experiments, samples, clusters, and subclusters included in the analysis, and how many cells are from each. Off to the side, not a part of the plot itself.

```{r}
table(sc_combined$experiment) # per experiment
table(sc_combined$orig.ident) # per sample
table(sc_combined$seurat_clusters) # per cluster
table(sc_combined$subcluster) # per subcluster
```)")
  )
}