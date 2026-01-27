groupby_choices <- list(
  "None" = NULL,
  "Cluster" = "seurat_clusters",
  "Subcluster" = "subcluster",
  "Sample" = "orig.ident",
  "Experiment" = "experiment"
)

choices_data <- list(
  "Cluster" = c(
    "cluster1",
    "cluster2",
    "cluster3",
    "cluster4",
    "cluster5"
    ),
  "Subcluster" = c(
    "subclusterA",
    "subclusterB",
    "subclusterC"
    ),
  "Sample" = c( # aka orig.ident
    "sample1",
    "sample2", 
    "sample3"
    ),
  "Experiment" = c(
    "experimentX",
    "experimentY"
    )
)