generate_circle_code <- function(sidebar_selections, label.edge, source_cells, target_cells) {
  format_vec <- function(vec) {
    paste0('c(', paste0('"', vec, '"', collapse = ", "), ')')
  }

  selections <- sidebar_selections()

  filename <- "cellchat_split.RData"
  if (selections$type == "Combined") {
    comp <- c(
      "ctl" = "cellchat_integrated.RData",
      "tb" = "tumours_integrated.RData"
    )
    filename <- comp[[selections$combined_sample]]
  }

  obj_name <- paste("cellchat", selections$combined_sample, selections$interaction_type, sep = "_")
  if (selections$type == "Split") {
    obj_name <- paste("cellchat", selections$split_sample, selections$interaction_type, sep = "_")
  }

  src_cells_vec <- format_vec(source_cells)
  tgt_cells_vec <- format_vec(target_cells)

  sprintf(r"(# -------------------------
# Circle Plot Code
# -------------------------
# Notes: The .RData file is large, ensure your computer has sufficient resources to process it.

library(CellChat)

# -------------------------
# 1. Read the CellChat object
# -------------------------
load("%s") # Ensure this is the correct path to the .RData file

# -------------------------
# 2. Create Circle Plot
# -------------------------
netVisual_circle(
  %s@net$count,
  weight.scale = TRUE,
  label.edge = %s,
  title.name = "Number of interactions",
  arrow.size = 0.05,
  sources.use = %s,
  targets.use = %s
)
)",
    filename,
    obj_name,
    label.edge,
    src_cells_vec, tgt_cells_vec
  )
}