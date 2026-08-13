generate_chord_code <- function(sidebar_selections, pathway, source_cells, target_cells) {
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
# Chord Plot Code
# -------------------------
# Notes: The .RData file is large, ensure your computer has sufficient resources to process it.

library(CellChat)

# -------------------------
# 1. Read the CellChat object
# -------------------------
load("%s") # Ensure this is the correct path to the .RData file

# -------------------------
# 2. Create Chord Plot
# -------------------------
netVisual_chord_gene(
  %s,
  signaling = %s,
  sources.use = %s,
  targets.use = %s,
  show.legend = TRUE,
  lab.cex = 0.5
)
)",
    filename,
    obj_name,
    pathway,
    src_cells_vec, tgt_cells_vec
  )
}