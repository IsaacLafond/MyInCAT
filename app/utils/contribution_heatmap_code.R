generate_contribution_heatmap_code <- function(sidebar_selections, pattern) {
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

  sprintf(r"(# -------------------------
# Contribution Heatmap Plot Code
# -------------------------
# Notes: The .RData file is large, ensure your computer has sufficient resources to process it.

library(CellChat)

# -------------------------
# 1. Read the CellChat object
# -------------------------
load("%s") # Ensure this is the correct path to the .RData file

# -------------------------
# 2. Create Contribution Heatmap
# -------------------------
netAnalysis_signalingRole_heatmap(
  %s,
  pattern = "%s",
  slot.name = "netP",
  color.heatmap = "Reds",
  width = 20,
  height = 20,
)
)",
    filename,
    obj_name,
    pattern
  )
}