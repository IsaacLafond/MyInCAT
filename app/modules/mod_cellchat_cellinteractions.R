# -------------------------
# CellChat Cell Interactions UI
# -------------------------
mod_cellchat_cellinteractions_ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    coming_soon() # TODO
  )
}

# -------------------------
# CellChat Cell Interactions server
# -------------------------
mod_cellchat_cellinteractions_server <- function(id, cellchat_object) {
  moduleServer(id, function(input, output, session) {
    
  })
}

# THIS PART???

# ## Identify enriched pathways from one cell type (or group of cells) to another

# ```{r}
# ### Extract communication probabilities at the pathway level
# netP_subset <- subsetCommunication(#cellchat_orig.ident_interaction.type, slot.name = "netP")

# ### Subset for enriched pathways for target cells to source cells
# netP_subset <- netP_subset[
#   netP_subset$source %in% c("#source_cell_1", "#source_cell_2", "#etc.") & 
#   netP_subset$target %in% c("#target_cell_1", "#target_cell_2", "#etc."), 
# ]

# ### Get unique pathways from specified cells to target cells
# pathways_subset <- unique(netP_subset$pathway_name)


# head(pathways_subset)
# ```
# ### NOTE - have option to download as a csv file or text file!