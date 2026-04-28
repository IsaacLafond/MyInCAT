# -------------------------
# DEG KEGG UI
# -------------------------
mod_deg_kegg_ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    coming_soon() #TODO
  )
}

# -------------------------
# DEG KEGG server
# -------------------------
mod_deg_kegg_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}