# -------------------------
# Home UI
# -------------------------
mod_home_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h2("Welcome to MyInCAT!"),
    p("This is the home page tab."),
    p("General information, instructions, or summaries/video here.")
  )
}

# -------------------------
# Home server
# -------------------------
mod_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server logic needed yet
  })
}
