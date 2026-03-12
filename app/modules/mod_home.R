# -------------------------
# Home UI
# -------------------------
mod_home_ui <- function(id) {
  ns <- NS(id)

  page_fixed(
    
    layout_column_wrap(
      width = 1,
      
      # --- Header & Intro Section ---
      card(
        card_header(h2("Welcome to MyInCAT", class = "mb-0")),
        p("MyInCAT (Myogenic single-cell Integration of Cachexia Transcriptomics data) is a comprehensive resource for exploring single-cell RNA-sequencing data in cancer cachexia. 
           We have integrated single-cell and single-nucleus data containing ", 
           strong("130,996 cells"), " from 6 studies and 5 tumour models."),
        p("Use this platform to perform differential gene expression, pathway analysis, and cell communication 
           modeling on skeletal muscle from mouse models to generate hypotheses and publication-ready plots.")
      ),
      
      # --- Tutorial Video Section ---
      card(
        card_header(h4("How to use MyInCAT")),
        div(
          class = "text-center",
          # Placeholder for the video
          img(
            src = "video-coming-soon-placeholder.gif", 
            alt = "Tutorial Video Coming Soon",
            style = "max-width: 600px; width: 100%; border-radius: 8px; border: 1px solid #ddd;"
          ),
          p(class = "text-muted mt-2", "A full video tutorial is currently in development.")
        )
      ),
      
      # --- Data Summary Table ---
      card(
        card_header(h4("Included Studies & Models")),
        tableOutput(ns("studies_table")),
        full_screen = TRUE
      ),
      
      # --- Citations & Contact Footer ---
      layout_column_wrap(
        width = 1/2,
        card(
          card_header(h5("Citations")),
          p("If you use plots, tables, or code generated from this resource, please cite:"),
          tags$blockquote("Your Citation Information Here", class = "blockquote", style = "font-size: 0.9rem;")
        ),
        card(
          card_header(h5("Support & Inquiries")),
          p("For questions regarding the data or the application, please contact:"),
          tags$a(href = "mailto:example@email.com", "example@email.com")
        )
      )
    ),

    p("Developed by Isaac Lafond", class = "fst-italic text-muted text-center small")
  )
}


# -------------------------
# Home Server
# -------------------------
mod_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- data.frame(
      Study = c("Agca et al. 2024", "Brown et al. 2026", "Kim et al. 2025", "Pryce et al. 2024 (C-26)", "Pryce et al. 2024 (KPP)", "Zhang et al. 2024"),
      Sequencing = c("Single nucleus", "Single cell", "Single cell", "Single cell", "Single cell", "Single nucleus"),
      Tumour_Model = c("LLC (subcut)", "LLC (subcut)", "B16F10 (subcut)", "C-26 (subcut)", "KPP (spont)", "KIC (spont)"),
      Cancer_Type = c("Lung", "Lung", "Melanoma", "Colon", "Pancreatic", "Pancreatic"),
      Sex = c("Male", "Male", "M/F", "Male", "Male", "Female"),
      Cells = c("11,220", "29,244", "18,804", "23,959", "39,832", "7,937")
    )

    output$studies_table <- renderTable({
      data
    })

  })
}
