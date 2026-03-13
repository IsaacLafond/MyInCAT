# -------------------------
# Home UI
# -------------------------
home_ui <- function() {

  page_fixed(
    h2("Welcome to MyInCAT!"),

    br(),

    p(
      "MyInCAT (Myogenic single-cell Integration of Cachexia Transcriptomics data), is a resource for exploring single-cell RNA-sequencing data in cancer cachexia! We have integrated single-cell and single-nucleus RNA sequencing data containing 130,996 cells from 6 studies, 5 tumour models to perform differential gene expression, pathway, and cell communication analyses on skeletal muscle from mouse models of cancer cachexia. Use this resource to generate hypotheses, validate findings, and generate publication-ready plots.",
      style = ""
    ),

    p("For a tutorial on how to use CachexiaSC, please watch this video!"),

    # tags$iframe(
    #   src = "https://www.youtube-nocookie.com/embed/dQw4w9WgXcQ",
    #   width = "66%",
    #   frameborder = "0",
    #   allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
    #   allowfullscreen = NA,
    #   style = "aspect-ratio: 16/9;",
    #   class = "mx-auto d-block"
    # ),
    img(
      src = "www/video-coming-soon-placeholder.gif",
      alt = "Video coming soon placeholder",
      width = "66%",
      style = "aspect-ratio: 16/9;",
      class = "mx-auto d-block"
    ),

    hr(),

    h3("Credits"),
  
    h3("Citations"),

    h3("Inquiries: exmaple@email.com"),

    h3("Studies/Models table"),

    p("Developed by Isaac Lafond", class = "fst-italic text-center small")
    # p("Version 0.1.0, Developed by Isaac Lafond", class = "fst-italic text-center small")
  )
}
