# -------------------------
# Home UI
# -------------------------
home_ui <- function() {

  page_fixed(
    h2("Welcome to MyInCAT!"),

    br(),

    p(
      "MyInCAT (Myogenic single-cell Integration of Cachexia Transcriptomics data), is a resource designed for exploring single-cell RNA-sequencing data in cancer cachexia! We have integrated single-cell and single-nucleus RNA sequencing data from skeletal muscle, containing 130,996 cells from 6 studies and 5 tumour models to perform differential gene expression, pathway, and cell communication analyses. Use this resource to generate hypotheses, validate findings, and generate publication-ready plots.",
      style = ""
    ),

    p("For a tutorial on how to use MyInCAT, please watch this video!"),

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
      style = "aspect-ratio: 16/9; margin-bottom: 1rem;",
      class = "mx-auto d-block"
    ),

    p(
      "To download the integrated mouse skeletal muscle and tumour Seurat objects, and the integrated and split CellChat objects, please visit: (will be published on Zenodo once the manuscript is published)"
    ),
    p(
      "Codes can be accessed on GitHub (",
      a(
        "https://github.com/IsaacLafond/MyInCAT",
        href = "https://github.com/IsaacLafond/MyInCAT",
        target = "_blank"
      ),
      ")."
    ),

    hr(),

    # -------------------------
    # Datasets Section
    # -------------------------
    h3("Datasets"),
    p("Below are tables showing demographic details for the datasets."),
    
    h4("Table 1: Mouse skeletal muscle samples single-cell and single nucleus RNA-sequencing dataset demographics", class = "mt-4"),
    tags$div(
      class = "table-responsive",
      tags$table(
        class = "table table-striped table-bordered table-hover align-middle",
        tags$thead(
          tags$tr(tags$th("Study"),
            tags$th(colspan = 2, a("Agca et al. 2024", href = "https://doi.org/10.1002/jcsm.13540", target = "_blank")),
            tags$th(colspan = 4, a("Brown et al. 2026", href = "https://doi.org/10.1002/jcsm.70260", target = "_blank")),
            tags$th(colspan = 2, a("Kim et al. 2025", href = "https://doi.org/10.1038/s43018-025-00975-6", target = "_blank")),
            tags$th(colspan = 2, a("Pryce et al. 2024", href = "https://doi.org/10.1016/j.celrep.2024.114925", target = "_blank")),
            tags$th(colspan = 2, a("Pryce et al. 2024", href = "https://doi.org/10.1016/j.celrep.2024.114925", target = "_blank")),
            tags$th(colspan = 2, a("Zhang et al. 2024", href = "https://doi.org/10.1016/j.celrep.2024.114587", target = "_blank"))
          )
        ),
        tags$tbody(
          tags$tr(tags$th("Data availability", scope="row"),
            tags$td(colspan = 2, a("https://zenodo.org/records/11090497", href = "https://zenodo.org/records/11090497", target = "_blank")),
            tags$td(colspan = 4, a("https://github.com/musclesci/CachexiaMuscleTimecourse.git", href = "https://github.com/musclesci/CachexiaMuscleTimecourse.git", target = "_blank")),
            tags$td(colspan = 2, "GSE211300"),
            tags$td(colspan = 2, "GSE248800"),
            tags$td(colspan = 2, "GSE248800"),
            tags$td(colspan = 2, "GSE272085")
          ),
          tags$tr(tags$th("Sequencing type", scope="row"),
            tags$td(colspan = 2, "Single-nucleus"),
            tags$td(colspan = 4, "Single-cell"),
            tags$td(colspan = 2, "Single-cell"),
            tags$td(colspan = 2, "Single-cell"),
            tags$td(colspan = 2, "Single-cell"),
            tags$td(colspan = 2, "Single-nucleus")
          ),
          tags$tr(tags$th("Cell preparation", scope="row"),
            tags$td(colspan = 2, "10x Chromium Next GEM Single Cell 3′ Reagent Kits (v3.1)"),
            tags$td(colspan = 4, "10x Chromium Next GEM Single Cell 3′ Reagent Kits (v3.1)"),
            tags$td(colspan = 2, "10x Chromium Next GEM Single Cell 3′ Reagent Kits (v3.1)"),
            tags$td(colspan = 2, "Not specified"),
            tags$td(colspan = 2, "Not specified"),
            tags$td(colspan = 2, "10x Next GEM single-cell Multiome ATAC + Gene Expression kit")
          ),
          tags$tr(tags$th("Sequencing platform", scope="row"),
            tags$td(colspan = 2, "Illumina HiSeq X"),
            tags$td(colspan = 4, "Illumina NextSeq 500"),
            tags$td(colspan = 2, "Illumina NovaSeq 6000"),
            tags$td(colspan = 2, "Illumina NovaSeq 6000"),
            tags$td(colspan = 2, "Illumina NovaSeq 6000"),
            tags$td(colspan = 2, "Illumina NextSeq 500")
          ),
          tags$tr(tags$th("Mouse background", scope="row"),
            tags$td(colspan = 2, "C57BL/6"),
            tags$td(colspan = 4, "C57BL/6"),
            tags$td(colspan = 2, "tdTomato flfl;Cdh5-CreERT2,ECtdTomato"),
            tags$td(colspan = 2, "CD2F1"),
            tags$td(colspan = 2, "KrasLSL-G12D/+, Ptf1aER-Cre/+, Ptenf/f"),
            tags$td(colspan = 2, "KrasLSL G12D/+;Ink4afl/fl;Ptf1aCre/+")
          ),
          tags$tr(tags$th("Cancer type", scope="row"),
            tags$td(colspan = 2, "Subcutaneous LLC (lung)"),
            tags$td(colspan = 4, "Subcutaneous LLC (lung)"),
            tags$td(colspan = 2, "Subcutaneous B16F10 (melanoma)"),
            tags$td(colspan = 2, "Subcutaneous C26 (colon)"),
            tags$td(colspan = 2, "Tamoxifen-inducible KPP (pancreatic)"),
            tags$td(colspan = 2, "Spontaneous KIC (pancreatic)")
          ),
          tags$tr(tags$th("Mouse inoculation age (weeks)", scope="row"),
            tags$td(colspan = 2, "8-12"),
            tags$td(colspan = 4, "12"),
            tags$td(colspan = 2, "10"),
            tags$td(colspan = 2, "Not specified"),
            tags$td(colspan = 2, "3 (tamoxifen administration)"),
            tags$td(colspan = 2, "n/a (spontaneous)")
          ),
          tags$tr(tags$th("Mouse collection time (days)", scope="row"),
            tags$td(colspan = 2, "16"),
            tags$td(colspan = 4, "17, 14, 17, 24"),
            tags$td(colspan = 2, "21"),
            tags$td(colspan = 2, "21"),
            tags$td(colspan = 2, "According to collection criteria"),
            tags$td(colspan = 2, "66-82")
          ),
          tags$tr(tags$th("Sex", scope="row"),
            tags$td(colspan = 2, "Male"),
            tags$td(colspan = 4, "Male"),
            tags$td(colspan = 2, "Male and Female"),
            tags$td(colspan = 2, "Male"),
            tags$td(colspan = 2, "Male"),
            tags$td(colspan = 2, "Female")
          ),
          tags$tr(tags$th("Condition", scope="row"),
            tags$td("Control"),
            tags$td("Tumour-bearing"),
            tags$td("Control"),
            tags$td("2-weeks tumour-bearing"),
            tags$td("2.5-weeks tumour-bearing"),
            tags$td("3.5-weeks tumour-bearing"),
            tags$td("Control"),
            tags$td("Tumour-bearing"),
            tags$td("Control"),
            tags$td("Tumour-bearing"),
            tags$td("Control"),
            tags$td("Tumour-bearing"),
            tags$td("Control"),
            tags$td("Tumour-bearing")
          ),
          tags$tr(tags$th("Sample numbers", scope="row"),
            tags$td("6 (pooled)"),
            tags$td("6 (pooled)"),
            tags$td("3 (pooled)"),
            tags$td("3 (pooled)"),
            tags$td("3 (pooled)"),
            tags$td("3 (pooled)"),
            tags$td("3 (pooled)"),
            tags$td("4 (pooled)"),
            tags$td("2"),
            tags$td("2"),
            tags$td("1"),
            tags$td("1"),
            tags$td("3 (pooled)"),
            tags$td("3 (pooled)")
          ),
          tags$tr(tags$th("Cell number (post filtered)", scope="row"),
            tags$td("5,835"),
            tags$td("5,385"),
            tags$td("7,786"),
            tags$td("8,556"),
            tags$td("6,817"),
            tags$td("6,085"),
            tags$td("7,002"),
            tags$td("11,802"),
            tags$td("8,836"),
            tags$td("15,096"),
            tags$td("18,831"),
            tags$td("21,001"),
            tags$td("3,346"),
            tags$td("4,591"),
          )
        )
      )
    ),

    br(),

    h4("Table 2: Mouse tumour samples single-cell RNA-sequencing dataset demographics", class = "mt-4"),
    tags$div(
      class = "table-responsive",
      tags$table(
        class = "table table-striped table-bordered table-hover align-middle",
        tags$thead(
          tags$tr(tags$th("Study"),
            tags$th(a("Ghasemi et al. 2023", href = "https://doi.org/10.1038/s43018-023-00668-y", target = "_blank")),
            tags$th(a("Lytle et al. 2019", href = "https://doi.org/10.1016/j.cell.2019.03.010", target = "_blank")),
            tags$th(a("Yang et al. 2024", href = "https://doi.org/10.1186/s12967-024-05118-6", target = "_blank")),
            tags$th(a("van Baarle et al. 2024", href = "https://doi.org/10.1038/s41467-024-50438-2", target = "_blank"))
          )
        ),
        tags$tbody(
          tags$tr(tags$th("Data availability", scope="row"),
            tags$td("GSE228014"),
            tags$td("GSE126388"),
            tags$td("GSE256051"),
            tags$td("GSE231804")
          ),
          tags$tr(tags$th("Cell preparation", scope="row"),
            tags$td("10x Chromium Next GEM Single Cell 3′ Reagent Kits (v3.1)"),
            tags$td("10x Chromium Single Cell 3′ GEM library and gel bead kit (v2)"),
            tags$td("SeekOne MM Single Cell 3′ library preparation kit"),
            tags$td("10x Chromium Next GEM Single Cell 5′ kit")
          ),
          tags$tr(tags$th("Sequencing platform", scope="row"),
            tags$td("Illumina NextSeq 6000"),
            tags$td("Illumina NextSeq 500"),
            tags$td("Illumina NextSeq 6000"),
            tags$td("Illumina HiSeq 4000, Illumina NovaSeq 6000")
          ),
          tags$tr(tags$th("Mouse background", scope="row"),
            tags$td("C57BL/6"),
            tags$td("LSL-KrasG12D/+, ; Trp53f/f; Ptf1a-Cre"),
            tags$td("C57BL/6N"),
            tags$td("C57BL/6J")
          ),
          tags$tr(tags$th("Cancer type", scope="row"),
            tags$td("Subcutaneous B16F10"),
            tags$td("Spontaneous KPC"),
            tags$td("Subcutaneous LLC"),
            tags$td("Intracolonic MC38 (immune cells)")
          ),
          tags$tr(tags$th("Mouse inoculation age (weeks)", scope="row"),
            tags$td("6-9"),
            tags$td("Spontaneous"),
            tags$td("6-8"),
            tags$td("8-16")
          ),
          tags$tr(tags$th("Mouse collection time (days)", scope="row"),
            tags$td("According to collection criteria"),
            tags$td("According to collection criteria (10-12 weeks of age)"),
            tags$td("According to collection criteria"),
            tags$td("21")
          ),
          tags$tr(tags$th("Sex", scope="row"),
            tags$td("Female"),
            tags$td("Male and Female"),
            tags$td("Female"),
            tags$td("Male")
          ),
          tags$tr(tags$th("Sample numbers", scope="row"),
            tags$td("3"),
            tags$td("2 (with 4 technical replicates each)"),
            tags$td("3 (pooled)"),
            tags$td("3")
          ),
          tags$tr(tags$th("Sample IDs", scope="row"),
            tags$td("GSM7112549 –GSM 7112551"),
            tags$td("GSM3597633 – GSM3597640"),
            tags$td("GSM8084789"),
            tags$td("GSM7300848 – GSM7300850")
          ),
          tags$tr(tags$th("Cell number (post-filtered)", scope="row"),
            tags$td("579"),
            tags$td("2,095"),
            tags$td("7,070"),
            tags$td("3,598")
          )
        )
      )
    ),

    hr(),

    # -------------------------
    # References Section
    # -------------------------
    h3("References"),
    
    p("To reference the use of the integrated dataset, the MyInCAT resource, and/or any associated codes, please cite:"),
    tags$ul(
      tags$li("Brown, A., Lafond, I., De Lisio, M., Wiper-Bergeron, N. (2026). Myofibre catabolism in cancer cachexia is driven by indirect immune and loss of maintenance-related signaling. ", tags$i("(In submission)"))
    ),
    
    p("Please also cite the individual datasets included in your analyses:"),
    
    h5("Skeletal Muscle Datasets"),
    tags$ul(
      tags$li("Agca, S.", tags$i("et al."), "Tumour‐induced alterations in single‐nucleus transcriptome of atrophying muscles indicate enhanced protein degradation and reduced oxidative metabolism.", tags$i("J cachexia sarcopenia muscle"), tags$b("15"), ", 1898–1914 (2024)."),
      tags$li("Brown, A.", tags$i("et al."), "Single‐Cell RNA‐Sequencing Reveals Cachectic Satellite Cell Population in Muscle of Male Mice With Cancer Cachexia.", tags$i("J cachexia sarcopenia muscle"), tags$b("17"), ", e70260 (2026)."),
      tags$li("Kim, Y.-M.", tags$i(" et al. "), "Skeletal muscle endothelial dysfunction through the activin A–PGC1α axis drives progression of cancer cachexia.", tags$i("Nat Cancer"), tags$b("6"), ", 1350–1369 (2025)."),
      tags$li("Pryce, B. R.", tags$i(" et al. "), "Muscle inflammation is regulated by NF-κB from multiple cells to control distinct states of wasting in cancer cachexia.", tags$i("Cell Reports"), tags$b("43"), ", 114925 (2024)."),
      tags$li("Zhang, Y.", tags$i(" et al. "), "A molecular pathway for cancer cachexia-induced muscle atrophy revealed at single-nucleus resolution.", tags$i(" Cell Reports "), tags$b("43"), ", 114587 (2024).")
    ),
    
    h5("Tumour Datasets"),
    tags$ul(
      tags$li("Ghasemi, A.", tags$i("et al."), "Cytokine-armed dendritic cell progenitors for antigen-agnostic cancer immunotherapy.", tags$i("Nat Cancer"), tags$b("5"), ", 240–261 (2023)."),
      tags$li("Lytle, N. K.", tags$i("et al."), "A Multiscale Map of the Stem Cell State in Pancreatic Adenocarcinoma.", tags$i("Cell "), tags$b("177"), ", 572-586.e22 (2019)."),
      tags$li("Yang, H.", tags$i("et al."), "Single-cell RNA sequencing reveals recruitment of the M2-like CCL8high macrophages in Lewis lung carcinoma-bearing mice following hypofractionated radiotherapy.", tags$i("J Transl Med"), tags$b("22"), ", 306 (2024)."),
      tags$li("Van Baarle, L.", tags$i("et al."), "IL-1R signaling drives enteric glia-macrophage interactions in colorectal cancer.", tags$i("Nat Commun"), tags$b("15"), ", 6079 (2024).")
    ),
    
    h5("R Packages Used for the Analyses"),
    tags$ul(
      tags$li(strong("CellChat:"), "Jin, S.", tags$i("et al."), "Inference and analysis of cell-cell communication using CellChat.", tags$i("Nat Commun"), tags$b("12"), ", 1088 (2021)."),
      tags$li(strong("ggplot2:"), "Wickham, H.", tags$i("Ggplot2: Elegant Graphics for Data Analysis."), "(Springer-Verlag, New York, NY, 2016)."),
      tags$li(strong("clusterProfiler:"), "Xu, S.", tags$i("et al."), "Using clusterProfiler to characterize multiomics data.", tags$i("Nat Protoc"), tags$b("19"), ", 3292–3320 (2024)."),
      tags$li(strong("org.Mm.eg.db:"), "Carlson, M.", tags$i("Org.Mm.Eg.Db: Genome-Wide Annotation for Mouse. R Package Version 3.19.1."), "(2024)."),
      tags$li(strong("Seurat:"), "Stuart, T.", tags$i("et al."), "Comprehensive Integration of Single-Cell Data.", tags$i("Cell"), tags$b("177"), ", 1888-1902.e21 (2019)."),
    ),

    hr(),

    # -------------------------
    # Footer Section
    # -------------------------
    h3("Inquiries"),
    p("For any questions, please contact: ", a("musclesci.lab@gmail.com", href = "mailto:musclesci.lab@gmail.com")),

    br(),
    p("Developed by Isaac Lafond", class = "fst-italic text-center small")
    # p("Version 0.1.0, Developed by Isaac Lafond", class = "fst-italic text-center small")
  )
}
