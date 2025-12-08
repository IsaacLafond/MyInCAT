FROM rocker/shiny:4.3.2

# -----------------------------
# System dependencies
# -----------------------------
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libfftw3-dev \
    libglpk-dev \
    libgit2-dev \
    cmake \
    build-essential \
    && apt-get clean

# -----------------------------
# Install required R packages
# -----------------------------

# CRAN first
RUN R -e "install.packages(c( \
    'Seurat', 'tidyverse', 'ggplot2', 'devtools',\
    'gridExtra', 'harmony', 'NMF', 'circlize', 'msigdbr' \
))"


# Bioconductor packages
RUN R -e "install.packages('BiocManager', repos='https://cloud.r-project.org')"

RUN R -e "BiocManager::install(c( \
  'Biobase','BiocGenerics','AnnotationDbi','org.Mm.eg.db', \
  'monocle3', 'clusterProfiler','ComplexHeatmap' \
))"

# Install SeuratWrappers + CellChat (GitHub / Bioc)
RUN R -e "devtools::install_github('satijalab/seurat-wrappers')"

RUN R -e "devtools::install_github('sqjin/CellChat')"

# -----------------------------
# Copy your Shiny app
# -----------------------------
# /srv/shiny-server/ is where Shiny Server looks for apps
COPY . /srv/shiny-server/app

# -----------------------------
# Expose port & run
# -----------------------------
EXPOSE 3838

# Allow to drop into R console OR run shiny server
CMD ["/init"]