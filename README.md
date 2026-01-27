# MyInCAT

Welcome to MyInCAT (Myogenic single-cell Integration of Cachexia Transcriptomics data), a resource for exploring single-cell RNA-sequencing data in cancer cachexia! We have integrated single-cell and single-nucleus RNA sequencing data containing 130,996 cells from 6 studies, 5 tumour models to perform differential gene expression, pathway, and cell communication analyses on skeletal muscle from mouse models of cancer cachexia. Use this resource to generate hypotheses, validate findings, and generate publication-ready plots.

## Project structure:
```
MyInCAT/
├── Alex/           # R markdown files of the analysis code to be built into web app.
├── app/            # App source files
│   ├── data/       # R datasets/data object
│   ├── modules/    # Shiny web app modules
│   ├── ui/         # UI components
│   ├── utils/
│   ├── www/        # Static web content
│   └── app.R       # App entry point
├── .dockerignore
├── .gitignore
├── Dockerfile
└── README.md       # Project overview
```

## Usage:

**Dev run command (source files as volume for dynamic changes):**
1. Build dev image:
```bash
docker build -t shiny-dev .
```
2. Run dev image:
```bash
docker run --rm -itd \
    -p 3838:3838 \
    -v "$(pwd)/app:/app" \
    shiny-dev
```

**Prod image run command (source files static copied to image):**
1. uncomment `# COPY . /app` in Dockerfile
2. Build production image:
```bash
docker build -t MyInCAT-shiny
```
3. Run production image or hook into shiny proxy deployment:
```bash
docker run -d \
    -v "$(/path/datafiles):/app/data \
    MyInCAT-shiny
```