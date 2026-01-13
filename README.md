Project structure:

Alex: Contains R markdown files of the analysis code to be built into web app.

app: source file for all the shiny web app code.
    modules: folder of all the web app modules (ui and server components).
    ui: folder of all reuable ui components used throughout the app.
    www: static web files for the web app.
    app.R: app entry point.
data: folder holding all the data objects/datasets used by the app.

Dev run command:
1. docker build -t shiny-dev
2. docker run --rm -itd \
    -p 3838:3838 \
    -v "$(pwd)/app:/app" \
    shiny-dev

Dist image run command:
1. uncomment "# COPY . /app"
2. docker build -t MyInCAT-shiny
3. docker run -d \
    -v "$(/path/datafiles):/app/data \
    MyInCAT-shiny
or
build image and hook into shiny proxy deployment.