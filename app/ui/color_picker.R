color_picker_ui <- function(id, label) {
  div(
    class = "d-flex justify-content-between align-center",

    tags$label(
      label,
      `for` = id
    ),

    colourInput(
      inputId = id,
      label = NULL,
      value = "#000000",
      showColour = "background",
      closeOnClick = TRUE,
      width = "45px"
    )
    
  )
}