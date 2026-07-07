with_custom_spinner <- function(expr, hide.ui = FALSE, caption = NULL) {
  withSpinner(
    expr,
    image = "www/loading_cat.png",
    hide.ui = hide.ui,
    color = "#000000",
    caption = caption
  )
}