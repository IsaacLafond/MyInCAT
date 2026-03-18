with_custom_spinner <- function(expr) {
  withSpinner(
    expr,
    image = "www/loading_cat.png",
    hide.ui = FALSE
  )
}