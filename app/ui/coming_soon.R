coming_soon <- function() {
  page_fillable(
    img(src = "www/coming_soon.png", alt = "Coming Soon", style = "display: block; margin-left: auto; margin-right: auto; height: 50%;"),
    h2("Coming Soon...", class = "text-center"),
    p("This feature is under development and will be available in a future update. Stay tuned!", class = "text-center")
  )
}