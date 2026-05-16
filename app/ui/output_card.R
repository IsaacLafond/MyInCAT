# Reusable UI Component
output_card <- function(
  header        = NULL,
  footer        = NULL,
  aspect_ratio  = NULL,   # NULL = fill entire body; "W/H" = maintain ratio
  content
) {
  # Chrome = everything above the content: navbar + optional header + padding
  chrome <- paste0(
    "calc(var(--bslib-navbar-height, 55px)", # navbar height
    if (!is.null(header)) " + 38px" else "", # card header if present
    if (!is.null(footer)) " + 38px" else "", # card footer if present
    " + var(--_padding, 1.5rem) * 2)" # padding around content
  )

  body <- if (!is.null(aspect_ratio)) {
    card_body(
      padding  = 0,
      fillable = FALSE,
      fill     = TRUE,            # body still stretches to fill the card
      style    = paste0(
        "display: flex;",
        "align-items: center;",
        "justify-content: center;"
      ),
      div(
        style = paste0(
          "width: 100%;",
          "aspect-ratio: ", aspect_ratio, ";",
          "max-height: calc(100dvh - ", chrome, ");",
          "max-width: calc((100dvh - ", chrome, ") * ", aspect_ratio, ");"
        ),
        content
      )
    )
  } else {
    card_body(
      fillable = TRUE,
      fill     = TRUE,
      content
    )
  }

  card(
    fill        = TRUE,
    full_screen = TRUE,
    max_height  = "calc(100dvh - var(--bslib-navbar-height, 55px) - var(--_padding, 1.5rem) * 2)",
    if (!is.null(header)) card_header(header),
    body,
    if (!is.null(footer)) card_footer(footer)
  )
}
