#' Custom CSS to tweak the rendering.
#' @noRd
custom_css <- function() {
  tags$head(
    tags$style(HTML(
      ".flow-layout > div {",
      "display: inline-block;",
      "vertical-align: top;",
      "margin-right: 12px;",
      "}",
      "h5 { margin-top: 0.5rem; margin-bottom: 1rem; font-weight: bold; }",
      ".navbar-brand { font-weight: bold; }",
      # Undo changes in Bootstrap theme:
      ".nav-underline .nav-link { border-bottom: 0; }",
      ".nav-underline .nav-link.active { font-weight: normal; }",
      # Fix slider inputs floating above dropdown menu:
      ".irs--shiny .irs-bar { z-index: 1; }",
      ".irs--shiny .irs-handle { z-index: 1; }"
    ))
  )
}
