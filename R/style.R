#' Custom CSS to tweak the rendering.
#' @noRd
custom_css <- function() {
  tags$head(
    tags$style(HTML(
      ".nav-hidden { height: 0 }",
      ".flow-layout > div {",
      "display: inline-block;",
      "vertical-align: top;",
      "margin-right: 12px;",
      "}",
      ".shiny-input-container { width: auto !important; min-width: 200px; }",
      "h5 { margin-top: 0.5rem; margin-bottom: 1rem; font-weight: bold; }",
      ".navbar-brand { font-weight: bold; }"
    ))
  )
}
