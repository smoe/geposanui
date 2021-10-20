#' Run the application server.
#'
#' @param port The port to serve the application on.
#'
#' @export
run_app <- function(port = 3464) {
    shiny::runApp(shiny::shinyApp(ui, server), port = port)
}
