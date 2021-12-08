#' Run the application server.
#'
#' @param port The port to serve the application on.
#'
#' @export
run_app <- function(port = 3464) {
    # These function calls make the required java scripts available.

    shinyjs::useShinyjs()
    rclipboard::rclipboardSetup()

    # Actually run the app.

    shiny::runApp(shiny::shinyApp(ui, server), port = port)
}
