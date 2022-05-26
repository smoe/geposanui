#' Run the application server.
#'
#' @param gene_sets A list of predefined gene sets. This should be a named list
#'   containing vectors of gene IDs for each set. The names will be used to
#'   present the gene set throughout the user interface. You have to provide *at
#'   least one gene set* which will be selected as the initial reference gene
#'   set.
#' @param species_sets A list of predefined species sets. This should be a named
#'   list containing vectors of species IDs for each set. The names will be used
#'   to present the species set throughout the user interface.
#' @param locked Whether the application should be locked and prohibit
#'   performing custom analyses. If this is set to `TRUE`, only the predefined
#'   gene and species sets are available for customizing the analysis. This may
#'   be useful to limit resource usage on a publicly available instance.
#' @param title Set the title of the application.
#' @param port The port to serve the application on.
#'
#' @export
run_app <- function(gene_sets,
                    species_sets = NULL,
                    locked = FALSE,
                    title = "Gene Position Analysis",
                    port = 3464) {
  stopifnot(!is.null(gene_sets) & !is.null(gene_sets[[1]]))

  # These function calls make the required java scripts available.
  shinyjs::useShinyjs()
  rclipboard::rclipboardSetup()

  # Bundle of global options to redue broilerplate.
  options <- list(
    gene_sets = gene_sets,
    species_sets = species_sets,
    locked = locked,
    title = title
  )

  # Actually run the app.
  shiny::runApp(
    shiny::shinyApp(ui(options), server(options)),
    port = port
  )
}
