source("process/clusteriness.R")
source("process/correlation.R")
source("process/neural.R")
source("process/proximity.R")

#' Construct a new method.
#'
#' A method describes a way to perform a computation on gene distance data that
#' results in a single score per gene. The function should accept the following
#' parameters in this order:
#'
#'  - `distances` Distance data to use.
#'  - `gene_ids` Genes to process.
#'  - `preset` Preset to apply.
#'
#' The function should return a `data.table` with the following columns:
#'
#'  - `gene` Gene ID of the processed gene.
#'  - `score` Score for the gene between 0.0 and 1.0.
#'
#' @param id Internal identifier for the method.
#' @param name Human readable name for the method.
#' @param description Short human readable description.
#' @param fn Function to perform the computation.
#'
#' @return A named list containing the arguments.
method <- function(id, name, description, fn) {
    list(
        id = id,
        name = name,
        description = description,
        fn = fn
    )
}

#' All methods to be included in the analysis.
methods <- list(
    method(
        "clusteriness",
        "Clustering",
        "Clustering of genes",
        process_clusteriness
    ),
    method(
        "correlation",
        "Correlation",
        "Correlation with known genes",
        process_correlation
    ),
    method(
        "proximity",
        "Proximity",
        "Proximity to telomeres",
        process_proximity
    ),
    method(
        "neural",
        "Neural",
        "Assessment by neural network",
        process_neural
    )
)