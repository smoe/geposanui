library(data.table)

source("process/util.R")

# Load input data

source("process/input.R")

species <- run_cached("inputs/species", retrieve_species)
genes <- run_cached("inputs/genes", retrieve_genes)

distances <- run_cached(
    "inputs/distances",
    retrieve_distances,
    species[, id],
    genes[, id]
)

genes <- merge(
    genes,
    distances[, .(n_species = .N), by = "gene"],
    by.x = "id",
    by.y = "gene"
)

source("process/methods.R")
source("process/presets.R")

#' Apply all methods with the specified preset without caching.
process_priv <- function(preset) {
    results <- data.table(gene = genes[, id])

    for (method in methods) {
        method_results <- method$fn(distances, genes[, id], preset)
        setnames(method_results, "score", method$id)

        results <- merge(
            results,
            method_results
        )
    }

    results
}

#' Apply all methods with the specified preset.
#'
#' The result will be cached by the preset's hash and restored from cache, if
#' possible. The return value is a `data.table` with one row for each gene
#' identified by it's ID (`gene` column). The additional columns contain the
#' resulting per method and are named after the method IDs.
process <- function(preset) {
    run_cached(
        sprintf("results/%s", rlang::hash(preset)),
        process_priv,
        preset
    )
}