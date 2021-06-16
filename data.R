library(data.table)

source("species.R")

#' Load and preprocess input data from `path`.
#'
#' A file named `cache.rds` will be created within that directory to reuse the
#' results for future runs. To forcefully recompute, delete that file.
#'
#' @seealso [load_data()]
load_data_cached <- function(path) {
    cache_file <- paste(path, "cache.rds", sep = "/")

    if (!file.exists(cache_file)) {
        # If the cache file doesn't exist, we have to do the computation.
        data <- load_data("input")

        # The results are cached for the next run.
        saveRDS(data, cache_file)

        data
    } else {
        # If the cache file exists, we restore the data from it.
        readRDS(cache_file)
    }
}

#' Merge genome data from files in `path` into `data.table`s.
#'
#' The result will be a list with two items:
#' - `genes` will be a table with one row per unique `geneid` and multiple
#'   columns per species containing the data of interest. 
#' - `species` will contain information that is useful to be accessed by
#'   species.
#'
#' @seealso [load_data_cached()]
load_data <- function(path) {
    # The resulting table for information by gene. For each species, columns
    # will be appended.
    genes_table <- data.table(geneid = integer())

    # The resulting table for information by species. This will result in a
    # warning, because all median_distance values will be filled with `NA`
    # (correctly).
    species_table <- data.table(species, median_distance = numeric())

    file_names <- list.files(path, "*_raw.txt")

    for (file_name in file_names) {
        species_id <- strsplit(file_name, split = "_")[[1]][1]
        genes_table_for_species <- fread(paste(path, file_name, sep = "/"))

        # Fill in the new column of the species table (`median_distance`).
        species_table[
            id == species_id,
            median_distance := median(genes_table_for_species[, dist])
        ]

        # Column names have to be unique for each species.
        colnames(genes_table_for_species)[c(2, 3, 4)] <- c(
            paste(species_id, c("dist", "name", "chromosome"), sep = "_")
        )

        # Add new genes as rows as well as new columns for this species.
        genes_table <- merge(
            genes_table,
            genes_table_for_species,
            all = TRUE
        )
    }

    list(
        genes = genes_table,
        species = species_table
    )
}
