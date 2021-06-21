library(dplyr)
library(readr)
library(tibble)

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
        data <- load_data(path)

        # The results are cached for the next run.
        saveRDS(data, cache_file)

        data
    } else {
        # If the cache file exists, we restore the data from it.
        readRDS(cache_file)
    }
}

#' Merge genome data from files in `path` into `tibble`s.
#'
#' The result will be a list with two named elements:
#' - `genes` will be a table with one row per unique `geneid` and multiple
#'   columns per species containing the data of interest.
#' - `species` will contain additional information on each species.
#'
#' @seealso [load_data_cached()]
load_data <- function(path) {
    # The resulting table for information by species.
    species <- read_csv(paste(path, "species.csv", sep = "/"))

    # The resulting table for information by gene. For each species, columns
    # will be appended.
    genes <- tibble(geneid = integer())

    # Each file will contain data on one species.
    file_names <- list.files(path, "*_raw.txt")

    # Table containing additional columns to be added to the species table.
    species_computed <- tibble(
        id = character(),
        median_distance = numeric()
    )

    for (file_name in file_names) {
        species_id <- strsplit(file_name, split = "_")[[1]][1]
        genes_for_species <- read_tsv(paste(path, file_name, sep = "/"))

        # Compute the median distance across all genes of this species.
        median_distance <- genes_for_species %>%
            select(dist) %>%
            summarise(median_distance = median(dist)) %>%
            pull(median_distance)

        # Cache the values to be added to the species table.
        species_computed <- species_computed %>% add_row(
            id = species_id,
            median_distance = median_distance,
        )

        # Column names have to be unique for each species.
        genes_for_species <- rename_with(
            genes_for_species,
            ~ paste(species_id, .x, sep = "_"),
            c(dist, name, chromosome)
        )

        genes <- full_join(genes, genes_for_species)
    }

    species <- left_join(species, species_computed)

    list(
        genes = genes,
        species = species
    )
}