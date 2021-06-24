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
#' The result will be a list with named elements:
#' - `genes` will be a table with metadata on human genes.
#' - `species` will contain metadata on each species.
#' - `distances` will contain each species' genes' distances to the telomere.
#'
#' @seealso [load_data_cached()]
load_data <- function(path) {
    genes <- read_tsv(paste(path, "genes.tsv", sep = "/"))
    species <- read_csv(paste(path, "species.csv", sep = "/"))
    distances <- tibble(geneid = integer())

    # Each file will contain data on one species.
    file_names <- list.files(paste(path, "genomes", sep = "/"))

    # Table containing additional columns to be added to the species table
    # later.
    species_computed <- tibble(
        id = character(),
        median_distance = numeric()
    )

    for (file_name in file_names) {
        species_id <- strsplit(file_name, split = ".", fixed = TRUE)[[1]][1]
        species_path <- paste(path, "genomes", file_name, sep = "/")
        species_distances <- read_tsv(species_path)

        # Compute the median distance across all genes of this species.
        median_distance <- species_distances %>%
            select(dist) %>%
            summarise(median_distance = median(dist)) %>%
            pull(median_distance)

        # Cache the values to be added to the species table.
        species_computed <- species_computed %>% add_row(
            id = species_id,
            median_distance = median_distance,
        )

        # Column names have to be unique for each species.
        # TODO: How to create a dynamic column name using `rename()`?
        species_distances <- species_distances %>%
            rename_with(function(x) species_id, dist)

        distances <- full_join(distances, species_distances)
    }

    # Add additional columns to the original species table.
    species <- left_join(species, species_computed)

    list(
        genes = genes,
        species = species,
        distances = distances
    )
}