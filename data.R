library(data.table)

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

#' Merge genome data from files in `path` into `data.table`s.
#'
#' The result will be a list with named elements:
#' - `genes` will be a table with metadata on human genes.
#' - `species` will contain metadata on each species.
#' - `distances` will contain each species' genes' distances to the telomere.
#'
#' @seealso [load_data_cached()]
load_data <- function(path) {
    genes <- fread(paste(path, "genes.tsv", sep = "/"))
    original_species <- fread(paste(path, "species.csv", sep = "/"))

    species <- data.table(
        id = character(),
        label = character(),
        median_distance = numeric()
    )

    distances <- data.table(geneid = integer())

    # Each file will contain data on one species.
    file_names <- list.files(paste(path, "genomes", sep = "/"))

    for (file_name in file_names) {
        species_id <- strsplit(file_name, split = ".", fixed = TRUE)[[1]][1]

        # Only continue for replicatively aging species.
        # TODO: Which other species should be included?
        if (original_species[id == species_id, group] == "replicative") {
            species_path <- paste(path, "genomes", file_name, sep = "/")
            species_distances <- fread(species_path)

            # Compute the median distance across all genes of this species and
            # add it to the species table along other static data.
            species <- rbindlist(list(species, data.table(
                id = species_id,
                label = original_species[id == species_id, label],
                median_distance = median(species_distances[, dist])
            )))

            # Column names have to be unique for each species.
            setnames(species_distances, "dist", species_id)

            distances <- merge(distances, species_distances, all = TRUE)
        }
    }

    list(
        genes = genes,
        species = species,
        distances = distances
    )
}