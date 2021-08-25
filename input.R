library(data.table)
library(rlog)

#' Merge genome data from files in `path` into `data.table`s.
#'
#' The result will be a list with named elements:
#' - `genes` will be a table with metadata on human genes.
#' - `species` will contain metadata on each species.
#' - `distances` will contain each species' genes' distances to the telomere.
#'
#' @seealso [load_data_cached()]
load_input <- function(path) {
    genes <- fread(paste(path, "genes.tsv", sep = "/"))
    original_species <- fread(paste(path, "species.csv", sep = "/"))

    species <- data.table(
        id = character(),
        label = character(),
        median_distance = numeric()
    )

    distances <- data.table(
        species = character(),
        gene = integer(),
        distance = integer()
    )

    # Each file will contain data on one species.
    file_names <- list.files(paste(path, "genomes", sep = "/"))
    n_species <- length(file_names)

    for (i in seq_along(file_names)) {
        file_name <- file_names[i]
        species_id <- strsplit(file_name, split = ".", fixed = TRUE)[[1]][1]
        species_path <- paste(path, "genomes", file_name, sep = "/")

        log_info(sprintf(
            "Reading species %i/%i (%s)", i, n_species, species_id
        ))

        species_distances <- fread(species_path)

        # Compute the median distance across all genes of this species and
        # add it to the species table along other static data.
        species <- rbindlist(list(species, data.table(
            id = species_id,
            label = original_species[id == species_id, label],
            median_distance = median(species_distances[, dist])
        )))

        species_distances <- data.table(
            species = species_id,
            gene = species_distances[, geneid],
            distance = species_distances[, dist]
        )

        distances <- rbindlist(list(distances, species_distances))
    }

    # Order species by their median distance.
    setorder(species, median_distance)

    list(
        genes = genes,
        species = species,
        distances = distances
    )
}