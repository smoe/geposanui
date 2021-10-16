library(data.table)

#' Create a new preset.
#'
#' A preset is a combination of input values to all processing methods. The
#' preset's hash will be used to cache the results of applying those.
#'
#' @param species_ids IDs of species to include.
#' @param reference_gene_ids Reference genes to use.
#'
#' @return A named list containing the arguments.
preset <- function(species_ids, reference_gene_ids) {
    list(
        species_ids = species_ids,
        reference_gene_ids = reference_gene_ids
    )
}

#' A default preset including only replicatively aging species.
preset_replicative_species <- preset(
    species[replicative == TRUE, id],
    genes[suggested | verified == TRUE, id]
)

#' A default preset including all species.
preset_all_species <-  preset(
    species[, id],
    genes[suggested | verified == TRUE, id]
)