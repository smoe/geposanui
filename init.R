source("input.R")
source("methods.R")
source("util.R")

# Load input data

species <- run_cached("input_species", retrieve_species)
genes <- run_cached("input_genes", retrieve_genes)

distances <- run_cached(
    "input_distances",
    retrieve_distances,
    species[, id],
    genes[, id]
)

all_species <- species[, id]
replicative_species <- species[replicative == TRUE, id]
all_genes <- genes[, id]
tpe_old_genes <- genes[suggested | verified == TRUE, id]

# Apply all methods for all species

results_all <- merge(
    genes,
    distances[, .(n_species = .N), by = "gene"],
    by.x = "id",
    by.y = "gene"
)

setnames(results_all, "id", "gene")

for (method in methods) {
    method_results <- run_cached(
        sprintf("%s_all", method$id),
        method$fn,
        distances,
        all_species,
        all_genes,
        tpe_old_genes
    )

    setnames(method_results, "score", method$id)

    results_all <- merge(
        results_all,
        method_results,
    )
}

# Apply all methods for replicatively aging species

results_replicative <- merge(
    genes,
    distances[
        species %chin% species_ids_replicative,
        .(n_species = .N),
        by = gene
    ],
    by.x = "id",
    by.y = "gene"
)

setnames(results_replicative, "id", "gene")

for (method in methods) {
    method_results <- run_cached(
        sprintf("%s_replicative", method$id),
        method$fn,
        distances,
        replicative_species,
        all_genes,
        tpe_old_genes
    )

    setnames(method_results, "score", method$id)

    results_replicative <- merge(
        results_replicative,
        method_results,
    )
}