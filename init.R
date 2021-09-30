source("clustering.R")
source("correlation.R")
source("input.R")
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

# Load processed data

all_species <- species[, id]
replicative_species <- species[replicative == TRUE, id]
all_genes <- genes[, id]
tpe_old_genes <- genes[suggested | verified == TRUE, id]

clustering_all <- run_cached(
    "clustering_all",
    process_clustering,
    distances,
    all_species,
    all_genes
)

clustering_replicative <- run_cached(
    "clustering_replicative",
    process_clustering,
    distances,
    replicative_species,
    all_genes
)

correlation_all <- run_cached(
    "correlation_all",
    process_correlation,
    distances,
    all_species,
    all_genes,
    tpe_old_genes
)

correlation_replicative <- run_cached(
    "correlation_replicative",
    process_correlation,
    distances,
    replicative_species,
    all_genes,
    tpe_old_genes
)

# Merge processed data as well as gene information.

results_all <- merge(
    genes,
    clustering_all,
    by.x = "id",
    by.y = "gene"
)

results_all <- merge(
    results_all,
    correlation_all,
    by.x = "id",
    by.y = "gene"
)

results_replicative <- merge(
    genes,
    clustering_replicative,
    by.x = "id",
    by.y = "gene"
)

results_replicative <- merge(
    results_replicative,
    correlation_replicative,
    by.x = "id",
    by.y = "gene"
)

# Rename `id` columns to `gene`.

setnames(results_all, "id", "gene")
setnames(results_replicative, "id", "gene")
