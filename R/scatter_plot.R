# Draw a scatter plot containing gene positions.
#
# @param results Results from [`process_input()`].
# @param species Species to be displayed.
# @param genes Genes to be displayed.
scatter_plot <- function(results, species, genes) {
    species_ids <- species[, id]

    data <- merge(
        genes[, .(id, name)],
        geposan::distances[species %in% species_ids],
        by.x = "id", by.y = "gene"
    )

    data[name == "", name := "Unknown"]

    plotly::plot_ly(
        data = data,
        x = ~species,
        y = ~distance,
        color = ~id,
        name = ~name,
        type = "scatter",
        mode = "markers"
    ) |> plotly::layout(
        xaxis = list(
            title = "Species",
            tickvals = species_ids,
            ticktext = species[, name]
        ),
        yaxis = list(title = "Distance to telomeres [Bp]")
    )
}
