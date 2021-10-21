# Java script function to replace gene IDs with Ensembl gene links.
js_link <- DT::JS("function(row, data) {
    let id = data[1];
    var name = data[2];
    if (!name) name = 'Unknown';
    let url = `https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=${id}`;
    $('td:eq(1)', row).html(`<a href=\"${url}\" target=\"_blank\">${name}</a>`);
}")

server <- function(input, output, session) {
    preset <- preset_editor_server("preset_editor")

    # Show the customized slider for setting the required number of species.
    output$n_species_slider <- renderUI({
        sliderInput(
            "n_species",
            "Required number of species per gene",
            min = 0,
            max = length(preset()$species_ids),
            step = 1,
            value = 10
        )
    })

    # Compute the results according to the preset.
    analysis <- reactive({
        preset <- preset()

        # Perform the analysis cached based on the preset's hash.
        results <- withProgress(
            message = "Analyzing genes",
            value = 0.0, {
                geposan::analyze(preset, function(progress) {
                    setProgress(progress)
                })
            }
        )

        # Add all gene information to the results.
        results <- merge(
            results,
            genes,
            by.x = "gene",
            by.y = "id"
        )

        # Count included species from the preset per gene.
        genes_n_species <- geposan::distances[
            species %chin% preset$species_ids,
            .(n_species = .N),
            by = "gene"
        ]

        setkey(genes_n_species, gene)

        # Exclude genes with too few species.
        results[genes_n_species[gene, n_species] >= input$n_species]
    })

    # Rank the results.
    results <- methods_server("methods", analysis)

    # Apply the cut-off score to the ranked results.
    results_filtered <- reactive({
        results()[score >= input$cutoff / 100]
    })

    output$genes <- DT::renderDT({
        method_ids <- sapply(methods, function(method) method$id)
        method_names <- sapply(methods, function(method) method$name)
        columns <- c("rank", "gene", "name", "chromosome", method_ids, "score")
        column_names <- c("", "Gene", "", "Chromosome", method_names, "Score")

        dt <- DT::datatable(
            results_filtered()[, ..columns],
            rownames = FALSE,
            colnames = column_names,
            style = "bootstrap",
            fillContainer = TRUE,
            extensions = "Scroller",
            options = list(
                rowCallback = js_link,
                columnDefs = list(list(visible = FALSE, targets = 2)),
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE
            )
        )

        DT::formatPercentage(dt, c(method_ids, "score"), digits = 1)
    })

    output$copy <- renderUI({
        results <- results_filtered()

        gene_ids <- results[, gene]
        names <- results[name != "", name]

        genes_text <- paste(gene_ids, collapse = "\n")
        names_text <- paste(names, collapse = "\n")

        splitLayout(
            cellWidths = "auto",
            rclipboard::rclipButton(
                "copy_ids_button",
                "Copy gene IDs",
                genes_text,
                icon = icon("clipboard")
            ),
            rclipboard::rclipButton(
                "copy_names_button",
                "Copy gene names",
                names_text,
                icon = icon("clipboard")
            )
        )
    })

    output$scatter <- plotly::renderPlotly({
        results <- results_filtered()

        gene_ids <- results[input$genes_rows_selected, gene]
        genes <- genes[id %chin% gene_ids]
        species <- species[id %chin% preset()$species_ids]

        scatter_plot(results, species, genes)
    })

    output$assessment_synopsis <- renderText({
        reference_gene_ids <- preset()$reference_gene_ids

        included_reference_count <- results_filtered()[
            gene %chin% reference_gene_ids,
            .N
        ]

        reference_results <- results()[gene %chin% reference_gene_ids]
        total_reference_count <- nrow(reference_results)

        if (total_reference_count > 0) {
            mean_rank <- as.character(round(
                reference_results[, mean(rank)],
                digits = 1
            ))

            min_rank <- as.character(reference_results[, min(rank)])
            max_rank <- as.character(reference_results[, max(rank)])
        } else {
            mean_rank <- "Unknown"
            min_rank <- "Unknown"
            max_rank <- "Unknown"
        }

        sprintf(
            "Included reference genes: %i/%i<br> \
            Mean rank of reference genes: %s<br> \
            First rank of reference genes: %s<br> \
            Last rank of reference genes: %s",
            included_reference_count,
            total_reference_count,
            mean_rank,
            min_rank,
            max_rank
        )
    })

    output$rank_plot <- plotly::renderPlotly({
        rank_plot(
            results(),
            preset()$reference_gene_ids,
            input$cutoff / 100
        )
    })

    output$gost <- plotly::renderPlotly({
        if (input$enable_gost) {
            result <- gprofiler2::gost(
                results_filtered()[, gene],
                ordered_query = TRUE
            )

            gprofiler2::gostplot(
                result,
                capped = FALSE,
                interactive = TRUE
            )
        } else {
            NULL
        }
    })
}
