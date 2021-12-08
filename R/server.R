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

    # Compute the results according to the preset.
    analysis <- reactive({
        preset <- preset()

        # Perform the analysis cached based on the preset's hash.
        analysis <- withProgress(
            message = "Analyzing genes",
            value = 0.0,
            { # nolint
                geposan::analyze(preset, function(progress) {
                    setProgress(progress)
                })
            }
        )

        analysis
    })

    # Rank the results.
    ranking <- methods_server("methods", analysis)

    # Add gene information to the results.
    results <- reactive({
        merge(
            ranking(),
            geposan::genes,
            by.x = "gene",
            by.y = "id",
            sort = FALSE
        )
    })

    # Apply the filters.
    results_filtered <- filters_server("filters", results)

    comparison_gene_ids <- comparison_editor_server("comparison_editor", preset)

    output$genes <- DT::renderDT({
        columns <- c("rank", "gene", "name", "chromosome", method_ids, "score")
        column_names <- c("", "Gene", "", "Chromosome", method_names, "Score")

        dt <- DT::datatable(
            results_filtered()[, ..columns],
            rownames = FALSE,
            colnames = column_names,
            style = "bootstrap",
            options = list(
                rowCallback = js_link,
                columnDefs = list(list(visible = FALSE, targets = 2)),
                pageLength = 25
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
        preset <- preset()
        gene_sets <- list("Reference genes" = preset$reference_gene_ids)
        comparison_gene_ids <- comparison_gene_ids()

        if (length(comparison_gene_ids) >= 1) {
            gene_sets <- c(
                gene_sets,
                list("Comparison genes" = comparison_gene_ids)
            )
        }

        geposan::plot_positions(preset$species_ids, gene_sets)
    })

    output$rank_plot <- plotly::renderPlotly({
        preset <- preset()
        gene_sets <- list("Reference genes" = preset$reference_gene_ids)
        comparison_gene_ids <- comparison_gene_ids()

        if (length(comparison_gene_ids) >= 1) {
            gene_sets <- c(
                gene_sets,
                list("Comparison genes" = comparison_gene_ids)
            )
        }

        geposan::plot_scores(
            ranking(),
            gene_sets = gene_sets,
            max_rank = results_filtered()[, max(rank)]
        )
    })

    output$rankings_plot <- plotly::renderPlotly({
        preset <- preset()
        gene_sets <- list("Reference genes" = preset$reference_gene_ids)
        comparison_gene_ids <- comparison_gene_ids()

        if (length(comparison_gene_ids) >= 1) {
            gene_sets <- c(
                gene_sets,
                list("Comparison genes" = comparison_gene_ids)
            )
        }

        all <- ranking()
        clusteriness <- geposan::ranking(all, list(clusteriness = 1))
        correlation <- geposan::ranking(all, list(correlation = 1))
        neural <- geposan::ranking(all, list(neural = 1))
        adjacency <- geposan::ranking(all, list(adjacency = 1))
        proximity <- geposan::ranking(all, list(proximity = 1))

        rankings <- list(
            "Clusteriness" = clusteriness,
            "Correlation" = correlation,
            "Neural" = neural,
            "Adjacency" = adjacency,
            "Proximity" = proximity,
            "Combined" = all
        )

        geposan::plot_rankings(rankings, gene_sets)
    })

    output$boxplot <- plotly::renderPlotly({
        preset <- preset()
        gene_sets <- list("Reference genes" = preset$reference_gene_ids)
        comparison_gene_ids <- comparison_gene_ids()

        if (length(comparison_gene_ids) >= 1) {
            gene_sets <- c(
                gene_sets,
                list("Comparison genes" = comparison_gene_ids)
            )
        }

        geposan::plot_boxplot(ranking(), gene_sets)
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
