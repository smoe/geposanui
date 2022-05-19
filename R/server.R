# Java script function to replace gene IDs with Ensembl gene links.
js_link <- DT::JS("function(row, data) {
    let id = data[1];
    var name = data[2];
    if (!name) name = 'Unknown';
    let url = `https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=${id}`;
    $('td:eq(1)', row).html(`<a href=\"${url}\" target=\"_blank\">${name}</a>`);
}")

#' Create a server function for the application.
#'
#' @param options Global application options.
#' @noRd
server <- function(options) {
    function(input, output, session) {
        input_reactives <- input_page_server("input_page", options)
        preset <- input_reactives$preset
        comparison_gene_ids <- input_reactives$comparison_gene_ids

        observe({
            updateNavbarPage(
                session,
                "main_page",
                selected = "Results"
            )
        }) |> bindEvent(preset(), ignoreInit = TRUE)

        # Compute the results according to the preset.
        analysis <- reactive({
            withProgress(
                message = "Analyzing genes",
                value = 0.0,
                { # nolint
                    geposan::analyze(
                        preset(),
                        progress = function(progress) {
                            setProgress(progress)
                        },
                        include_results = FALSE
                    )
                }
            )
        }) |> bindCache(preset())

        # Rank the results.
        ranking <- methods_server("methods", analysis, comparison_gene_ids)

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

        # Server for the detailed results panel.
        results_server("results", results_filtered)

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

            rankings <- list()
            methods <- preset$methods
            all <- ranking()

            for (method in methods) {
                weights <- list()
                weights[[method$id]] <- 1.0
                rankings[[method$name]] <- geposan::ranking(all, weights)
            }

            rankings[["Combined"]] <- all

            gene_sets <- list("Reference genes" = preset$reference_gene_ids)
            comparison_gene_ids <- comparison_gene_ids()

            if (length(comparison_gene_ids) >= 1) {
                gene_sets <- c(
                    gene_sets,
                    list("Comparison genes" = comparison_gene_ids)
                )
            }

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

        output$positions_plot <- plotly::renderPlotly({
            preset <- preset()
            gene_sets <- list("Reference genes" = preset$reference_gene_ids)
            comparison_gene_ids <- comparison_gene_ids()

            if (length(comparison_gene_ids) >= 1) {
                gene_sets <- c(
                    gene_sets,
                    list("Comparison genes" = comparison_gene_ids)
                )
            }

            geposan::plot_scores_by_position(
                ranking(),
                input$positions_plot_chromosome_name,
                gene_sets = gene_sets
            )
        })

        gost <- reactive({
            withProgress(
                message = "Querying g:Profiler",
                value = 0.0,
                { # nolint
                    setProgress(0.2)
                    gprofiler2::gost(results_filtered()[, gene])
                }
            )
        })

        output$gost_plot <- plotly::renderPlotly({
            gprofiler2::gostplot(
                gost(),
                capped = FALSE,
                interactive = TRUE
            )
        })

        output$gost_details <- DT::renderDT({
            data <- data.table(gost()$result)
            setorder(data, p_value)

            data[, total_ratio := term_size / effective_domain_size]
            data[, query_ratio := intersection_size / query_size]

            data <- data[, .(
                source,
                term_name,
                total_ratio,
                query_ratio,
                p_value
            )]

            dt <- DT::datatable(
                data,
                rownames = FALSE,
                colnames = c(
                    "Source",
                    "Term",
                    "Total ratio",
                    "Query ratio",
                    "p-value"
                ),
                style = "bootstrap",
                options = list(
                    pageLength = 25
                )
            )

            dt <- DT::formatRound(dt, "p_value", digits = 4)
            dt <- DT::formatPercentage(
                dt,
                c("total_ratio", "query_ratio"),
                digits = 1
            )
        })

        output$disgenet <- DT::renderDT({
            withProgress(
                message = "Querying DisGeNET",
                value = 0.0,
                { # nolint
                    setProgress(0.2)

                    gene_names <- results_filtered()[, name]
                    gene_names <- unique(gene_names[gene_names != ""])

                    diseases <- disgenet2r::disease_enrichment(gene_names)

                    data <- data.table(diseases@qresult)

                    data <- data[, .(Description, Ratio, BgRatio, pvalue)]
                    setorder(data, pvalue)

                    dt <- DT::datatable(
                        data,
                        rownames = FALSE,
                        colnames = c(
                            "Disease",
                            "Query ratio",
                            "Total ratio",
                            "p-value"
                        ),
                        style = "bootstrap",
                        options = list(
                            pageLength = 25
                        )
                    )

                    dt <- DT::formatRound(dt, "pvalue", digits = 4)

                    dt
                }
            )
        })
    }
}
