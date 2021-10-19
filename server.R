library(data.table)
library(DT)
library(geposan)
library(gprofiler2)
library(plotly)
library(rclipboard)
library(shiny)

source("rank_plot.R")
source("scatter_plot.R")
source("utils.R")

#' Java script function to replace gene IDs with Ensembl gene links.
js_link <- JS("function(row, data) {
    let id = data[1];
    var name = data[2];
    if (!name) name = 'Unknown';
    let url = `https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=${id}`;
    $('td:eq(1)', row).html(`<a href=\"${url}\" target=\"_blank\">${name}</a>`);
}")

server <- function(input, output, session) {
    #' Show the customized slider for setting the required number of species.
    output$n_species_slider <- renderUI({
        sliderInput(
            "n_species",
            "Required number of species per gene",
            min = 0,
            max = if (input$species == "all") {
                nrow(species)
            } else {
                length(species_ids_replicative)
            },
            step = 1,
            value = 10
        )
    })

    observeEvent(input$optimize_button, {
        results <- isolate(results())
        method_ids <- NULL

        for (method in methods) {
            if (isolate(input[[method$id]])) {
                method_ids <- c(method_ids, method$id)
            }
        }

        weights <- geposan::optimize_weights(
            results,
            method_ids,
            genes_tpe_old
        )

        for (method_id in method_ids) {
            updateSliderInput(
                session,
                sprintf("%s_weight", method_id),
                value = weights[[method_id]] * 100
            )
        }
    })

    # Observe each method's enable button.
    lapply(methods, function(method) {
        observeEvent(input[[method$id]], {
            shinyjs::toggleState(sprintf("%s_weight", method$id))
        }, ignoreInit = TRUE)
    })

    #' Rank the results based on the specified weights. Filter out genes with
    #' too few species but don't apply the cut-off score.
    results <- reactive({
        # Select the preset.
        preset <- if (input$species == "all") {
            preset_all_species
        } else {
            preset_replicative_species
        }

        # Perform the analysis cached based on the preset's hash.
        results <- run_cached(rlang::hash(preset), geposan::analyze, preset)

        # Add all gene information to the results.
        results <- merge(
            results,
            genes,
            by.x = "gene",
            by.y = "id"
        )

        # Exclude genes with too few species.
        results <- results[n_species >= input$n_species]

        # Rank the results based on the weights.

        weights <- NULL

        for (method in methods) {
            if (input[[method$id]]) {
                weight <- input[[sprintf("%s_weight", method$id)]]
                weights[[method$id]] <- weight
            }
        }

        geposan::ranking(results, weights)
    })

    #' Apply the cut-off score to the ranked results.
    results_filtered <- reactive({
        results()[score >= input$cutoff / 100]
    })

    output$genes <- renderDT({
        method_ids <- sapply(methods, function(method) method$id)
        method_names <- sapply(methods, function(method) method$name)
        columns <- c("rank", "gene", "name", "chromosome", method_ids, "score")
        column_names <- c("", "Gene", "", "Chromosome", method_names, "Score")

        dt <- datatable(
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

        formatPercentage(dt, c(method_ids, "score"), digits = 1)
    })

    output$copy <- renderUI({
        results <- results_filtered()

        gene_ids <- results[, gene]
        names <- results[name != "", name]

        genes_text <- paste(gene_ids, collapse = "\n")
        names_text <- paste(names, collapse = "\n")

        splitLayout(
            cellWidths = "auto",
            rclipButton(
                "copy_ids_button",
                "Copy gene IDs",
                genes_text,
                icon = icon("clipboard")
            ),
            rclipButton(
                "copy_names_button",
                "Copy gene names",
                names_text,
                icon = icon("clipboard")
            )
        )
    })

    output$scatter <- renderPlotly({
        results <- results_filtered()

        gene_ids <- results[input$genes_rows_selected, gene]
        genes <- genes[id %chin% gene_ids]

        species <- if (input$species == "all") {
            species
        } else {
            species[replicative == TRUE]
        }

        scatter_plot(results, species, genes, distances)
    })

    output$assessment_synopsis <- renderText({
        reference_gene_ids <- genes[suggested | verified == TRUE, id]

        reference_count <- results_filtered()[
            gene %chin% reference_gene_ids,
            .N
        ]

        reference_results <- results()[gene %chin% reference_gene_ids]

        sprintf(
            "Included reference genes: %i/%i<br> \
            Mean rank of reference genes: %.1f<br> \
            Maximum rank of reference genes: %i",
            reference_count,
            length(reference_gene_ids),
            reference_results[, mean(rank)],
            reference_results[, max(rank)]
        )
    })

    output$rank_plot <- renderPlotly({
        rank_plot(
            results(),
            genes[suggested | verified == TRUE, id],
            input$cutoff / 100
        )
    })

    output$gost <- renderPlotly({
        if (input$enable_gost) {
            result <- gost(results_filtered()[, gene], ordered_query = TRUE)
            gostplot(result, capped = FALSE, interactive = TRUE)
        } else {
            NULL
        }
    })
}
