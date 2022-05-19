#' Create a comparison editor.
#'
#' @param options Global application options
#' @noRd
comparison_editor_ui <- function(id, options) {
    verticalLayout(
        h3("Comparison"),
        selectInput(
            NS(id, "comparison_genes"),
            "Comparison genes",
            choices = c(
                "None",
                "Random genes",
                names(options$gene_sets),
                "Customize"
            )
        ),
        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Customize'",
                NS(id, "comparison_genes")
            ),
            gene_selector_ui(NS(id, "custom_genes"))
        )
    )
}

#' Create a server for the comparison editor.
#'
#' @param id ID for namespacing the inputs and outputs.
#' @param preset A reactive containing the current preset.
#' @param options Global application options
#'
#' @return A reactive containing the comparison gene IDs.
#'
#' @noRd
comparison_editor_server <- function(id, preset, options) {
    moduleServer(id, function(input, output, session) {
        custom_gene_ids <- gene_selector_server("custom_genes")

        reactive({
            if (input$comparison_genes == "None") {
                NULL
            } else if (input$comparison_genes == "Random genes") {
                preset <- preset()
                gene_pool <- preset$gene_ids
                reference_gene_ids <- preset$reference_gene_ids
                gene_pool <- gene_pool[!gene_pool %chin% reference_gene_ids]
                gene_pool[sample(length(gene_pool), length(reference_gene_ids))]
            } else if (input$comparison_genes == "Customize") {
                custom_gene_ids()
            } else {
                options$gene_sets[[input$comparison_genes]]
            }
        })
    })
}
