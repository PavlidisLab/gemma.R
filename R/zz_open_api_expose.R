#' Full API wrapper
#'
#' Exposes the full Gemma API. Auto generated using
#' \href{https://openapi-generator.tech/}{OpenAPI generator}
#' Exposed to user to allow access unsupported endpoints
#' by the main package.
#' @seealso \code{\link{gemmaCall}}
#' @inheritSection DefaultApi Methods
#'
#' @export
#' @keywords misc
gemma_api <- list(
    get_result_set = function(result_set, data_file = NULL, ...) {
        fname <- "get_result_set"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_result_set_as_tsv = function(result_set_, data_file = NULL, ...) {
        fname <- "get_result_set_as_tsv"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_result_sets = function(datasets = NULL, database_entries = NULL, filter = "",
    offset = 0, limit = 20, sort = "+id", data_file = NULL, ...) {
        fname <- "get_result_sets"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    search_annotations = function(query, data_file = NULL, ...) {
        fname <- "search_annotations"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    search_datasets = function(query, filter = "", offset = 0, limit = 20, sort = "+id",
    data_file = NULL, ...) {
        fname <- "search_datasets"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    search_taxon_datasets = function(taxon, query, filter = "", offset = 0, limit = 20,
    sort = "+id", data_file = NULL, ...) {
        fname <- "search_taxon_datasets"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_dataset_annotations = function(dataset, data_file = NULL, ...) {
        fname <- "get_dataset_annotations"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_dataset_design = function(dataset, data_file = NULL, ...) {
        fname <- "get_dataset_design"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_dataset_differential_expression = function(datasets, diff_ex_set = NULL, threshold = 1, limit = 100,
    keep_non_specific = FALSE, consolidate = NULL, data_file = NULL,
    ...) {
        fname <- "get_dataset_differential_expression"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_dataset_differential_expression_analyses = function(dataset, offset = 0, limit = 20, data_file = NULL,
    ...) {
        fname <- "get_dataset_differential_expression_analyses"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_dataset_differential_expression_analyses_result_sets = function(dataset, ...) {
        fname <- "get_dataset_differential_expression_analyses_result_sets"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_dataset_expression = function(dataset, filter = FALSE, data_file = NULL, ...) {
        fname <- "get_dataset_expression"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_dataset_expression_for_genes = function(datasets, genes, keep_non_specific = FALSE, consolidate = NULL,
    data_file = NULL, ...) {
        fname <- "get_dataset_expression_for_genes"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_dataset_expression_pca = function(datasets, component = 1, limit = 100, keep_non_specific = FALSE,
    consolidate = NULL, data_file = NULL, ...) {
        fname <- "get_dataset_expression_pca"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_dataset_platforms = function(dataset, data_file = NULL, ...) {
        fname <- "get_dataset_platforms"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_dataset_raw_expression = function(dataset, data_file = NULL, ...) {
        fname <- "get_dataset_raw_expression"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_dataset_samples = function(dataset, data_file = NULL, ...) {
        fname <- "get_dataset_samples"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_dataset_svd = function(dataset, data_file = NULL, ...) {
        fname <- "get_dataset_svd"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_datasets = function(filter = "", offset = 0, limit = 20, sort = "+id",
    data_file = NULL, ...) {
        fname <- "get_datasets"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_datasets_by_ids = function(dataset, filter = "", offset = 0, limit = 20, sort = "+id",
    data_file = NULL, ...) {
        fname <- "get_datasets_by_ids"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_gene_gene_coexpression = function(gene, with = NULL, limit = 100, stringency = 1, data_file = NULL,
    ...) {
        fname <- "get_gene_gene_coexpression"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_gene_go_terms = function(gene, data_file = NULL, ...) {
        fname <- "get_gene_go_terms"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_gene_locations = function(gene, data_file = NULL, ...) {
        fname <- "get_gene_locations"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_gene_probes = function(gene, offset = 0, limit = 20, data_file = NULL, ...) {
        fname <- "get_gene_probes"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_genes = function(genes, data_file = NULL, ...) {
        fname <- "get_genes"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_platform_annotations = function(platform, data_file = NULL, ...) {
        fname <- "get_platform_annotations"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_platform_datasets = function(platform, offset = 0, limit = 20, data_file = NULL,
    ...) {
        fname <- "get_platform_datasets"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_platform_element = function(platform, probes, offset = 0, limit = 20, data_file = NULL,
    ...) {
        fname <- "get_platform_element"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_platform_element_genes = function(platform, probe, offset = 0, limit = 20, data_file = NULL,
    ...) {
        fname <- "get_platform_element_genes"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_platform_elements = function(platform, offset = 0, limit = 20, data_file = NULL,
    ...) {
        fname <- "get_platform_elements"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_platforms = function(filter = "", offset = 0, limit = 20, sort = "+id",
    data_file = NULL, ...) {
        fname <- "get_platforms"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_platforms_by_ids = function(platform, filter = "", offset = 0, limit = 20, sort = "+id",
    data_file = NULL, ...) {
        fname <- "get_platforms_by_ids"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_api_info = function(data_file = NULL, ...) {
        fname <- "get_api_info"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    search = function(query = NULL, taxon = NULL, platform = NULL, result_types = NULL,
    limit = 20, data_file = NULL, ...) {
        fname <- "search"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_gene_locations_in_taxon = function(taxon, gene, data_file = NULL, ...) {
        fname <- "get_gene_locations_in_taxon"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_taxa = function(data_file = NULL, ...) {
        fname <- "get_taxa"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_taxa_by_ids = function(taxa, data_file = NULL, ...) {
        fname <- "get_taxa_by_ids"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_taxon_datasets = function(taxon, filter = "", offset = 0, limit = 20, sort = "+id",
    data_file = NULL, ...) {
        fname <- "get_taxon_datasets"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_taxon_genes = function(taxon, gene, data_file = NULL, ...) {
        fname <- "get_taxon_genes"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    },
    get_taxon_genes_overlapping_chromosome = function(taxon, chromosome, strand = "+", start = NULL, size = NULL,
    data_file = NULL, ...) {
        fname <- "get_taxon_genes_overlapping_chromosome"
        api <- DefaultApi$new()
        api$api_client$base_path <- gsub("/$", "", gemmaPath())
        if (!is.null(getOption("gemma.username")) && !is.null(getOption("gemma.password"))) {
            api_instance$api_client$username <- getOption("gemma.username")
            api_instance$api_client$password <- getOption("gemma.password")
        }
        call <- match.call()
        call[[1]] <- api[[fname]]
        eval(call)
    }
)
