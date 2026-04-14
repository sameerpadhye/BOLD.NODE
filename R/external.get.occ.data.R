#' Extract occurrence data from BOLD search results
#'
#' @description Extracts occurrence data (specimen counts by taxon and location) from BOLD search results.
#'
#' @details This function transforms the search results from `bold.data.search` into occurrence data matrices used commonly in biodiversity and ecological analyses. It supports aggregation at different taxonomic ranks (from kingdom to species), optional filtering by specific taxon names, and grouping by site categories or geographic coordinates. The function can convert count data to presence/absence (1/0) format.
#'
#' @param bold.search.res A tbl_sql object containing BOLD search results
#' @param taxon.rank Taxonomic rank to aggregate by (kingdom, phylum, class, order, family, genus, or species)
#' @param taxon.name Optional vector of specific taxon names to include
#' @param site.cat Optional categorical variable to group occurrence data by (e.g., habitat type)
#' @param pre.abs Logical indicating whether to convert counts to presence/absence (1/0) data (default: FALSE)
#'
#' @return A data frame with occurrence data (taxon names as columns, site categories or coordinates as rows)
#'
#' @importFrom dplyr select filter mutate collect count
#' @importFrom rlang sym
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#'
#'
#' # Search the BOLD data
#' bold_search <- bold.data.search(
#' input.parquet=parquet_file,
#' taxonomy = "Odonata",
#' geography = "Thailand")
#'
#' # Get the occurrence matrix
#'
#' occurrence_data <- get.occ.data(
#'   bold_search,
#'   taxon.rank = "family",
#'   site.cat = "region"
#'   )
#'
#' }
#' @export
get.occ.data <- function(bold.search.res,
                         taxon.rank,
                         taxon.name = NULL,
                         site.cat = NULL,
                         pre.abs = FALSE)
{

  check.tbl.sql(bold.search.res)

  rank_map <- c(kingdom = "kingdom",
    phylum = "phylum",
    class   = "class",
    order   = "order",
    family  = "family",
    genus   = "genus",
    species = "species")

  taxon.rank <- rank_map[[tolower(taxon.rank)]]

  if (is.null(taxon.rank)) {
    stop("Invalid taxonomic rank supplied.")
  }

  cols_to_select <- Filter(Negate(is.null),
                           site.cat)

  occ.data <- bold.search.res %>%
    dplyr::select(
      bin_uri = matches("bin_uri$", ignore.case = TRUE),
      coord   = matches("coord$", ignore.case = TRUE),
      taxon   = !!rlang::sym(taxon.rank),
      dplyr::all_of(cols_to_select)) %>%
    dplyr::filter(bin_uri != "None", !is.na(bin_uri))


  if (!is.null(taxon.name)) {

    occ.data <- occ.data %>%
      dplyr::filter(taxon %in% taxon.name)

  }


  if (!is.null(site.cat)) {

    occ.data <- occ.data %>%
      dplyr::filter(!is.na(.data[[site.cat]])) %>%
      dplyr::count(
        .data[[site.cat]],
        taxon,
        name = "bin_count"
      )

  } else {

    occ.data <- occ.data %>%
      dplyr::filter(!is.na(coord)) %>%
      dplyr::count(
        coord,
        taxon,
        name = "bin_count"
      )

  }


  # Collect AFTER aggregation (important for large datasets)
  occ.data <- occ.data %>%
    dplyr::collect() %>%
    tidyr::pivot_wider(
      names_from  = taxon,
      values_from = bin_count,
      values_fill = 0
    )

  occ.data
}
