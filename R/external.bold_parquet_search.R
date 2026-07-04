#' Search the BOLD public data packages available in parquet format
#'
#' @description Search the BOLD public data packages available in the parquet format based on various search criteria including taxonomy, geography, institutes etc.
#'
#' @details This function loads the BOLD public data packages parquet files (<https://boldsystems.org/data/data-packages/>) via DuckDB and applies filters based on the provided parameters. It supports filtering by ids (sampleid, processid), taxonomy (using combination of taxonomy.kingdom and taxonomy.names), geography (using combination of geography.level and geography.names), biogeography (biome to ecoregion level), BINs, institutes, identifiers, sequence sources, genetic markers, nucleotide base counts, dataset or projects, spatial bounding boxes, and ambiguous base percent cutoffs. Taxonomy and geography have a two filter system where `taxonomy.kingdom` and `geography.level` are assigned default values `Animalia` at `any` respectively to get all animal records from all matching geographic terms. There are some cases with respect to geographic names where the same names are used at two different geographic levels, e.g., Azerbaijan is a country but there is a region named the same in Iran. Using `any` geography level will get records for both but if the `geography.level` is set to `country.ocean`, only country level records from Azerbaijan will be selected. A similar situation also exists for some taxonomic names where same names exist in both plants and animals (Ex., The genus name Iris exists for plants as well as animals). Users can also specify particular columns to return using the `specific.cols` parameter (column names can be checked using the `bcdm_field_names` function). The `tbl_sql` object can then be used by any of the `bcdm_to_*` functions for data transformations or `bold_search_collect` to load all the data in memory.
#'
#' @param input.parquet Path to the input parquet file.
#' @param ids Vector of process IDs or sample IDs to filter by.
#' @param bins Vector of BIN numbers (i.e., URIs) to filter by.
#' @param taxonomy.kingdom Character value specifying the kingdom (includes "all", "Animalia", "Plantae", "Protista", "Fungi", "Bacteria"). Default is "all".
#' @param taxonomy.names Vector of taxonomic names to filter by (includes "kingdom", "phylum", "class", "order", "family", "subfamily","genus", "species").
#' @param geography.level A single character value specifying the geographic hierarchy level to search within (includes "any", "country.ocean", "province.state", "region", "sector", "site"). Default is "any".
#' @param geography.names Vector of geographic locations to filter by based on the `geography.level`.
#' @param institutes Vector of institute codes to filter by.
#' @param identified.by Vector of identifiers to filter by.
#' @param seq.source Vector of sequence run sites to filter by.
#' @param marker Vector of marker codes to filter by.
#' @param basecount Nucleotide base count filter - either a single value or a vector of two values for a range.
#' @param biogeo.cat Vector of biogeographic/ecological categories to filter by (biome, realm, or ecoregion).
#' @param dataset.projects Vector of dataset/project codes to filter by.
#' @param bounding.box Numeric vector of length 4: c(min_lon, max_lon, min_lat, max_lat).
#' @param ambi.base.cutoff Character value for filtering data based proportion of ambiguous bases (IUPAC codes). Three values currently available ("<1%", "1-5%", or ">5%"). Default value is NULL.
#' @param specific.cols Optional character vector of specific columns to return.
#'
#' @return A `tbl_sql` object containing the filtered data. Total records in the search are printed on the console.
#'
#' @importFrom dplyr filter select
#' @importFrom tools file_ext
#' @importFrom rlang !!
#' @importFrom utils head
#'
#' @examples
#' \dontrun{
#'
#'
#' # Search the BOLD data package
#'
#' # Taxonomy
#' bold_search <- bold_parquet_search(
#'   input.parquet = parquet_file,
#'   taxonomy.names = c("Odonata", "Poecilia")
#' )
#'
#' # Geography
#' bold_search <- bold_parquet_search(
#'   input.parquet = parquet_file,
#'   geography.names = "Canada"
#' )
#'
#' # Combination of many search criteria
#' bold_search <- bold_parquet_search(
#'   input.parquet = parquet_file,
#'   taxonomy.names = "Coleoptera",
#'   geography.names = "Canada",
#'   marker = "COI-5P",
#'   basecount = c(500, 660)
#' )
#' }
#'
#' @export
bold_parquet_search <- function(input.parquet,
                                ids = NULL,
                                bins = NULL,
                                taxonomy.kingdom = 'Animalia',
                                taxonomy.names = NULL,
                                geography.level = 'any',
                                geography.names = NULL,
                                institutes = NULL,
                                identified.by = NULL,
                                seq.source = NULL,
                                marker = NULL,
                                basecount = NULL,
                                biogeo.cat = NULL,
                                dataset.projects = NULL,
                                bounding.box = NULL,
                                ambi.base.cutoff = NULL,
                                specific.cols = NULL) {
  # checking the data type
  if (tolower(tools::file_ext(input.parquet)) != "parquet") {
    stop("Error: Input file must be a parquet")
  }
  # Import the parquet data
  parquet_data <- tryCatch(
    import_parquet_data(input.parquet),
    error = function(e) {
      stop(
        "Error: Failed to import parquet file '", input.parquet, "'. ",
        "Details: ", conditionMessage(e)
      )
    }
  )
  # Empty parquet data check
  if (nrow(parquet_data %>% head(1) %>% collect()) == 0) stop("Error: Parquet data is empty")
  # mapping the fetch.filter arguments with the bold.search function arguments
  bold.search.args <- list(
    ids = ids,
    bins = bins,
    taxonomy.kingdom = taxonomy.kingdom,
    taxonomy.names = taxonomy.names,
    geography.level = geography.level,
    geography.names = geography.names,
    institutes = institutes,
    identified.by = identified.by,
    seq.source = seq.source,
    marker = marker,
    basecount = basecount,
    biogeo_cat = biogeo.cat,
    dataset.projects = dataset.projects,
    bounding.box = bounding.box,
    ambi.base.cutoff = ambi.base.cutoff
  )
  # Filter out NULL values and get their values
  null_args <- sapply(
    bold.search.args,
    is.null
  )
  # Selecting the non null arguments
  non_null_args <- bold.search.args[!null_args]
  # add the parquet data to the list
  non_null_args$bold.df <- parquet_data
  # Apply all the search filters on the non null arguments
  result <- do.call(
    bold.search.filters,
    non_null_args
  )
  # If specific columns are provided
  if (!is.null(specific.cols)) {
    bcdm_fields <- bcdm_field_names()
    # To check if the column names are BCDM
    if (any(!specific.cols %in% bcdm_fields$field)) stop("Please re-check the column names")
    result <- result %>%
      dplyr::select(all_of(specific.cols))
    result
  } else {
    result
  }
  # To provide the total records available in the search
  tot_records <- result %>%
    summarise(Total_records = n()) %>%
    collect()
  message(paste("The search has", tot_records, "records in the dataset", sep = " "))
  return(invisible(result))
}
