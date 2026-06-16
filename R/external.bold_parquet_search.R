#' Search the BOLD public data packages available in parquet format
#'
#' @description Search the BOLD public data packages available in the parquet format based on various search criteria including taxonomy, geography, institutes etc.
#'
#' @details This function loads the BOLD public data packages parquet files (<https://boldsystems.org/data/data-packages/>) via duckDB and applies filters based on the provided parameters. It supports filtering by ids (sampleid, processid), taxonomy (from kingdom to species level), geography (country to site level), biogeography (biome to ecoregion level), BINs, institutes, identifiers, sequence sources, genetic markers, nucleotide base counts,dataset or projects, spatial bounding boxes, and ambiguous base percent cutoffs. Users can also specify particular columns to return using the `specific.cols` parameter (column names can be checked using the `bcdm_field_names` function). The `tbl_sql` object can then be used by any of the `bcdm_to_*` functions for data transformations or `bold_search_collect` to load all the data in memory.
#'
#' @param input.parquet Path to the input parquet file
#' @param ids Vector of process IDs or sample IDs to filter by
#' @param bins Vector of BIN numbers (i.e. URIs) to filter by
#' @param taxonomy Vector of taxonomic names to filter by (can include kingdom, phylum, class, order, family, subfamily, genus, species)
#' @param geography Vector of geographic locations to filter by (can include country.ocean, province.state, region, sector, site)
#' @param institutes Vector of institute codes to filter by
#' @param identified.by Vector of identifiers to filter by
#' @param seq.source Vector of sequence run sites to filter by
#' @param marker Vector of marker codes to filter by
#' @param basecount Nucleotide base count filter - either a single value or a vector of two values for a range
#' @param biogeo.cat Vector of biogeographic/ecological categories to filter by (biome, realm, or ecoregion)
#' @param dataset.projects Vector of dataset/project codes to filter by
#' @param bounding.box Numeric vector of length 4: c(min_lon, max_lon, min_lat, max_lat)
#' @param ambi.base.cutoff Character value for filtering data based proportion of ambiguous bases (IUPAC codes).Three values currently available ("<1%", "1-5%", or ">5%"); Default value is NULL
#' @param specific.cols Optional character vector of specific columns to return
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
#' # Search the BOLD data
#'
#' # Taxonomy
#' bold_search <- bold_parquet_search(
#'   input.parquet = parquet_file,
#'   taxonomy = c("Odonata", "Poecilia")
#' )
#'
#' # Geography
#' bold_search <- bold_parquet_search(
#'   input.parquet = parquet_file,
#'   geography = "Canada"
#' )
#'
#' # Combination of many search criteria
#' bold_search <- bold_parquet_search(
#'   input.parquet = parquet_file,
#'   taxonomy = "Coleoptera",
#'   geography = "Canada",
#'   marker = "COI-5P",
#'   basecount = c(500, 660)
#' )
#' }
#'
#' @export
bold_parquet_search <- function(input.parquet,
                                ids = NULL,
                                bins = NULL,
                                taxonomy = NULL,
                                geography = NULL,
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
  # Condition to check the file extension

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
    taxonomy = taxonomy,
    geography = geography,
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

  # Null arguments

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


  if (!is.null(specific.cols)) {
    bcdm_fields <- bcdm_field_names()

    if (any(!specific.cols %in% bcdm_fields$field)) stop("Please re-check the column names")

    result <- result %>%
      dplyr::select(all_of(specific.cols))

    result
  } else {
    result
  }


  tot_records <- result %>%
    summarise(Total_records = n()) %>%
    collect()

  message(paste("The search has", tot_records, "records in the dataset", sep = " "))


  return(invisible(result))
}
