#' Extract occurrence data from BOLD search results
#'
#' @description Extracts occurrence data (specimen counts by taxon and location) from BOLD search results.
#'
#' @details This function transforms the search results from `bold.data.search` into occurrence data matrices used commonly in biodiversity and ecological analyses by packages like `vegan` and `betapart`. `kingdom` argument specifies the taxa kingdom with the default value being `Animalia`. The occurrences differ based on the kingdom. For `Animalia`, only records having BINs are counted (i.e. records without BINs are removed before calculations), while for other kingdoms, all records having a sequence are counted (i.e. records without sequences are removed before calculations). It supports aggregation at different taxonomic ranks (from kingdom to species) for a single or multiple taxa (also applicable for bin_uri; the difference being that the numbers in each cell would be the number of times that respective BIN is found at a particular `site.cat`), optional filtering by specific taxon names. `site.cat` can be any of the `geography` fields (Meta data on fields can be checked using the `bold.bcdm.fields` function). In case where `site.cat` = NULL, the column `coord` will be used as default. The function can convert count data to presence/absence (1/0) format. In case `specific.cols` is used in the `bold.data.search` function to obtain only few columns, occurrence matrix wont be generated.
#'
#'
#' @param bold.search.res A `tbl_sql` object containing BOLD search results
#' @param kingdom Character value specifying the kingdom (default: Animalia)
#' @param taxon.rank Taxonomic rank to aggregate by (kingdom, phylum, class, order, family, genus, or species)
#' @param taxon.name Optional vector of specific taxon names to include
#' @param site.cat Optional categorical variable to group occurrence data by (e.g., region)
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
#'   kingdom = 'Animalia',
#'   taxon.rank = "family",
#'   site.cat = "region"
#'   )
#'
#' }
#' @export
get.occ.data <- function(bold.search.res,
                         kingdom="Animalia",
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
                species = "species",
                bin_uri = 'bin_uri')

  taxon.rank <- rank_map[[tolower(taxon.rank)]]


  if (is.null(taxon.rank)) {
    stop("Invalid taxonomic rank supplied.")
  }

  cols_to_select <- Filter(Negate(is.null),
                           site.cat)


  if(kingdom=='Animalia')

  {

    occ.data <- bold.search.res %>%
      dplyr::select(
        bin_uri = matches("bin_uri$", ignore.case = TRUE),
        coord   = matches("coord$", ignore.case = TRUE),
        taxon   = !!rlang::sym(taxon.rank),
        dplyr::all_of(cols_to_select)) %>%
      dplyr::filter(!is.na(bin_uri),bin_uri!='')%>%
      dplyr::filter(!is.na(taxon))



  } else if (kingdom%in%c('Plantae','Protista','Fungi','Mixture', 'Bacteria'))

  {

    occ.data <- bold.search.res %>%
      dplyr::select(
        nuc = matches("nuc$", ignore.case = TRUE),
        coord   = matches("coord$", ignore.case = TRUE),
        taxon   = !!rlang::sym(taxon.rank),
        dplyr::all_of(cols_to_select)) %>%
      dplyr::filter(!is.na(nuc),nuc!='')%>%
      dplyr::filter(!is.na(taxon))

  }


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
        name = "count"
      )

  } else {

    occ.data <- occ.data %>%
      dplyr::filter(!is.na(coord)) %>%
      dplyr::count(
        coord,
        taxon,
        name = "count"
      )

  }


  # Collect
  occ.data <- occ.data %>%
    dplyr::collect() %>%
    tidyr::pivot_wider(
      names_from  = taxon,
      values_from = count,
      values_fill = 0
    )

  return(occ.data)


}
