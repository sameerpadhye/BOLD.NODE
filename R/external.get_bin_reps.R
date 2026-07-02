#' Select representative records by BIN
#'
#' @description
#' Obtain one or more representative record(s) from each BIN in search results or
#' a BCDM data frame. BIN representatives can also be selected for each unique BIN-taxon
#' combination. The provided selection criteria are applied in sequence to select
#' representatives, thus criteria should be given in order of priority. If multiple
#' records are tied after all criteria have been applied, the tie can be broken
#' randomly (the default) or deterministically by setting a random seed, making it
#' possible to obtain reproducible results for the same input values.
#'
#' @details
#' The provided `bold.search.res` input can be a search result object from
#' \code{\link{bold_parquet_search}} or a BCDM data frame. Alternatively, it can be
#' any data frame or data table minimally containing `bin_uri`, unique record identifiers
#' (e.g. `processid` or `sampleid`), taxonomic identifications for all available records,
#' and any fields relevant to the provided selection `criteria` (see below).
#'
#' Selection `criteria` must be listed in order of priority. Available criteria
#' include the following:
#'   * `vouchered`: If `TRUE`, prioritize records with known voucher
#'    repositories over those mined from databases like GenBank. Setting this to
#'    `FALSE` will prioritize records *without* vouchers. To exclude this
#'    criterion, simply omit it. Corresponding BCDM field: `inst`.
#'   * `seq_length`: Can be used either to specify target barcode sequence
#'    length as an integer, to preferentially select longer or shorter sequences
#'    ("longest", "shortest"), or to select sequences that match the modal* barcode
#'    length for each BIN ("COI_auto"). If a target length is provided, sequences
#'    closest to that target are prioritized. *Modal barcode length ("COI_auto"
#'    option) is determined after first rounding sequence lengths to the nearest
#'    full codon; in the event of multiple modes, the one closest to 658bp is
#'    chosen. Corresponding BCDM field: `nuc_basecount`.
#'   * `id_method`: A character vector listing preferred identification methods
#'    in order of priority. The function definition lists all available values per
#'    the BCDM specification. Corresponding BCDM field: `identification_method`.
#'   * `inst`: A character vector listing preferred voucher specimen repositories
#'    in order of priority. Values not present in the input data are ignored.
#'    Corresponding BCDM field: `inst`.
#'   * `coll_date`: Prioritize recently collected specimens ("latest") or
#'    those collected longest ago ("oldest"). Records without dates are selected
#'    last in either case. Corresponding BCDM field: `collection_date_start`.
#'   * `seq_date`: Prioritize recently uploaded sequences ("latest") or
#'    those with the earliest upload date ("oldest"). Corresponding BCDM field:
#'    `sequence_upload_date`.
#'
#' Default selection criteria are as follows:
#' ```R
#' criteria = list(vouchered = TRUE,
#'                 seq_length = "COI_auto",
#'                 id_method = c("Morphology", "Morphology and sequence based",
#'                               "Image based", "Image and sequence based",
#'                               "Tree based", "BIN based", "BOLD ID Engine",
#'                               "Other sequence based approach", "Other"),
#'                 inst = "Centre for Biodiversity Genomics",
#'                 coll_date = "latest",
#'                 seq_date = "latest")
#' ```
#'
#' **Note**: The same data provided either as a `tbl_sql` object or a data frame
#' may yield different representative records, even with the same random seed.
#' This is because input goes through one of two paths depending on format, each
#' with different default tie-breaking behaviours.
#'
#' @param bold.search.res A `tbl_sql` object obtained from \code{\link{bold_parquet_search}} or a data frame or data table in BCDM format.
#' @param Nreps Integer indicating the maximum number of representatives to select for each BIN (or BIN-taxon combination).
#' @param by.taxon Logical value indicating whether to select representatives for each unique combination of BIN and taxonomic identification. If `TRUE`, the additional parameters `non_redundant_taxa` and `enforce_scientific` are also applied.
#' @param non.redundant.taxa Logical value indicating whether to select representatives at the lowest available rank from each distinct taxonomic lineage. For example, in a BIN with the identifications "Apidae" and "Bombus impatiens", only records assigned to "Bombus impatiens" will be selected. Ignored if `by.taxon` is `FALSE`.
#' @param enforce.scientific Logical value indicating whether to ignore representatives with non-scientific, provisional names. Ignored if `by.taxon` is `FALSE`.
#' @param criteria Named list of selection criteria to apply when sampling representatives, given in priority order. See details for more information and default values.
#' @param seed Optional positive integer to use as a random seed for reproducible tie-breaking. If `NULL` (the default), ties are broken randomly and selected records may differ between runs.
#'
#' @returns A data frame of selected representatives.
#'
#' @usage
#' get_bin_reps(
#'   bold.search.res,
#'   Nreps = 1,
#'   by.taxon = FALSE,
#'   non.redundant.taxa = FALSE,
#'   enforce.scientific = FALSE,
#'   criteria = list(vouchered = TRUE,
#'                   seq_length = c("COI_auto", "longest", "shortest", 658),
#'                   id_method = c("Morphology", "Morphology and sequence based",
#'                                 "Image based", "Image and sequence based",
#'                                 "Tree based", "BIN based", "BOLD ID Engine",
#'                                 "Other sequence based approach", "Other"),
#'                  inst = "Centre for Biodiversity Genomics",
#'                  coll_date = c("latest", "oldest"),
#'                  seq_date = c("latest", "oldest")),
#'   seed = NULL
#' )
#'
#' @examples
#' \dontrun{
#'
#' # Search BOLD data package
#' bold_search <- bold_parquet_search(
#'   input.parquet = parquet_file,
#'   taxonomy.names = "Araneae",
#'   geography.names = "Canada"
#' )
#'
#' # Select three representatives per BIN from the searched data,
#' # prioritizing those with a morphological ID and with vouchers
#' # deposited at the CBG (e.g. in case vouchers need to be examined)
#' bin_reps <- get_bin_reps(
#'   bold.search.res = bold_search,
#'   Nreps = 3,
#'   criteria = list(inst = "Centre for Biodiversity Genomics",
#'                   id_method = c("Morphology",
#'                                 "Morphology and sequence based"))
#' )
#'
#' # Select one representative for each combination of BIN and taxonomic
#' # lineage (scientific names only), with preference for 658-bp barcodes
#' # (e.g. for building a sequence tree of all known taxa)
#' bin_tax_reps <- get_bin_reps(
#'   bold.search.res = bold_search,
#'   Nreps = 1,
#'   by.taxon = TRUE,
#'   non.redundant.taxa = TRUE,
#'   enforce.scientific = TRUE,
#'   criteria = list(seq_length = 658)
#' )
#' }
#'
#' @export
get_bin_reps <- function(
    bold.search.res,
    Nreps = 1,
    by.taxon = FALSE,
    non.redundant.taxa = FALSE,
    enforce.scientific = FALSE,
    criteria = list(vouchered = TRUE,
                    seq_length = c("COI_auto", "longest", "shortest", 658),
                    id_method = c("Morphology", "Morphology and sequence based",
                                  "Image based", "Image and sequence based",
                                  "Tree based", "BIN based", "BOLD ID Engine",
                                  "Other sequence based approach", "Other"),
                    inst = "Centre for Biodiversity Genomics",
                    coll_date = c("latest", "oldest"),
                    seq_date = c("latest", "oldest")),
    seed = NULL
) {
  # Check input format
  is_tbl_sql <- isTRUE(try(check.tbl.sql(bold.search.res), silent = TRUE))
  if(!is_tbl_sql && !is.data.frame(bold.search.res)) stop("`bold.search.res` must be either a bold_parquet_search output (tbl_sql / dbplyr table) or a data frame / data table.")
  # Check parameters
  stopifnot("'Nreps' must be a single numeric value." = is.numeric(Nreps)  && length(Nreps) == 1,
            "'by.taxon' must be a single logical value." = is.logical(by.taxon) && length(by.taxon) == 1,
            "'non.redundant.taxa' must be a single logical value." = is.logical(non.redundant.taxa) && length(non.redundant.taxa) == 1,
            "'enforce.scientific' must be a single logical value." = is.logical(enforce.scientific) && length(enforce.scientific) == 1)
  if(!missing(criteria)) {
    if(length(criteria$seq_length) > 1) {
      stop("'seq_length' criterion must be a single value.")
    } else if(length(criteria$seq_length) == 1) {
      if(!any((criteria$seq_length %in% c("COI_auto", "longest", "shortest")),
              is.numeric(criteria$seq_length))) {
        stop("'seq_length' criterion must be one of 'COI_auto', 'longest', 'shortest', or a numeric value.")
      }
    }
    stopifnot("'vouchered' criterion must be a single logical value." =
                length(criteria$vouchered) == 0 || is.logical(criteria$vouchered) && length(criteria$vouchered) == 1)
    if(length(criteria$coll_date) > 1) stop("'coll_date' criterion must be one of 'latest' or 'oldest'.")
    if(length(criteria$seq_date) > 1) stop("'seq_date' criterion must be one of 'latest' or 'oldest'.")
  } else {
    # Supply default criteria if param is omitted
    criteria = list(vouchered = TRUE,
                    seq_length = "COI_auto",
                    id_method = c("Morphology", "Morphology and sequence based",
                                  "Image based", "Image and sequence based",
                                  "Tree based", "BIN based", "BOLD ID Engine",
                                  "Other sequence based approach", "Other"),
                    inst = "Centre for Biodiversity Genomics",
                    coll_date = "latest",
                    seq_date = "latest")
  }
  # Default BOLD ranks
  ranks <- c("kingdom", "phylum", "class", "order", "family", "subfamily", "tribe", "genus", "species", "subspecies")
  # Define grouping columns
  select_by <- if(by.taxon) {
    c("bin_uri", "identification")
  } else {
    "bin_uri"
  }
  # Proceed according to input object type
  bin_reps <- if(is_tbl_sql) {
    # Shuffle data to randomize representative order, or set pre-determined order using random seed
    df_shuffle <-if (!is.null(seed)) {
      id_col <- intersect(c("processid", "sampleid", "fieldid", "museumid", "record_id", "specimenid"),
                          colnames(bold.search.res))[[1]]
      bold.search.res %>% dplyr::mutate(.rand = md5(paste0(!!sym(id_col), !!as.character(seed))))
    } else {
      bold.search.res %>% dplyr::mutate(.rand = random())
    }
    # Build sequence of sort keys to use in query
    sort_sequence <- get_sql_sort(df_shuffle, criteria)
    # Assign temporary column names for sorting
    temp_names <- paste0(".sort_", names(criteria))
    # Assemble expressions for temporary sort columns
    sort_exprs <- setNames(
      lapply(sort_sequence, function(step) step$key),
      temp_names
    )
    # Assemble sequential sort expressions
    arrange_exprs <- c(
      lapply(seq_along(temp_names), function(i) {
        nm <- sym(temp_names[[i]])
        if (isTRUE(sort_sequence[[i]]$desc)) dplyr::expr(desc(!!nm)) else nm
      }),
      list(sym(".rand"))
    )
    # Apply any preliminary transformations (e.g. get modal sequence length per BIN)
    df_prepped <- Reduce(function(df, step) {
      if (!is.null(step$prep)) step$prep(df) else df
    }, sort_sequence, init = df_shuffle)
    # Apply query expressions and collect representatives
    df_reps <- df_prepped %>%
      dplyr::filter(!is.na(bin_uri)) %>%
      dplyr::mutate(!!!sort_exprs) %>%
      dplyr::group_by(across(all_of(select_by))) %>%
      dbplyr::window_order(!!!arrange_exprs) %>%
      dplyr::mutate(.row_rank = dplyr::row_number()) %>%
      dplyr::filter(.row_rank <= round(Nreps, 0)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(!!!arrange_exprs) %>%
      dplyr::collect() %>%
      dplyr::select(-all_of(temp_names), -.row_rank, -.rand, -any_of(c(".bin_mode")))
    # For simplicity when by.taxon == TRUE all unique combinations of `bin_uri` and `identification` are collected
    # If necessary they are filtered further according to supplied parameters
    if(by.taxon) {
      if(enforce.scientific) {
        df_reps <- df_reps %>% dplyr::filter(!grepl(re_int, identification, perl = T))
      }
      if(non.redundant.taxa) {
        # Function to find lowest non-redundant identification for each BIN
        filter_ids <- function(df, ...) {
          ranks_to_check <- ranks[sapply(ranks, function(rank) !all(is.na(df[[rank]]) | df[[rank]] == ""))]
          bin_df <- df %>%
            dplyr::distinct(identification, identification_rank, across(all_of(ranks_to_check)))
          bin_df <- bin_df %>%
            dplyr::mutate(count = mapply(
              function(rank, id) sum(bin_df[[rank]] == id, na.rm = TRUE),
              bin_df[["identification_rank"]], bin_df[["identification"]]
            ))
          unique_ids <- bin_df %>%
            dplyr::filter(count == 1) %>%
            dplyr::pull("identification") %>%
            as.character()
          df %>% dplyr::filter(identification %in% unique_ids)
        }
        col_order <- names(df_reps)
        df_reps <- df_reps %>%
          dplyr::group_by(bin_uri) %>%
          dplyr::group_modify(filter_ids) %>%
          dplyr::ungroup() %>%
          dplyr::relocate(all_of(col_order))
      }
    }
    df_reps
  } else { # If local data is supplied, select BIN reps from there
    # Randomize representative order (using random seed for pre-determined order if provided)
    if(!is.null(seed)) set.seed(seed)
    data <- as.data.table(bold.search.res)[sample(nrow(bold.search.res))][!is.na(bin_uri) & (bin_uri != "")]
    # Filter by identification if applicable
    if(by.taxon) {
      if(enforce.scientific) {
        data <- data[!grepl(re_int, identification, perl = TRUE)]
      }
      if(non.redundant.taxa) {
        # This expression finds and applies the indices of records with the lowest non-redundant identification in each BIN
        data <- data[data[, .I[({
          cols_to_use <- c("identification", "identification_rank", ranks[sapply(ranks, function(rank) !all(is.na(.SD[[rank]]) | .SD[[rank]] == ""))])
          bin_taxa <- unique(.SD[, .SD, .SDcols = cols_to_use])
          bin_taxa[, "id_count" := mapply(function(rank, id) bin_taxa[bin_taxa[[rank]] == id, .N], as.character(identification_rank), as.character(identification))]
          unique_lineages <- as.character(bin_taxa[bin_taxa[["id_count"]] == 1][["identification"]])
          as.character(.SD[["identification"]]) %in% unique_lineages
        })], by = "bin_uri"]$V1]
      }
    }
    # Build sequence of sort keys to apply to data table
    sort_sequence <- get_dt_sort(data, criteria)
    # Apply sort keys in sequence to select and return representatives
    data[data[do.call("order", sort_sequence), .I[seq_len(min(round(Nreps, 0), .N))], by = select_by]$V1, ]
  }
  return(as.data.frame(bin_reps))
}
