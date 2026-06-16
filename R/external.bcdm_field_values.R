#' Extract unique values of different BCDM fields from the BOLD dataset
#'
#' @description Extracts distinct values of specified field/s in the BOLD parquet data.
#'
#' @details This function extracts unique values from one or more specified columns in BOLD parquet data. It handles both the parquet file and `tbl_sql` objects from `bold_parquet_search` as input. The results can be saved to disk as an RDS file for later use.
#'
#' @param input.data Path to the input parquet file or the `bold_parquet_search` result
#' @param specific.cols Name of the column to extract unique values from
#' @param save.data Logical value indicating whether to save the results to disk as a .rds file (default: FALSE)
#' @param output.file Path (without extension) for saving results as .rds file (required if save.data = TRUE)
#'
#' @return A list containing unique values from the specified column; if `save.data` = T, a `.rds` file exported locally
#'
#' @importFrom dplyr filter distinct collect %>%
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#'
#'
#' # Search the BOLD data
#' bold_search <- bold_parquet_search(
#'   input.parquet = parquet_file,
#'   taxonomy = "Diptera",
#'   geography = "India",
#'   marker = "COI-5P"
#' )
#'
#' # Get the field values
#'
#' vocab.data <- bcdm_field_values(bold_search, bold_search, specific.cols = c("inst", "identified.by"))
#' }
#' @export
#'
#'
bcdm_field_values <- function(
  input.data,
  specific.cols,
  save.data = FALSE,
  output.file = NULL
) {
  # Allow both parquet path OR tbl_sql input
  if (inherits(input.data, "tbl_sql")) {
    bold_parquet_data <- input.data
  } else {
    bold_parquet_data <- import_parquet_data(input.data)
  }

  # Get unique values per column separately
  terms_list <- lapply(specific.cols, function(col) {
    bold_parquet_data %>%
      dplyr::filter(!is.na(.data[[col]]) & .data[[col]] != "") %>%
      dplyr::distinct(.data[[col]]) %>%
      dplyr::collect() %>%
      dplyr::pull(.data[[col]])
  })

  names(terms_list) <- specific.cols

  if (save.data) {
    if (is.null(output.file)) {
      stop("output.file must be provided when save.data = TRUE")
    }

    saveRDS(terms_list, paste0(output.file, ".rds"))
  }

  return(terms_list)
}
