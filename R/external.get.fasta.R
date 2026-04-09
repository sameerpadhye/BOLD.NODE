#' Export BOLD search results to FASTA format
#'
#' @description Exports nucleotide sequences from BOLD search results to a FASTA file with customizable headers.
#'
#' @details This function transforms the search results from `bold.data.search` into a FASTA file. Data chunking  option is available to manage large sizes to avoid memory issues. The `fas.header` argument lets users create custom headers using the BCDM column names.
#'
#' @param bold.search.res A tbl_sql object containing BOLD search results
#' @param output.file Path to the output FASTA file
#' @param fas.header Character vector of field names to include in the FASTA header (separated by "|")
#' @param chunk.size Number of records to process in each chunk (default: 1e6)
#'
#' @return writes a FASTA file to disk with custom headers as specified by the user
#'
#' @importFrom dplyr select filter mutate compute collect
#' @importFrom dbplyr sql
#' @importFrom DBI dbQuoteIdentifier
#'
#' @examples
#' \dontrun{
#'
#'
#' # Search the BOLD data
#' bold_search <- bold.data.search(
#' parquet_path=parquet_file,
#' taxonomy = "Coleoptera",
#' geography = "Canada",
#' marker = "COI-5P",
#' basecount = c(500, 660)
#' )
#'
#' # Get a fasta file
#' get.fasta(
#' bold_search,
#' output.file = "trial2.fas",
#' fas.header = c("bin_uri", "processid")
#')
#'
#'
#' }
#'
#' @export
get.fasta <- function(bold.search.res,
                      output.file,
                      fas.header,
                      chunk.size = 1e6)
{

  # Check if input is a tbl_sql (helper function you already have)
  check.tbl.sql(bold.search.res)

  # Ensure user-specified fields exist
  user_specified_fields <- fas.header

  quoted_fields <- DBI::dbQuoteIdentifier(
    dbplyr::remote_con(bold.search.res),
    user_specified_fields
  )

  fas_headers <- paste(quoted_fields, collapse = ", '|' ,")

  # Prepare sequence data
  seq.data <- bold.search.res %>%
    dplyr::select(
      nuc = matches("^nuc$", ignore.case = TRUE),
      dplyr::all_of(user_specified_fields)
    ) %>%
    dplyr::filter(!is.na(nuc) & nuc != "") %>%
    dplyr::mutate(
      # SQL-safe FASTA header
      seq.name = sql(paste0("concat('>', ", fas_headers, ")")),
      # Row number for chunking
      row_num  = sql("row_number() over (order by nuc)")
    ) %>%
    dplyr::compute()  # temporary table, safe for repeated runs

  # Count total rows
  total_rows <- seq.data %>% summarise(n = sql("count(*)")) %>% pull(n)

  # Determine chunk indices
  num_chunks <- ceiling(total_rows / chunk.size)

  # Open FASTA file
  con <- file(output.file, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  # Loop through chunks
  for (i in seq_len(num_chunks)) {
    start <- (i - 1) * chunk.size + 1
    end   <- min(i * chunk.size, total_rows)

    chunk <- seq.data %>%
      dplyr::filter(row_num >= start & row_num <= end) %>%
      dplyr::select(seq.name, nuc) %>%
      dplyr::collect()

    # Write FASTA: alternating header and sequence
    writeLines(as.vector(rbind(chunk$seq.name, chunk$nuc)), con)
  }

  message("FASTA file written to: ", output.file)
}
