#' Collect and export BOLD NODE search results
#'
#' @description collects,outputs and exports the tbl_sql 'bold.data.search' results object, processing large datasets in user defined manageable chunks.
#'
#' @details This function collects (downloads it in the R session) the search results from `bold.data.search`. Data chunking and system sleep options are available to manage large sizes to avoid memory issues. The function also supports exporting the searches in either TSV or Parquet format. export=FALSE (default) returns the collected data in the R session only. The full output_path has to be provided (along with the intended file name and file extension) when export=TRUE.
#' \emph{Important Note}: Some data searches (e.g. all Diptera) once collected can get very large and overload the RAM capacity for some machines.
#' @param bold.search.res A `tbl_sql` object obtained from `bold.data.search`
#' @param chunk.size Maximum number of rows to process in each chunk. Default value is 1e6
#' @param sys.sleep Time to sleep between chunks in seconds. Default value is 0
#' @param export Logical value that allows user to export the output locally. Default value is FALSE
#' @param export.type Character string specifying the data type of the exported file (tsv or parquet)
#' @param output.path Character string specifying the local path for data export along with the file name and extension
#'
#' @return A data frame containing all collected results; if export = TRUE, either a TSV or parquet file exported locally
#'
#' @importFrom dplyr summarise collect pull bind_rows %>%
#' @importFrom DBI dbExecute
#' @importFrom dbplyr remote_con sql_render
#' @importFrom progressr with_progress progressor
#' @examples
#' \dontrun{
#'
#'
#' # Search the BOLD data
#' bold_search <- bold.data.search(
#' input.parquet=parquet_file,
#' taxonomy = "Coleoptera",
#' geography = "Canada",
#' marker = "COI-5P",
#' basecount = c(500, 660)
#' )
#'
#' # Collect the data  (no export)
#' bold.data.collect(
#' bold_search,
#' chunk.size = 50000,
#' export = FALSE)
#'
#' # Collect and export
#' bold.data.collect(
#' bold_search,
#' chunk.size = 50000,
#' export = TRUE,
#' export.type = "parquet",
#' output.path = 'userdefinedpath')
#'
#'}
#' @export

bold.data.collect <- function(
    bold.search.res,
    chunk.size = 1e6,
    sys.sleep = 0,
    export = FALSE,
    export.type = c("tsv", "parquet"),
    output.path = NULL)
  {
  # Match the type of file for export
  export.type <- match.arg(export.type)

  # check if the input is a 'tbl_sql' object
  check.tbl.sql(bold.search.res)

  # establish a temporary connection (for duckdb)
  con <- dbplyr::remote_con(bold.search.res)

  # parquet export (direct and doesnt need chunks)
  if (export && export.type == "parquet") {
    if (is.null(output.path)) {
      stop("Please provide output.path for parquet export")
    }

    query <- dbplyr::sql_render(bold.search.res)

    DBI::dbExecute(
      con,
      paste0("COPY (",query,") TO '",output.path,"'(FORMAT PARQUET, COMPRESSION ZSTD)"))

    message("Parquet export complete.")

    return(invisible(NULL))
  }

  # TSV export needs chunking of data for collection before export
  #1 Getting chunks

  # Disable progress bar

   DBI::dbExecute(con, "PRAGMA disable_progress_bar;")

  chunk_info <- get_chunk_indices(
    input_file = bold.search.res,
    chunksize = chunk.size
  )

  total_rows <- chunk_info$total_rows
  chunk_size <- chunk_info$chunk_size
  chunk_indices <- chunk_info$chunk_indices
  total_chunks <- length(chunk_indices)

  # Enable the progress bar
  DBI::dbExecute(con, "PRAGMA enable_progress_bar;")

  if (total_chunks == 1) {
    message(sprintf("Collecting all %d rows in a single chunk...", total_rows))
    res <- bold.search.res %>% dplyr::collect()
  } else {
    tbl_sql <- dbplyr::sql_render(bold.search.res)

    res_chunks <- progressr::with_progress({
      p <- progressr::progressor(steps = total_chunks)

      lapply(chunk_indices, function(i) {
        offset <- (i - 1) * chunk_size
        size <- min(chunk_size, total_rows - offset)

        p(sprintf("Chunk %d/%d (%d rows)", i, total_chunks, size))

        sql_query <- paste0(
          "SELECT * FROM (",
          tbl_sql,
          ") AS sub_tbl ",
          "LIMIT ",
          size,
          " OFFSET ",
          offset
        )

        out <- DBI::dbGetQuery(con, sql_query)

        gc()

        if (sys.sleep > 0) {
          Sys.sleep(sys.sleep)
        }

        out
      })
    })

    # Combine all the chunks
    res <- dplyr::bind_rows(res_chunks)
  }

  # Converting data format to match API-fetched data (from BOLDconnectR)

  # res <- res %>%
  #   dplyr::mutate(coord = gsub("\\[|\\]|\\s", "", coord),
  #                 bold_recordset_code_arr = gsub("\\[|\\]|\\'|\\s", "", bold_recordset_code_arr))
  #   #dplyr::rename(all_of(c(country.ocean = "country/ocean", province.state = "province/state")))

  # Exporting as a TSV

  if (export && export.type == "tsv") {
    if (is.null(output.path)) {
      stop("Please provide output.path for TSV export")
    }

    utils::write.table(
      res,
      file = output.path,
      sep = "\t",
      row.names = FALSE,
      quote = FALSE
    )

    message("TSV export complete.")
  }

  return(invisible(res))
}
