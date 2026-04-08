#' Convert BOLD search results to spatial points
#'
#' @description Converts BOLD search results with coordinate data to spatial points (sf object).
#'
#' @details This function transforms the search results from `bold.data.search` into an `sf` object. Data chunking  option is available to manage large sizes to avoid memory issues. The function creates point geometries in the WGS84 coordinate system (EPSG:4326). Records that dont have coordinate data are removed during processing.
#'
#' @param bold.search.res A tbl_sql object containing BOLD search results
#' @param chunk.size Number of records to process in each chunk (default: 10000)
#'
#' @return An sf object with point geometry in WGS84 coordinate system (EPSG:4326)
#'
#' @importFrom dplyr filter mutate select collect
#' @importFrom sf st_as_sf
#' @importFrom dbplyr sql
#'
#' @examples
#' \dontrun{
#'
#'
#' # Search the BOLD data
#' bold_search <- bold.data.search(
#' parquet_path=parquet_file,
#' taxonomy = "Odonata",
#' geography = "Malaysia")
#'
#' # Get the occurrence matrix
#'
#' occurrence_data <- get.sf(bold_search, chunk.size = 100000)
#'
#' }
#'
#' @export
get.sf <- function(bold.search.res, chunk.size = 100000) {

  check.tbl.sql(bold.search.res)

  indices <- get_chunk_indices(input_file = bold.search.res,
                               chunksize = chunk.size)

  # Optional sql query


  # coord_clean = regexp_replace(coord, '\\[|\\]|\\s', ''),
  # lat = sql("split_part(coord_clean, ',', 1)"),
  # # Extract longitude = second part
  # lon = sql("replace(split_part(coord_clean, ',', 2), ']', '')"),
  # row_num = sql("row_number() over (order by coord)")
  #
  geo_data <- bold.search.res %>%
    dplyr::filter(!is.na(coord)) %>%
    dplyr::mutate(coord_clean = sql("replace(replace(trim(coord), '[', ''), ']', '')"),
                    # instr here gives the position of the substring. Here it checks that the string contains a comma before splitting and casting as a double
                    lat = sql("CASE WHEN instr(replace(replace(trim(coord), '[', ''), ']', ''), ',') > 0
          THEN CAST(split_part(replace(replace(trim(coord), '[', ''), ']', ''), ',', 1) AS DOUBLE)
          ELSE NULL END"),
                    lon = sql("CASE WHEN instr(replace(replace(trim(coord), '[', ''), ']', ''), ',') > 0
          THEN CAST(split_part(replace(replace(trim(coord), '[', ''), ']', ''), ',', 2) AS DOUBLE)
          ELSE NULL END"),
                    row_num = sql("row_number() over (order by coord)"))

  sf_data_list <- lapply(indices$chunk_indices, function(i) {
    start <- (i - 1) * indices$chunk_size + 1
    end   <- min(i * indices$chunk_size, indices$total_rows)

    geo_data %>%
      dplyr::filter(row_num >= start & row_num <= end) %>%
      dplyr::select(-row_num, coord_clean) %>%
      filter(!is.na(coord_clean))%>%
      dplyr::collect() %>%
      dplyr::filter(!is.na(lat)|!is.na(lon))%>%
      st_as_sf(coords = c("lon","lat"), crs = 4326, remove = FALSE)
  })

  sf_data <- dplyr::bind_rows(sf_data_list)

  return(sf_data)
}
