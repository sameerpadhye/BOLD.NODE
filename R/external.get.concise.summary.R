#' Generate a concise summary of bold.data.search results
#'
#' @description Creates a summary statistics table from the bold.data.search tb_sql object that includes counts of records, sequences, species, BINs, countries, institutes, identifiers, depositories, and amplicon length ranges.
#'
#' @details The function provides a concise summary of the search obtained by `bold.data.search` that includes details like total records, unique BINs, unique institutes, unique markers and amplicon size range.
#'
#' @param bold.search.res A tbl_sql object containing bold.data.search results
#'
#' @return A data frame with the summary statistics
#'
#' @importFrom dplyr summarise n n_distinct case_when pull mutate across select collect %>% everything
#' @importFrom tidyr pivot_longer
#' @importFrom DBI dbExecute
#' @importFrom dbplyr remote_con
#'
#' @examples
#' \dontrun{
#'
#' # Search data
#' bold_search <- bold.data.search(
#' input.parquet=parquet_file,
#' taxonomy = "Hemiptera",
#' geography = "India",
#' marker = "COI-5P"
#' )
#' # Get the concise summary
#' bold_summary <- get.concise.summary(bold_search)
#'
#' }
#' @export

get.concise.summary<-function(bold.search.res)

  {

  check.tbl.sql(bold.search.res)

  bold.search.res.cols=bold.search.res%>%colnames()

  bold_field_data = bold.bcdm.fields(print.output = F)%>%
    dplyr::select(field)

  if (!all(bold_field_data$field %in% bold.search.res.cols)) {
    stop("Error: Concise summary requires all BCDM fields. Please re-check the search criteria.")
  }

  con <- dbplyr::remote_con(bold.search.res)

  concise_summary <- bold.search.res %>%
    summarise(
      Total_records = n(),

      Total_records_w_sequences = sum(!is.na(nuc)),

      Unique_species = n_distinct(species, na.rm = TRUE),

      Unique_species_w_BINs = n_distinct(
        case_when(!is.na(bin_uri) ~ species),
        na.rm = TRUE
      ),

      Unique_BINs = n_distinct(bin_uri, na.rm = TRUE),

      Unique_countries = n_distinct(country.ocean, na.rm = TRUE),

      Unique_institutes = n_distinct(inst, na.rm = TRUE),

      Unique_identified_by = n_distinct(identified_by, na.rm = TRUE),

      Unique_specimen_depositories = n_distinct(sequence_run_site, na.rm = TRUE),

      min_amplicon = min(nuc_basecount, na.rm = TRUE),

      max_amplicon = max(nuc_basecount, na.rm = TRUE)
    ) %>%
    collect()

  DBI::dbExecute(con, "PRAGMA disable_progress_bar;")
  concise_summary = concise_summary %>%
    mutate(
      Unique_markers = paste(unique(bold.search.res %>%
                                      distinct(marker_code) %>%
                                      collect() %>%
                                      pull()), collapse = ","),
      Amplicon_length_range = paste(min_amplicon, "-", max_amplicon),
      across(everything(), as.character)) %>%
    select(-min_amplicon, -max_amplicon) %>%
    tidyr::pivot_longer(
      everything(),
      names_to = "Category",
      values_to = "Value"
    )
  DBI::dbExecute(con, "PRAGMA enable_progress_bar;")

  return(concise_summary)

}





