#' Convert bold.data.search results in to Darwin Core (DwC) format
#'
#' @description Converts bold.data.search results from BCDM format to Darwin Core Standard format.
#'
#' @details This function maps BCDM (Barcode Core Data Model) fields to their Darwin Core equivalents using the official mapping file from the BCDM repository.
#'
#' @param bold.search.res A tbl_sql object containing BOLD search results
#'
#' @return A data frame with columns renamed to Darwin Core equivalent fields
#'
#' @importFrom dplyr select mutate case_when filter rename collect any_of
#' @importFrom data.table fread
#' @importFrom dbplyr sql
#'
#' @examples
#' \dontrun{
#'
#' # Search the BOLD data
#' bold_search <- bold.data.search(
#' parquet_path=parquet_file,
#' taxonomy = c("Cerambycidae","Meloidae"),
#' geography = "Australia"
#' )
#'
#' # Get the DNAStringset object
#'
#' bold.dwc<-get.DNAStringSet(bold_search)
#'
#' }
#' @export

get.dwc<-function(bold.search.res)


  {

  check.tbl.sql(bold.search.res)

  # BOLD to BCDM mapping with certain updates for some field name mappings

  bold.2.bcdm <- suppressMessages(data.table::fread("https://raw.githubusercontent.com/boldsystems-central/BCDM/refs/heads/main/mapping_BCDM_to_DWC.tsv",
                                                    sep = '\t',
                                                    quote = "",
                                                    check.names = FALSE,
                                                    verbose = FALSE,
                                                    showProgress = FALSE,
                                                    data.table = FALSE,
                                                    fill=TRUE,
                                                    tmpdir = tempdir()))%>%
    select(bcdm_field,
           dwc_field,
           dwc_type)%>%
    # change the country and province names for ease of use ('/' against '.')
    dplyr::mutate(bcdm_field=dplyr::case_when(bcdm_field=="country/ocean"~"country.ocean",
                                              bcdm_field=="province/state"~"province.state",
                                              TRUE~bcdm_field))


  # Only the dwc fields are retained

  bold.2.bcdm <- bold.2.bcdm%>%
    dplyr::mutate(dwc_field=dplyr::case_when(bcdm_field=="processid"~"recordNumber",
                                             bcdm_field=="sampleid"~"materialEntityID",
                                             bcdm_field=="taxid"~"taxonID",
                                             bcdm_field=="species"~"acceptedNameUsage",
                                             bcdm_field=="identification"~"verbatimIdentification",
                                             bcdm_field=="identification_rank"~"verbatimTaxonRank",
                                             bcdm_field=="voucher type"~"disposition",
                                             bcdm_field=="tissue type"~"materialEntityType",
                                             bcdm_field=="specimen_linkout"~"associatedMedia",
                                             bcdm_field=="collection_notes"~"eventRemarks",
                                             bcdm_field=="geoid"~" locationId",
                                             bcdm_field=="site"~"locality",
                                             bcdm_field=="site_code"~"locationID",
                                             bcdm_field=="nuc"~"measurementValue",
                                             bcdm_field=="insdc_acs"~"materialSampleID",
                                             bcdm_field=="funding_src"~"fundingAttribution",
                                             bcdm_field=="marker_code"~"target_subfragment",
                                             bcdm_field=="primers_forward"~"pcr_primer_forward",
                                             bcdm_field=="primers_reverse"~"pcr_primer_reverse",
                                             bcdm_field=="sovereign_list"~"rightsHolder",
                                             bcdm_field=='collection_date_end'~'eventDate2',
                                             TRUE~dwc_field))%>%
    mutate(bcdm_field=case_when(bcdm_field=='recordset_code_arr'~'bold_recordset_code_arr',
                                TRUE~bcdm_field))%>%
    dplyr::filter(dwc_field!='')


  dwc_output<-bold.search.res%>%
    select(any_of(bold.2.bcdm$bcdm_field))%>%
    mutate(coord_clean = regexp_replace(coord, '\\[|\\]|\\s', ''),
           decimalLatitude = sql("split_part(coord_clean, ',', 1)"),
           # Extract longitude = second part
           decimalLongitude = sql("replace(split_part(coord_clean, ',', 2), ']', '')"))%>%
    select(-c(coord))%>%
    collect()%>%
    select(-coord_clean)

  rename_vec <- setNames(bold.2.bcdm$bcdm_field,bold.2.bcdm$dwc_field)

  # Remove the coord name from the rename vector as lat and lon are already created above
  coord_col<-which(rename_vec=='coord')

  rename_vec<-rename_vec[-c(coord_col[[1]])]

  # Apply the column name changes to the data

  dwc_output<-dwc_output%>%
    rename(!!!rename_vec)

  return(dwc_output)


  #
  # add_fields<-data.frame(bcdm_field=c("biome","ecoregion","realm"),
  #                        dwc_field=rep('occurrenceRemarks',3),
  #                        dwc_type=rep("string",3))
  #
  # bold.2.bcdm <- bind_rows(bold.2.bcdm,add_fields)





  # occurrenceRemarks=paste0('ecoregion: ',ecoregion,'|','biome: ',biome,'|','realm: ',realm)

  #    rename(!!!rename_vec)%>%
  # collect()

  #select(-c(coord,coord_clean,biome,ecoregion,realm))%>%



  # occ_remark_cols<-which(names(rename_vec)=='occurrenceRemarks')
  #
  # occ_remark_cols

  # 2,3,4 here represent the columns for ecoregions.biome and realm from bcdm. The same name is used for all three and hence this has to be removed as column names need to be unique

  # rename_vec<-rename_vec[-c(occ_remark_cols)]


}









